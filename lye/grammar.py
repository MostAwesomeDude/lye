from __future__ import division

from fractions import Fraction

import pymeta.grammar
import pymeta.runtime

from lye.algos import pitch_to_number
from lye.drums import drum_notes
from lye.types import ENDVOICE, MEASURE, PARTIAL, TIE, Note, Rest

class InternalParseError(Exception):
    """
    An impossible condition happened inside the parser.

    This is probably an implementation error.
    """

class LyeError(Exception):
    """
    Something happened when parsing, and it's your fault.
    """

class Chord(object):
    """
    A Note list.
    """

    def __init__(self, notes):
        self.pitches = [note.pitch for note in notes]
        self.duration = notes[0].duration

    def __repr__(self):
        return "Chord(%r, %d)" % (self.pitches, self.duration)

    __str__ = __repr__

    def __eq__(self, other):
        return (self.pitches == getattr(other, "pitches", None) and
            self.duration == getattr(other, "duration", None))

# es requires a special case, because it can either be spelled es or ees.
pitch_dict = dict(zip("cxdxefxgxaxb", range(48, 60)))
pitch_dict["es"] = 51
del pitch_dict["x"]

relative_dict = dict(zip("cdefgab", range(7)))
relative_dict["es"] = relative_dict["e"]

def concat(l):
    rv = []
    for i in l:
        rv.extend(i)
    return rv

grammar = """
int ::= <digit>+:d => int("".join(d))

drums ::= <token "\\\\drums">
relative ::= <token "\\\\relative">
times ::= <token "\\\\times">

sharp ::= 'i' 's' => 1
flat ::= 'e' 's' => -1
accidental ::= (<sharp> | <flat>)+:a => sum(a)

octave_up ::= '\'' => 1
octave_down ::= ',' => -1
octave ::= (<octave_up> | <octave_down>)+:o => sum(o)

expr_notes ::= <token "{"> <expr>+:e <token "}"> => e

begin_drums ::= <drums> <token "{"> => self.open_brace("drums", True)

begin_relative ::= <relative> <spaces>
                   <pitch>:p <accidental>? <octave>?:o <token "{">
                 => self.start_relative((p, o if o else 0))

begin_times ::= <times> <spaces> <int>:n '/' <int>:d <token "{">
              => self.open_brace("tuplet", Fraction(n, d))

directive ::= <begin_drums> | <begin_relative> | <begin_times>

close_brace ::= <token "}"> => self.close_brace()

rest ::= <token "r"> <duration>?:d => Rest(None, self.check_duration(d))

kit ::= <token "bd"> | <token "hhc"> | <token "sn">

drum ::= ?( self.drums ) <kit>:k <duration>?:d
       => Note(drum_notes[k], None, self.check_duration(d))

es ::= 'e' 's' => "es"
pitch ::= 'c' | 'd' | <es> | 'e' | 'f' | 'g' | 'a' | 'b'

duration ::= <int>:i '.'*:dots => self.undot_duration(i, len(dots))

note ::= ?( not self.drums ) <spaces>? <pitch>:p <accidental>?:a <octave>?:o
         <duration>?:d
       => Note(self.abs_pitch_to_number(p, a, o), None,
               self.check_duration(d))

notes ::= <note> | <drum> | <rest>

chord ::= <token '<'> <notes>:n !( self.start_chord() ) <notes>*:ns
          <token '>'> !( self.end_chord() )
        => Chord([n] + ns)

measure_marker ::= <token '|'> => MEASURE

partial_marker ::= <token "\\\\partial"> => PARTIAL

tie_marker ::= <token "~"> => TIE

marker ::= <measure_marker> | <partial_marker> | <tie_marker>

protonote ::= <spaces>? (<marker> | <chord> | <notes>)

scope ::= <token '{'> <melody>:m <token '}'> => m
voices ::= <token "<<"> <scope>+:ss <token ">>"> => ss + [ENDVOICE]

melody ::= <directive> <melody>+:m <close_brace> => concat(m)
         | <voices>
         | <protonote>+
"""

class LyGrammar(pymeta.grammar.OMeta.makeGrammar(grammar, globals())):
    """
    Class providing parsing and lexing of pseudo-Lilypond streams into data
    structures that can be passed to other high-level libraries.

    Like with standard Lilypond, the default octave starts at C3 (48).
    """

    # Number of ticks per beat.
    tpb = 120

    # The previous pitch parsed. Defaults to middle C.
    last_pitch = ("c", 0)

    # Whether we're in relative mode.
    relative = False

    # Tuplet timing factor, or None if durations are not tupled.
    tuplet = None

    # Whether we are in drums mode.
    drums = None

    def __init__(self, *args, **kwargs):
        super(LyGrammar, self).__init__(*args, **kwargs)

        self.duration = self.tpb

        self.brace_stack = []

    def open_brace(self, name, arg=None):
        self.brace_stack.append(
            lambda self: setattr(self, name, None))
        setattr(self, name, arg)

    def close_brace(self):
        self.brace_stack.pop()(self)

    def start_relative(self, pitch):
        self.last_pitch = pitch
        self.relative = True
        self.brace_stack.append(
            lambda self: setattr(self, "relative", False))

    def start_chord(self):
        self._saved_last_pitch = self.last_pitch

    def end_chord(self):
        self.last_pitch = self._saved_last_pitch

    def undot_duration(self, duration, dots):
        """
        Turn duration into a number of ticks, and apply dots, if any.
        """

        # Multiply ticks per beat by four since ly assumes that "1" is a whole
        # note, not a quarter note, but beats are quarter notes.
        duration = self.tpb * 4 // duration

        dotted = duration
        while dots:
            dotted /= 2
            duration += dotted
            dots -= 1
        return duration

    def abs_pitch_to_number(self, pitch, accidental, octave):
        """
        Convert an absolute pitch to its MIDI/scientific number.
        """

        accidental = accidental or 0
        octave = octave or 0

        if self.relative:
            rel_pitch, rel_octave = self.last_pitch
            octave += rel_octave
            if abs(relative_dict[rel_pitch] - relative_dict[pitch]) > 3:
                if relative_dict[pitch] > 4:
                    octave -= 1
                else:
                    octave += 1

        self.last_pitch = pitch, octave

        return pitch_to_number(pitch, accidental, octave)

    def check_duration(self, d):
        if d:
            self.duration = d
        if self.tuplet:
            return int(self.tuplet * self.duration)
        else:
            return self.duration

def chords_from_ly(s):
    """
    Make a `Chords` from a ly string.
    """

    chords = LyGrammar(s).apply("chords")
    if not chords:
        raise LyeError("Failed chords %s" % s)
    return chords
