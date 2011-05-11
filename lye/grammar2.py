import logging

import pymeta.grammar
import pymeta.runtime

log = logging.getLogger("salsa.ly")

class Note(object):
    """
    The fundamental unit of composition.
    """

    def __init__(self, pitch, duration):
        self.pitch = pitch
        self.duration = duration

    def __repr__(self):
        return "Note(%d, %d)" % (self.pitch, self.duration)

    __str__ = __repr__

class Chord(Note):
    """
    A Note list.
    """

    def __init__(self, notes):
        self.pitches = [note.pitch for note in notes]
        self.duration = notes[0].duration

    def __repr__(self):
        return "Chord(%r, %d)" % (self.pitches, self.duration)

    __str__ = __repr__

class Marker(object):
    """
    A singleton representing a measure marker.
    """

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "Marker(%r)" % self.name

    __str__ = __repr__

# es requires a special case, because it can either be spelled es or ees.
pitch_dict = dict(zip("cxdxefxgxaxb", range(48, 60)))
pitch_dict["es"] = 51
del pitch_dict["x"]

relative_dict = dict(zip("cdefgab", range(7)))
relative_dict["es"] = relative_dict["e"]

grammar = """
relative ::= <token "\\\\relative">

begin_relative ::= <relative> <spaces>
                   <pitch>:p <accidental>? <octave>?:o <spaces> '{'
                 => self.open_brace("relative", (p, o if o else ""))

close_brace ::= '}' => self.close_brace()

directive ::= <begin_relative> | <close_brace>

sharp ::= 'i' 's' => "is"
flat ::= 'e' 's' => "es"
accidental ::= (<sharp> | <flat>)+:a => "".join(a)

pitch ::= 'r' | 'c' | 'd' | <flat> | 'e' | 'f' | 'g' | 'a' | 'b'

octave ::= ('\'' | ',')+:o => "".join(o)

duration ::= <digit>+:d '.'*:dots
           => self.undot_duration(int("".join(d)), len(dots))

note ::= <spaces>? <pitch>:p <accidental>?:a <octave>?:o <duration>?:d
       => Note(self.abs_pitch_to_number(p, a, o), self.check_duration(d))

notes ::= <note>*:ns => ns

chord ::= <token '<'> <notes>:ns <token '>'> => Chord(ns)

measure_marker ::= <token '|'> => Marker("measure")

partial_marker ::= <token "\\\\partial"> => Marker("partial")

marker ::= <measure_marker> | <partial_marker>

protonote ::= <marker> | <chord> | <note>

protonote_cluster ::= <spaces>? <protonote>:pn (<spaces>? <protonote>)*:pns
                    => [pn] + pns

melody ::= <directive>? <protonote_cluster>:m <directive>? => m
"""

class LyGrammar(pymeta.grammar.OMeta.makeGrammar(grammar, globals())):
    """
    Class providing parsing and lexing of pseudo-Lilypond streams into data
    structures that can be passed to other high-level libraries.

    Like with standard Lilypond, the default octave starts at C3 (48).
    """

    ticks_per_beat = 120
    """
    Number of ticks per beat.
    """

    def __init__(self, *args, **kwargs):
        super(LyGrammar, self).__init__(*args, **kwargs)

        self.duration = self.ticks_per_beat

        self.brace_stack = []
        self.relative = None

    def open_brace(self, name, arg=None):
        self.brace_stack.append(
            lambda self: setattr(self, name, None))
        setattr(self, name, arg)

    def close_brace(self):
        self.brace_stack.pop()(self)

    def undot_duration(self, duration, dots):
        """
        Turn duration into a number of ticks, and apply dots, if any.
        """

        duration = self.ticks_per_beat * 4 / duration
        while dots:
            dots -= 1
            duration = duration * 3 / 2
        return duration

    def abs_pitch_to_number(self, pitch, accidental, octave):
        """
        Convert an absolute pitch to its MIDI/scientific number.
        """

        if pitch == "r":
            # Rests are forced to -1
            return -1
        accidental = accidental if accidental else ""
        octave = octave if octave else ""

        if self.relative:
            rel_pitch, rel_octave = self.relative
            octave += rel_octave
            if abs(relative_dict[rel_pitch] - relative_dict[pitch]) > 3:
                if relative_dict[pitch] > 4:
                    octave += ","
                else:
                    octave += "'"
            self.relative = pitch, octave

        n = pitch_dict[pitch]

        while accidental:
            if accidental.startswith("es"):
                accidental = accidental[2:]
                n -= 1
            elif accidental.startswith("is"):
                accidental = accidental[2:]
                n += 1
            else:
                log.error("Unknown symbol %s while lexing accidental"
                    % accidental)
                break

        for c in octave:
            if c == ",":
                n -= 12
            elif c == "'":
                n += 12
            else:
                log.error("Unknown symbol %s while lexing octave" % c)
                break

        return n

    def check_duration(self, d):
        if d:
            self.duration = d
        return self.duration

def chords_from_ly(s):
    """
    Make a `Chords` from a ly string.
    """

    chords = LyGrammar(s).apply("chords")
    if not chords:
        log.error("Failed chords %s" % s)
    return chords
