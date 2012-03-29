from __future__ import division

from fractions import Fraction

import pymeta.grammar
import pymeta.runtime

from lye.algos import pitch_to_number
from lye.ast import (TIE, Chord, Drum, Drums, Duration, Music, Note, Relative,
                     Rest, Times)
from lye.drums import drum_notes

class InternalParseError(Exception):
    """
    An impossible condition happened inside the parser.

    This is probably an implementation error.
    """

class LyeError(Exception):
    """
    Something happened when parsing, and it's your fault.
    """

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

relative ::= <token "\\\\relative">
times ::= <token "\\\\times">

sharp ::= 'i' 's' => 1
flat ::= 'e' 's' => -1
accidental ::= (<sharp> | <flat>)+:a => sum(a)

octave_up ::= '\'' => 1
octave_down ::= ',' => -1
octave ::= (<octave_up> | <octave_down>)+:o => sum(o)

duration ::= <int>:i '.'*:dots => Duration(i, len(dots))

kit ::= <token "bd"> | <token "hhc"> | <token "sn">

es ::= 'e' 's' => "es"
pitch ::= 'c' | 'd' | <es> | 'e' | 'f' | 'g' | 'a' | 'b'

voices ::= <token "<<"> <scope>+:ss <token ">>"> => ss + [ENDVOICE]

expr_chord    ::= <token "<"> <expr_note>+:ns <token ">"> => Chord(ns)
expr_drum     ::= <kit> <duration>?:d => Drum(drum_notes[k], d)
expr_drums    ::= <token "\\\\drums"> <expr>:e => Drums(e)
expr_music    ::= <token "{"> <expr>+:e <token "}"> => Music(e)
expr_note     ::= <pitch>:p <accidental>?:a <octave>?:o <duration>?:d
                => Note(p, a, o, d)
expr_relative ::= <token "\\\\relative"> <spaces> <pitch>:p <accidental>?
                  <octave>?:o <expr_music>:e
                => Relative(p, o, e)
expr_rest     ::= <token "r"> <duration>?:d => Rest(d)
expr_tie      ::= <token "~"> => TIE
expr_times    ::= <token "\\\\times"> <spaces> <int>:n '/' <int>:d
                  <expr_music>:e
                => Times(Fraction(n, d), e)
expr_voices   ::= <token "<<"> <expr_music>+:es <token ">>"> => Voices(es)
expr ::= <spaces>? (<expr_chord> | <expr_drum> | <expr_drums> | <expr_music> |
         <expr_note> | <expr_relative> | <expr_rest> | <expr_tie> |
         <expr_times>)
"""

class LyeGrammar(pymeta.grammar.OMeta.makeGrammar(grammar, globals())):
    """
    Simple grammar for Lye, a relatively strict subset of Lilypond useful for
    declaring small snippets of music.

    To use this grammar, instantiate it with text to parse, and then call its
    ast() method.
    """

    def __init__(self, data):
        super(LyeGrammar, self).__init__(data)
        self._data = data

    def ast(self):
        try:
            return self.apply("expr")[0]
        except ParseError, pe:
            raise LyeError("Couldn't parse: %s" % pe.formatError(self._data))

class Visitor(object):
    """
    An object that visits every node in an AST.
    """

    def visit_generic(self, node):
        """
        Visit a node with no type information.

        This method is a good point for polymorphic or duck-typed
        transformations.
        """

        return node

    def visit(self, node):
        """
        Recursively visit every node in an AST.
        """

        method = getattr(self, "visit_%s" % node.__class__.__name__,
            self.visit_generic)
        node = method(node)

        if "expr" in node._fields:
            node = node._replace(expr=self.visit(node.expr))

        if "exprs" in node._fields:
            exprs = [self.visit(expr) for expr in node.exprs]
            node = node._replace(exprs=exprs)

        return node

class DurationWalker(Visitor):
    """
    Tool for filling out durations in a Lye AST.
    """

    tpb = 120
    duration = Duration(4, 0)

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

    def fill_duration(self, d):
        """
        Fill durations forward.
        """

        if d is not None:
            self.duration = d
        return self.duration

    def visit_generic(self, ast):
        """
        Simplify and fill in durations along an AST.
        """

        if "duration" in ast._fields:
            duration = self.fill_duration(ast.duration)
            duration = self.undot_duration(duration.length, duration.dots)
            ast = ast._replace(duration=duration)

        return ast

class Remainder:
    """
    Class providing lexing and parsing of pseudo-Lilypond streams into data
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
        super(LyeGrammar, self).__init__(*args, **kwargs)

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

    chords = LyeGrammar(s).apply("chords")
    if not chords:
        raise LyeError("Failed chords %s" % s)
    return chords
