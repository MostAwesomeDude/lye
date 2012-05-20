from __future__ import division

from fractions import Fraction

from pymeta.grammar import OMeta
from pymeta.runtime import ParseError

from lye.ast import (MEASURE, TIE, Chord, Drums, Duration, Music, Note,
                     Relative, Rest, SciNote, Times, Voices)
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

def concat(l):
    rv = []
    for i in l:
        rv.extend(i)
    return rv

kit_rule = """
kit ::= %s
""" % (" | ".join("<token \"%s\">" % note for note in drum_notes))

grammar = kit_rule + """
int ::= <digit>+:d => int("".join(d))

sharp ::= 'i' 's' => 1
flat ::= 'e' 's' => -1
accidental ::= (<sharp> | <flat>)+:a => sum(a)

octave_up ::= '\'' => 1
octave_down ::= ',' => -1
octave ::= (<octave_up> | <octave_down>)+:o => sum(o)

duration ::= <int>:i '.'*:dots => Duration(i, len(dots))

pitch ::= <token "c"> | <token "d"> | <token "es"> | <token "e"> |
          <token "f"> | <token "g"> | <token "a"> | <token "b">

expr_chord    ::= <token "<"> <expr_note>+:ns <token ">"> => Chord(ns)
expr_drum     ::= <kit>:k <duration>?:d => SciNote(drum_notes[k], d)
expr_drums    ::= <token "\\\\drums"> <expr>:e => Drums(e)
expr_measure  ::= <token "|"> => MEASURE
expr_music    ::= <token "{"> <expr>+:e <token "}"> => Music(e)
expr_note     ::= <pitch>:p <accidental>?:a <octave>?:o <duration>?:d
                => Note(p, a or 0, o or 0, d)
expr_relative ::= <token "\\\\relative"> <spaces> <pitch>:p <accidental>?
                  <octave>?:o <expr_music>:e
                => Relative(p, o or 0, e)
expr_rest     ::= <token "r"> <duration>?:d => Rest(d)
expr_tie      ::= <token "~"> => TIE
expr_times    ::= <token "\\\\times"> <spaces> <int>:n '/' <int>:d
                  <expr_music>:e
                => Times(Fraction(n, d), e)
expr_voices   ::= <token "<<"> <expr_music>+:es <token ">>"> => Voices(es)
expr ::= <expr_chord> | <expr_drum> | <expr_drums> | <expr_measure> |
         <expr_music> | <expr_note> | <expr_relative> | <expr_rest> |
         <expr_tie> | <expr_times> | <expr_voices>
"""

class LyeGrammar(OMeta.makeGrammar(grammar, globals())):
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
