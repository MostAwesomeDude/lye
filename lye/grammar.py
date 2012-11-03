from __future__ import division

from fractions import Fraction

from parsley import makeGrammar

from lye.ast import (CLOSE_SLUR, MEASURE, OPEN_SLUR, TIE, Chord, Drums,
                     Duration, Dynamic, Music, Note, Relative, Rest, SciNote,
                     Times, Voices)
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


grammar_globals = {
    "CLOSE_SLUR": CLOSE_SLUR,
    "Chord": Chord,
    "Drums": Drums,
    "Duration": Duration,
    "Dynamic": Dynamic,
    "Fraction": Fraction,
    "MEASURE": MEASURE,
    "Music": Music,
    "Note": Note,
    "OPEN_SLUR": OPEN_SLUR,
    "Relative": Relative,
    "Rest": Rest,
    "SciNote": SciNote,
    "TIE": TIE,
    "Times": Times,
    "Voices": Voices,
    "drum_notes": drum_notes,
}

dynamics = "pp p mp mf f ff".split()

dynamic_rule = """
expr_dynamic = (%s):d -> Dynamic(d[1:])
""" % (" | ".join("token(\"\\\\%s\")" % mark for mark in dynamics))

kit_rule = """
kit = %s
""" % (" | ".join("token(\"%s\")" % note for note in drum_notes))

grammar = dynamic_rule + kit_rule + """
int = <digit+>:d -> int(d)

sharp = 'i' 's' -> 1
flat = 'e' 's' -> -1
accidental = (sharp | flat)+:a -> sum(a)

octave_up = '\'' -> 1
octave_down = ',' -> -1
octave = (octave_up | octave_down)+:o -> sum(o)

duration = int:i '.'*:dots -> Duration(i, len(dots))

pitch = token("c") | token("d") | token("es") | token("e") | token("f")
      | token("g") | token("a") | token("b")

open_slur  = token("(") -> OPEN_SLUR
close_slur = token(")") -> CLOSE_SLUR
measure    = token("|") -> MEASURE
tie        = token("~") -> TIE

expr_chord    = token("<") expr_note+:ns token(">") -> Chord(ns)
expr_drum     = kit:k duration?:d -> SciNote(drum_notes[k], d, None)
expr_drums    = token("\\\\drums") expr:e -> Drums(e)
expr_marker   = open_slur | close_slur | measure | tie
expr_music    = token("{") expr+:e token("}") -> Music(e)
expr_note     = pitch:p accidental?:a octave?:o duration?:d
              -> Note(p, a or 0, o or 0, d)
expr_relative = token("\\\\relative") spaces pitch:p accidental? octave?:o
                expr_music:e
              -> Relative(p, o or 0, e)
expr_rest     = token("r") duration?:d -> Rest(d)
expr_times    = token("\\\\times") spaces int:n '/' int:d expr_music:e
              -> Times(Fraction(n, d), e)
expr_voices   = token("<<") expr_music+:es token(">>") -> Voices(es)
expr = expr_chord | expr_drum | expr_drums | expr_dynamic | expr_marker
     | expr_music | expr_note | expr_relative | expr_rest | expr_times
     | expr_voices

lye = expr:e spaces? end -> e
"""

LyeGrammar = makeGrammar(grammar, grammar_globals)
