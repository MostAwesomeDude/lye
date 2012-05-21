from collections import namedtuple

ENDVOICE = object()
MEASURE = object()
PARTIAL = object()
TIE = object()

Chord = namedtuple("Chord", "notes")
Drums = namedtuple("Drums", "expr")
Duration = namedtuple("Duration", "length, dots")
Music = namedtuple("Music", "exprs")
Note = namedtuple("Note", "pitch, accidental, octave, duration")
Relative = namedtuple("Relative", "pitch, octave, expr")
Rest = namedtuple("Rest", "duration")
SciNote = namedtuple("SciNote", "pitch, duration, velocity")
Times = namedtuple("Times", "fraction, expr")
Voice = namedtuple("Voice", "exprs")
Voices = namedtuple("Voices", "exprs")
