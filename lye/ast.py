from collections import namedtuple

class Named(object):
    """
    Named for convenience, compare me by identity.
    """

    def __init__(self, name):
        self.name = name

    def __str__(self):
        return "<%s>" % self.name

    __repr__ = __str__

CLOSE_SLUR = Named("Close Slur")
ENDVOICE = Named("End Voice")
MEASURE = Named("Measure")
OPEN_SLUR = Named("Open Slur")
PARTIAL = Named("Partial")
TIE = Named("Tie")

Chord = namedtuple("Chord", "notes")
Drums = namedtuple("Drums", "expr")
Duration = namedtuple("Duration", "length, dots")
Dynamic = namedtuple("Dynamic", "mark")
Music = namedtuple("Music", "exprs")
Note = namedtuple("Note", "pitch, accidental, octave, duration")
PitchBend = namedtuple("PitchBend", "offset, value")
Relative = namedtuple("Relative", "pitch, octave, expr")
Rest = namedtuple("Rest", "duration")
SciNote = namedtuple("SciNote", "pitch, duration, velocity")
Slur = namedtuple("Slur", "exprs")
Times = namedtuple("Times", "fraction, expr")
Voice = namedtuple("Voice", "exprs")
Voices = namedtuple("Voices", "exprs")
