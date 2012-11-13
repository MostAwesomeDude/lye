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

def nt(*args):
    """
    Hax namedtuples.
    """

    def f(self, p, cycle):
        name = type(self).__name__,
        if cycle:
            p.text("%s(...)" % name)

        with p.group(1, "%s(" % name, ")"):
            for i, field in enumerate(self._fields):
                val = getattr(self, field)
                if i:
                    p.text(",")
                    p.breakable()
                p.text("%s=" % field)
                p.pretty(val)

    cls = namedtuple(*args)
    cls.__pretty__ = f
    return cls

CLOSE_SLUR = Named("Close Slur")
ENDVOICE = Named("End Voice")
MEASURE = Named("Measure")
OPEN_SLUR = Named("Open Slur")
PARTIAL = Named("Partial")
TIE = Named("Tie")

Chord = nt("Chord", "notes")
Drums = nt("Drums", "expr")
Duration = nt("Duration", "length, dots")
Dynamic = nt("Dynamic", "mark")
Key = nt("Key", "pitch, mode")
Music = nt("Music", "exprs")
Note = nt("Note", "pitch, accidental, octave, duration")
PitchBend = nt("PitchBend", "offset, value")
Relative = nt("Relative", "pitch, octave, expr")
Rest = nt("Rest", "duration")
SciNote = nt("SciNote", "pitch, duration, velocity")
Slur = nt("Slur", "exprs")
Times = nt("Times", "fraction, expr")
Voice = nt("Voice", "exprs")
Voices = nt("Voices", "exprs")
