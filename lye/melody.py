from __future__ import division

from lye.grammar import LyeGrammar
from lye.instruments import NEAREST, fit
from lye.visitors import express_ast, simplify_ast
from lye.visitors.folds import ChordCounter, NoteScheduler, fold
from lye.visitors.maps import HarmonySplitter, Multiply

class Melody(object):

    instrument = None
    volume = 100
    pan = 63

    fit_method = NEAREST

    _len = None
    _scheduled = None

    def __init__(self, music, tpb, instrument=None, data=None):
        self.music = music
        self.tpb = tpb
        self.data = data

        self.change_instrument(instrument)

    def __nonzero__(self):
        return any(self.music)

    def __repr__(self):
        return "Melody(%d, %d, %r)" % (self.tpb, len(self), self.scheduled)

    __str__ = __repr__

    def __len__(self):
        return self._len

    def __mul__(self, other):
        music = Multiply(other).visit(self.music)
        return Melody(music, self.tpb)

    @property
    def scheduled(self):
        return self._scheduled

    def change_instrument(self, instrument):
        """
        Set the instrument for this melody.

        Forces the melody to be within the range of the instrument, and
        applies expressions to the melody.
        """

        self.instrument = instrument
        music = self.music

        if instrument:
            music = fit(music, self.instrument, self.fit_method)
            music = express_ast(music, self.instrument)

        # And finally reschedule.
        self._scheduled, self._len = fold(NoteScheduler, music)

    def split(self):
        """
        Split this melody into harmonies.

        The melody should be harmonized somewhat already.

        Attempts are made to put as much work into the top of the melody as
        possible.

        Returned melodies are high-to-low.
        """

        voices = fold(ChordCounter, self.music)

        rv = []
        for voice in range(voices):
            melody = Melody(HarmonySplitter(voice).visit(self.music),
                    self.tpb)
            melody.pan = self.pan
            melody.volume = self.volume
            rv.append(melody)
        return rv

def melody_from_ly(s):
    """
    Make a `Melody` from a ly string.
    """

    g = LyeGrammar(s)
    ast = g.expr()
    ast = simplify_ast(ast)
    melody = Melody(ast, 120, data=s)
    return melody
