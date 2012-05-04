from __future__ import division

from fluidsynth import fluidsynth

from lye.algos import schedule_notes
from lye.grammar import LyeGrammar
from lye.instruments import NEAREST, fit
from lye.visitor import ChordCounter, HarmonySplitter, Multiply, simplify_ast

class Melody(object):

    instrument = None
    volume = 127
    pan = 63

    fit_method = NEAREST

    _len = None
    _scheduled = None

    def __init__(self, music, tpb):
        self.music = music
        self.tpb = tpb
        self._scheduled, self._len = schedule_notes(self.music, self.tpb)

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

    def fit(self):
        """
        Force this melody to be within the range of its instrument.
        """

        if self.instrument:
            self.music = fit(self.music, self.instrument, self.fit_method)
        # And reschedule.
        self._scheduled, self._len = schedule_notes(self.music, self.tpb)

    def split(self):
        """
        Split this melody into harmonies.

        The melody should be harmonized somewhat already.

        Attempts are made to put as much work into the top of the melody as
        possible.

        Returned melodies are high-to-low.
        """

        counter = ChordCounter()
        counter.visit(self.music)
        voices = counter.length

        rv = []
        for voice in range(voices):
            melody = Melody(HarmonySplitter(voice).visit(self.music),
                    self.tpb)
            melody.pan = self.pan
            melody.volume = self.volume
            rv.append(melody)
        return rv

    def to_fs(self, sequencer):
        """
        Sends the melody to `sequencer`.
        """

        # XXX
        # tpb = sequencer.ticks_per_beat
        # Each item in the seq is (fluidsynth.FS, (dest, destname))
        # We just want the dest
        dest = sequencer.items()[0][1][0]
        # XXX this fudge value might not be needed?
        ticks = sequencer.ticks + 10

        for pitch, begin, duration in self.scheduled:
            event = fluidsynth.FluidEvent()
            event.dest = dest
            # XXX ? pitch vel duration
            event.note(0, pitch, self.volume, duration)
            sequencer.send(event, ticks + begin)

    def to_midi(self, f, channel):
        """
        Create a MIDI expression for this melody.
        """

        track = 0

        for pitch, begin, duration in self.scheduled:
            begin = begin / self.tpb
            duration = duration / self.tpb
            f.addNote(track, channel, pitch, begin, duration, self.volume)

def melody_from_ly(s):
    """
    Make a `Melody` from a ly string.
    """

    g = LyeGrammar(s)
    ast = g.ast()
    ast = simplify_ast(ast)
    melody = Melody(ast, 120)
    return melody
