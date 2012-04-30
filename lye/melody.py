from __future__ import division

from collections import namedtuple

from fluidsynth import fluidsynth

from lye.ast import MEASURE, PARTIAL, Music, SciNote, Rest, Voice
from lye.grammar import Chord, LyeGrammar
from lye.instruments import NEAREST, fit
from lye.visitor import Multiply, simplify_ast

ScheduledNote = namedtuple("ScheduledNote", "pitch, begin, duration")

class Melody(object):

    instrument = None
    volume = 127
    pan = 63

    _len = None
    _scheduled = None

    def __init__(self, music, tpb):
        self.music = music
        self.tpb = tpb
        self._scheduled, self._len = self.schedule_notes()

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

    def fit(self, strategy=NEAREST):
        """
        Force this melody to be within the range of its instrument.
        """

        if self.instrument:
            fit(self.music, self.instrument, strategy)

    def split(self):
        """
        Split this melody into harmonies.

        The melody should be harmonized somewhat already.

        Attempts are made to put as much work into the top of the melody as
        possible.

        Returned melodies are high-to-low.
        """

        count = 0

        for expr in self.music.exprs:
            if isinstance(expr, Chord):
                count = max(count, len(expr.notes))

        melodies = [Music([]) for i in range(count)]

        for expr in self.music.exprs:
            if isinstance(expr, Chord):
                for i, note in enumerate(sorted(expr.notes, reverse=True)):
                    melodies[i].exprs.append(note)
                for i in range(i + 1, len(melodies)):
                    # Create rests. Rely on the leaked note name from the
                    # previous loop.
                    melodies[i].exprs.append(Rest(note.duration))
            else:
                for melody in melodies:
                    melody.exprs.append(expr)

        rv = []
        for m in melodies:
            melody = Melody(m, self.tpb)
            melody.pan = self.pan
            melody.volume = self.volume
            rv.append(melody)
        return rv

    def schedule_notes(self):
        """
        Attach correct beginning times to notes.

        Additionally, this step discards rests.
        """

        relative_marker = 0
        partial = False
        partial_offset = 0
        scheduled = []

        for i, expr in enumerate(self.music.exprs):
            if expr is MEASURE:
                remainder = ((relative_marker - partial_offset) % self.tpb)
                if remainder and not partial:
                    print "Marker is off by %d" % remainder
                # Start the next bar.
                partial = False
                partial_offset = remainder

            elif expr is PARTIAL:
                partial = True

            elif isinstance(expr, Voice):
                nested = Melody(expr, self.tpb).schedule_notes()
                nested = [n._replace(begin=n.begin + relative_marker)
                    for n in nested]
                # If the next thing's not part of a voice, bump the relative
                # marker.
                if (len(self.music.exprs) > i + 1 and
                    not isinstance(self.music.exprs[i + 1], Voice)):
                        relative_marker = (nested[-1].begin +
                            nested[-1].duration)
                scheduled.extend(nested)

            elif isinstance(expr, Chord):
                begin = relative_marker
                relative_marker = begin + expr.notes[0].duration

                for note in expr.notes:
                    scheduled.append(ScheduledNote(note.pitch, begin,
                        note.duration))

            elif isinstance(expr, SciNote):
                begin = relative_marker
                relative_marker = begin + expr.duration

                scheduled.append(ScheduledNote(expr.pitch, begin,
                    expr.duration))

            elif isinstance(expr, Rest):
                begin = relative_marker
                relative_marker = begin + expr.duration

        return scheduled, relative_marker

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
