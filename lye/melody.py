from __future__ import division

from collections import namedtuple

from fluidsynth import fluidsynth

from lye.algos import simplify_ties
from lye.grammar import Chord, Note, Marker, LyGrammar, LyeError

NoteTuple = namedtuple("NoteTuple", "pitch, begin, duration")

class Melody(object):

    ticks_per_beat = 480

    def __init__(self, notes):
        self.notes = notes
        simplify_ties(notes)
        self.schedule_notes()

    def __nonzero__(self):
        return any(self.notes)

    def __repr__(self):
        return "Melody(%r)" % self.notes

    __str__ = __repr__

    def schedule_notes(self):
        """
        Schedule notes by turning their durations into absolute begin, end
        pairs.

        Additionally, this step discards rests.
        """

        relative_marker = 0
        partial = False
        partial_offset = 0
        scheduled = []

        for note in self.notes:
            if isinstance(note, Marker):
                if note.name == "measure":
                    remainder = ((relative_marker - partial_offset) %
                        self.ticks_per_beat)
                    if remainder and not partial:
                        print "Marker is off by %d" % remainder
                    # Start the next bar.
                    partial = False
                    partial_offset = remainder
                elif note.name == "partial":
                    partial = True
                continue

            elif isinstance(note, Chord):
                begin = relative_marker
                relative_marker = begin + note.duration

                for pitch in note.pitches:
                    scheduled.append(NoteTuple(pitch, begin, note.duration))

            elif isinstance(note, Note):
                # Note
                begin = relative_marker
                # XXX fudge?
                relative_marker = begin + note.duration

                # If this note isn't a rest...
                if note.pitch != -1:
                    scheduled.append(NoteTuple(note.pitch, begin,
                        note.duration))

        self.notes = scheduled

    def to_fs(self, sequencer):
        """
        Sends the melody to `sequencer`.
        """

        # XXX
        tpb = sequencer.ticks_per_beat
        # Each item in the seq is (fluidsynth.FS, (dest, destname))
        # We just want the dest
        dest = sequencer.items()[0][1][0]
        # XXX this fudge value might not be needed?
        ticks = sequencer.ticks + 10

        for pitch, begin, duration in self.notes:
            event = fluidsynth.FluidEvent()
            event.dest = dest
            # XXX ? pitch vel duration
            event.note(0, pitch, 127, duration)
            sequencer.send(event, ticks + begin)

    def to_midi(self, f, channel):
        """
        Create a MIDI expression for this melody.
        """

        track = 0

        for pitch, begin, duration in self.notes:
            begin = begin / self.ticks_per_beat
            duration = duration / self.ticks_per_beat
            f.addNote(track, channel, pitch, begin, duration, 127)

def melody_from_ly(s):
    """
    Make a `Melody` from a ly string.
    """

    melody = Melody(LyGrammar(s).apply("melody")[0])
    if not melody:
        raise LyeError("Failed melody %s" % s)
    return melody
