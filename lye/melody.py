from __future__ import division

from fluidsynth import fluidsynth

from lye.algos import simplify_ties
from lye.grammar import Chord, Note, Marker, LyGrammar, LyeError
from lye.instruments import NEAREST, fit

class Melody(object):

    ticks_per_beat = 480

    instrument = None
    volume = 127
    pan = 63

    def __init__(self, notes):
        self.notes = notes
        simplify_ties(notes)

    def __nonzero__(self):
        return any(self.notes)

    def __repr__(self):
        return "Melody(%r)" % self.notes

    __str__ = __repr__

    def __mul__(self, value):
        """
        Extend this melody.
        """

        other = Melody(self.notes)
        other.notes *= value
        other.pan = self.pan
        other.volume = self.volume
        other.instrument = self.instrument
        return other

    def __imul__(self, value):
        self.notes *= value
        return self

    def fit(self, strategy=NEAREST):
        """
        Force this melody to be within the range of its instrument.
        """

        if self.instrument:
            fit(self, self.instrument, strategy)

    def split(self):
        """
        Split this melody into harmonies.

        The melody should be harmonized somewhat already.

        Attempts are made to put as much work into the top of the melody as
        possible.

        Returned melodies are high-to-low.
        """

        count = 0

        for o in self.notes:
            if isinstance(o, Chord):
                count = max(count, len(o.pitches))

        melodies = [[] for i in range(count)]

        for o in self.notes:
            if isinstance(o, Chord):
                for i, pitch in enumerate(sorted(o.pitches, reverse=True)):
                    melodies[i].append(Note(pitch, None, o.duration))
                for i in range(i + 1, len(melodies)):
                    # Create rests.
                    melodies[i].append(Note(-1, None, o.duration))
            else:
                for melody in melodies:
                    melody.append(o)

        rv = []
        for m in melodies:
            melody = Melody(m)
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
                    scheduled.append(Note(pitch, begin, note.duration))

            elif isinstance(note, Note):
                # Note
                begin = relative_marker
                # XXX fudge?
                relative_marker = begin + note.duration

                # If this note isn't a rest...
                if note.pitch != -1:
                    scheduled.append(note._replace(begin=begin))

        return scheduled

    def to_fs(self, sequencer):
        """
        Sends the melody to `sequencer`.
        """

        scheduled = self.schedule_notes()

        # XXX
        tpb = sequencer.ticks_per_beat
        # Each item in the seq is (fluidsynth.FS, (dest, destname))
        # We just want the dest
        dest = sequencer.items()[0][1][0]
        # XXX this fudge value might not be needed?
        ticks = sequencer.ticks + 10

        for pitch, begin, duration in scheduled:
            event = fluidsynth.FluidEvent()
            event.dest = dest
            # XXX ? pitch vel duration
            event.note(0, pitch, self.volume, duration)
            sequencer.send(event, ticks + begin)

    def to_midi(self, f, channel):
        """
        Create a MIDI expression for this melody.
        """

        scheduled = self.schedule_notes()

        track = 0

        for pitch, begin, duration in scheduled:
            begin = begin / self.ticks_per_beat
            duration = duration / self.ticks_per_beat
            f.addNote(track, channel, pitch, begin, duration, self.volume)

def melody_from_ly(s):
    """
    Make a `Melody` from a ly string.
    """

    melody = Melody(LyGrammar(s).apply("melody")[0])
    if not melody:
        raise LyeError("Failed melody %s" % s)
    return melody
