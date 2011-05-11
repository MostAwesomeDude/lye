from lye.grammar import Chord, Note, Marker, LyGrammar, LyeError

class Melody(object):

    ticks_per_beat = 480

    def __init__(self, notes):
        self.notes = notes
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
                end = begin + note.duration
                relative_marker = end

                for pitch in note.pitches:
                    scheduled.append((pitch, begin, end))

            elif isinstance(note, Note):
                # Note
                begin = relative_marker
                # XXX fudge?
                end = begin + note.duration
                relative_marker = end

                # If this note isn't a rest...
                if note.pitch != -1:
                    scheduled.append((note.pitch, begin, end))

        self.notes = scheduled

    def play(self, sequencer):
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

        for pitch, begin, end in self.notes:
            event = fluidsynth.FluidEvent()
            event.dest = dest
            # XXX ? pitch vel duration
            event.note(0, pitch, 127, end - begin)
            sequencer.send(event, ticks + begin)

def melody_from_ly(s):
    """
    Make a `Melody` from a ly string.
    """

    melody = Melody(LyGrammar(s).apply("melody")[0])
    if not melody:
        raise LyeError("Failed melody %s" % s)
    return melody
