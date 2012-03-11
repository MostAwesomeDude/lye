from StringIO import StringIO

from lye.MidiFile import MIDIFile

class Song(object):
    """
    A collection of melodies.
    """

    ticks_per_beat = 480

    def __init__(self, *melodies):
        self.melodies = melodies

    def to_midi(self):
        f = MIDIFile(len(self.melodies), ticksPerBeat=self.ticks_per_beat)

        time = 0
        track = 0

        f.addTrackName(track, time, "Lye")
        f.addTempo(track, time, 84 / 4)

        for channel, melody in enumerate(self.melodies):
            melody.to_midi(f, channel)

        sio = StringIO()
        f.writeFile(sio)
        return sio.getvalue()
