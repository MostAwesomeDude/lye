from StringIO import StringIO

from lye.instruments import numbered_instruments
from lye.MidiFile import MIDIFile

class Song(object):
    """
    A collection of melodies.
    """

    ticks_per_beat = 480

    def __init__(self):
        self.melodies = {}

    def add_melody(self, melody, channel, instrument):
        """
        Add a melody to this song.

        It is up to the caller to avoid clobbering channels; this method will
        gladly replace a melody with another melody given the chance.

        The instrument may be None if the channel is 10 (drums).
        """

        self.melodies[channel] = melody, instrument

    def to_midi(self):
        f = MIDIFile(len(self.melodies), ticksPerBeat=self.ticks_per_beat)

        time = 0
        track = 0

        f.addTrackName(track, time, "Lye")
        f.addTempo(track, time, 84 / 4)

        for channel in self.melodies:
            melody, instrument = self.melodies[channel]
            # Pan.
            f.addControllerEvent(track, channel, time, 0x0a, melody.pan)
            if instrument in numbered_instruments:
                f.addProgramChange(track, channel, time,
                    numbered_instruments[instrument])
            melody.to_midi(f, channel)

        sio = StringIO()
        f.writeFile(sio)
        return sio.getvalue()
