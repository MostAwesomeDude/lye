from copy import deepcopy
from StringIO import StringIO

from lye.instruments import numbered_instruments
from lye.MidiFile import MIDIFile

class Song(object):
    """
    A collection of melodies.
    """

    tempo = 120
    ticks_per_beat = 120

    def __init__(self):
        self.melodies = {}

    def __imul__(self, value):
        for channel in self.melodies:
            self.melodies[channel] *= value
        return self

    def add_melody(self, melody, channel, instrument=None):
        """
        Add a melody to this song.

        It is up to the caller to avoid clobbering channels; this method will
        gladly replace a melody with another melody given the chance.

        The instrument may be None if the channel is 10 (drums).
        """

        # XXX Hax: Do a deep copy since melodies are pretty easy to reuse and
        # we might mutate them later.
        copied = deepcopy(melody)
        if instrument is not None:
            copied.instrument = instrument
        self.melodies[channel] = copied

    def to_midi(self):
        f = MIDIFile(len(self.melodies), ticksPerBeat=self.ticks_per_beat)

        time = 0
        track = 0

        f.addTrackName(track, time, "Lye")
        f.addTempo(track, time, self.tempo)

        for channel, melody in self.melodies.iteritems():
            # Pan.
            f.addControllerEvent(track, channel, time, 0x0a, melody.pan)
            if melody.instrument in numbered_instruments:
                f.addProgramChange(track, channel, time,
                    numbered_instruments[melody.instrument])
            melody.to_midi(f, channel)

        sio = StringIO()
        f.writeFile(sio)
        return sio.getvalue()
