from __future__ import division

from StringIO import StringIO
from fractions import gcd
from itertools import takewhile

from fluidsynth.fluidsynth import FluidEvent

from lye.MidiFile import MIDIFile
from lye.instruments import (instruments as midi_instruments,
                             numbered_instruments, HIGHEST, NEAREST, LOWEST)

INSTRUMENT, LYNE, PAN, VOLUME = range(4)

TACET = object()

def multipliers(numbers):
    denominator = reduce(gcd, numbers)
    numerator = max(numbers) // denominator
    lcm = denominator * numerator
    return [lcm // number for number in numbers]

def find_instrument(name):
    """
    Attempt to fully qualify a MIDI instrument name.
    """

    name = name.lower()
    found = []

    for instrument in midi_instruments:
        if instrument.startswith(name):
            found.append(instrument)

    if len(found) < 1:
        raise Exception("Couldn't match any instruments for %s" % name)
    elif len(found) > 1:
        raise Exception("Found multiple instruments for %s: %s"
            % (name, found))
    return found[0]

class Timelyne(object):
    """
    A song assembled from Lye snippets.
    """

    tempo = 120
    ticks_per_beat = 120

    _drum_channel = None
    _previous_lynes = None

    def __init__(self, library):
        self.library = library

        self.channels = [[] for chaff in range(16)]
        self.marks = [[] for chaff in range(16)]
        self._previous_instruments = [None] * 16

    @classmethod
    def from_lines(cls, library, lines):
        """
        Parse a song from a series of lines.
        """

        self = cls(library)
        for line in lines:
            print "==="
            line = line.strip()
            marker, line = line[0], line[1:]
            if marker == "\\":
                # Directive. Add it and break.
                self.directive(line)
                continue
            elif marker == '"':
                self.set_marks()
                continue

            tokens = [i.strip() for i in line.split("|")]
            if marker == ">":
                self.set_instruments(tokens)
            elif marker == "%":
                self.set_pan(tokens)
            elif marker == "@":
                self.set_volume(tokens)
            elif marker == "&":
                self.add_lynes(tokens)
            else:
                print "Unknown marker %s with line %r" % (marker, line)

        self.reorder()

        return self

    def directive(self, d):
        name, arguments = d.split(" ", 1)
        try:
            getattr(self, "directive_%s" % name)(arguments)
        except AttributeError:
            raise Exception("Unknown directive %s" % name)

    def directive_tempo(self, args):
        self.tempo = int(args)
        print "Tempo: %d" % self.tempo

    def set_marks(self):
        for i, channel in enumerate(self.channels):
            if self.marks[i]:
                previous = self.marks[-1][1]
            else:
                previous = 0
            mark = slice(previous, len(channel))
            self.marks[i].append(mark)
            print "%d: Mark %r" % (i, mark)

    def set_instruments(self, instruments):
        for i, instrument in enumerate(instruments):
            if instrument == "drums":
                self._drum_channel = i
                print "%d: Drums" % i
            elif instrument in ("-", '"'):
                print "%d: No change" % i
            else:
                instrument = find_instrument(instrument)
                self._previous_instruments[i] = instrument
                print "%d: Instrument %s" % (i, instrument)
                self.channels[i].append((INSTRUMENT, instrument))

    def set_pan(self, pans):
        for i, pan in enumerate(pans):
            self.channels[i].append((PAN, int(pan)))

    def set_volume(self, volumes):
        for i, volume in enumerate(volumes):
            self.channels[i].append((VOLUME, int(volume)))

    def add_lynes(self, names):
        melodies = []
        for i, name in enumerate(names):
            if name == '"':
                if self._previous_lynes:
                    melodies.append(self._previous_lynes[i])
                else:
                    raise Exception("Can't use \" in first lyne!")
            elif name == "-":
                melodies.append(TACET)
            else:
                basename = "".join(takewhile(lambda c: c not in "',[", name))
                fit = NEAREST
                voice = None
                iterator = iter(name[len(basename):])
                for c in iterator:
                    if c == ",":
                        fit = LOWEST
                    elif c == "'":
                        fit = HIGHEST
                    elif c == "[":
                        voice = takewhile(lambda c: c != "]", iterator)
                        voice = int("".join(voice))

                snippet = self.library.snippets()[basename]
                melody = snippet.melody()
                melody.fit_method = fit
                if voice is not None:
                    melody = melody.split()[voice]
                melodies.append(melody)

        self._previous_lynes = melodies

        lengths = []
        for m in melodies:
            if m is TACET:
                lengths.append(1)
            else:
                lengths.append(len(m))

        muls = multipliers(lengths)
        total = lengths[0] * muls[0]

        for i, melody in enumerate(melodies):
            if melody is TACET:
                print "%d: Tacet (%d)" % (i, total)
                self.channels[i].append((TACET, total))
            else:
                instrument = self._previous_instruments[i]
                new = melody * muls[i]
                new.instrument = instrument
                new.fit_method = melody.fit_method
                new.fit()
                print "%d: %d ticks (%d), %s %d" % (i, len(new), len(melody),
                        instrument, new.fit_method)
                self.channels[i].append((LYNE, new))

    def reorder(self):
        dc = self._drum_channel
        drums = 9
        cs = self.channels
        if dc != drums:
            print "Moving drums", dc, "->", drums
            cs[dc], cs[drums] = cs[drums], cs[dc]

    def to_fs(self, sequencer):
        time = [0] * len(self.channels)

        sequencer.beats_per_minute = self.tempo
        sequencer.ticks_per_beat = self.ticks_per_beat

        ticks = sequencer.ticks
        ticks += self.ticks_per_beat

        # Each item in the seq is (fluidsynth.FS, (dest, destname))
        # We just want the dest
        dest = sequencer.items()[0][1][0]

        for channel, l in enumerate(self.channels):
            for t, data in l:
                if t is LYNE:
                    for pitch, begin, duration in data.scheduled:
                        begin += time[channel]
                        event = FluidEvent()
                        event.dest = dest
                        event.note(0, pitch, 127, duration)
                        sequencer.send(event, ticks + begin)
                    time[channel] += len(data)
                elif t is TACET:
                    time[channel] += data

        elapsed = sequencer.ticks - ticks
        print "Spare ticks", elapsed

    def to_midi(self):
        f = MIDIFile(len(self.channels), ticksPerBeat=self.ticks_per_beat)

        time = [0] * len(self.channels)
        track = 0

        f.addTrackName(track, 0, "Lye")
        f.addTempo(track, 0, self.tempo)

        for channel, l in enumerate(self.channels):
            for t, data in l:
                if t is INSTRUMENT:
                    f.addProgramChange(track, channel, time[channel],
                            numbered_instruments[data])
                elif t is LYNE:
                    for pitch, begin, duration in data.scheduled:
                        begin = begin / data.tpb + time[channel]
                        duration = duration / data.tpb
                        f.addNote(track, channel, pitch, begin, duration,
                                data.volume)
                    time[channel] += len(data) / self.ticks_per_beat
                elif t is PAN:
                    f.addControllerEvent(track, channel, time[channel], 0x0a,
                            data)
                elif t is VOLUME:
                    f.addControllerEvent(track, channel, time[channel], 0x07,
                            data)
                elif t is TACET:
                    time[channel] += data / self.ticks_per_beat

        sio = StringIO()
        f.writeFile(sio)
        return sio.getvalue()
