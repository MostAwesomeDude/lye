from __future__ import division

from fractions import gcd
from itertools import takewhile

from lye.instruments import numbered_instruments, HIGHEST, NEAREST, LOWEST
from lye.utilities import find_instrument, make_velocity

INSTRUMENT, LYNE, PAN, VOLUME = range(4)

TACET = object()

def multipliers(numbers):
    denominator = reduce(gcd, numbers)
    numerator = max(numbers) // denominator
    lcm = denominator * numerator
    return [lcm // number for number in numbers]

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
            print ">",
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

        # Last-minute things. Set a final mark, because of the fencepost
        # problem. Also, reorder tracks so that the drum channel is relocated.
        self.set_marks()
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
        print "Mark"
        for i, channel in enumerate(self.channels):
            if self.marks[i]:
                previous = self.marks[i][-1].stop
            else:
                previous = 0
            mark = slice(previous, len(channel))
            self.marks[i].append(mark)

    def set_instruments(self, instruments):
        for i, instrument in enumerate(instruments):
            if instrument == "drums":
                self._drum_channel = i
                self.channels[i].append((INSTRUMENT, 0))
            elif instrument in ("-", '"'):
                pass
            else:
                instrument = find_instrument(instrument)
                self._previous_instruments[i] = instrument
                self.channels[i].append((INSTRUMENT,
                    numbered_instruments[instrument]))

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
                self.channels[i].append((TACET, total))
            else:
                instrument = self._previous_instruments[i]
                new = melody * muls[i]
                new.fit_method = melody.fit_method
                new.change_instrument(instrument)
                self.channels[i].append((LYNE, new))

        print "Lyne of %d" % total

    def reorder(self):
        dc = self._drum_channel
        drums = 9
        cs = self.channels
        ms = self.marks
        if dc != drums:
            print "Moving drums", dc, "->", drums
            cs[dc], cs[drums] = cs[drums], cs[dc]
            ms[dc], ms[drums] = ms[drums], ms[dc]

    def export(self, mark, exporter):
        """
        Export a mark.
        """

        time = [0] * len(self.channels)

        for channel, (marks, l) in enumerate(zip(self.marks, self.channels)):
            m = marks[mark]
            for t, data in l[m]:
                if t is INSTRUMENT:
                    exporter.pc(channel, time[channel], data)
                elif t is LYNE:
                    for pitch, velocity, begin, duration in data.scheduled:
                        if pitch == 0 and duration == 0:
                            # Hax'd pitch bend data.
                            exporter.bend(channel, time[channel] + begin,
                                    velocity)
                        else:
                            velocity = make_velocity(velocity)
                            exporter.note(channel, time[channel] + begin,
                                    duration, pitch, velocity)
                    time[channel] += len(data)
                elif t is TACET:
                    time[channel] += data

        return max(time), exporter.commit()
