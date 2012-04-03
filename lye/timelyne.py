from fractions import gcd
from operator import mul

from lye.instruments import instruments as midi_instruments

INSTRUMENT, LYNE = range(2)

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

    def __init__(self, library):
        self.channels = [[] for chaff in range(16)]
        self.library = library

    @classmethod
    def from_lines(cls, library, lines):
        """
        Parse a song from a series of lines.
        """

        self = cls(library)
        for line in lines:
            marker, line = line[0], line[1:]
            tokens = [i.strip() for i in line.split("|")]
            if marker == ">":
                self.set_instruments(tokens)
            elif marker == "&":
                self.add_lynes(tokens)
            else:
                print "Unknown marker %s with line %r" % (marker, line)

        return self

    def set_instruments(self, instruments):
        for i, instrument in enumerate(instruments):
            instrument = find_instrument(instrument)
            print "Setting %d to %s" % (i, instrument)
            self.channels[i].append((INSTRUMENT, instrument))

    def add_lynes(self, names):
        melodies = []
        for i, name in enumerate(names):
            snippet = self.library.snippets()[name]
            melodies.append(snippet.melody())

        lengths = [len(m) for m in melodies]
        muls = multipliers(lengths)

        print "Using multipliers %r" % muls

        for i, melody in enumerate(melodies):
            print "Setting %d to %s" % (i, melody)
            self.channels[i].append((LYNE, melody))
