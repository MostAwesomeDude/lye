"""
Twisted-specific Lye support functions and objects.

Also contains Fluidsynth stuff.
"""

from __future__ import division

from collections import namedtuple

from lye.export import FSExporter
from lye.library import Library
from lye.lyne import Timelyne

Seq = namedtuple("Seq", "sequencer, synthesizer, driver")

def mma(old, new, weight):
    """
    Performs a Moving Modified Average, using the old value, new value,
    and a weight.

    Weight must be greater than zero.

    From madsnippets/maths.py.
    """

    return ((weight - 1) * old + new) // weight

def create_sequencer(*soundfonts):
    """
    Obtain a Fluidsynth sequencer.

    Also return the synthesizer.
    """

    from fluidsynth import fluidsynth

    settings = fluidsynth.FluidSettings()
    # settings.quality = "low"
    settings["audio.realtime-prio"] = 0
    settings["synth.threadsafe-api"] = True
    # settings["synth.verbose"] = True

    synth = fluidsynth.FluidSynth(settings)
    for soundfont in soundfonts:
        synth.load_soundfont(soundfont)

    driver = fluidsynth.FluidAudioDriver(synth)

    seq = fluidsynth.FluidSequencer()
    seq.beats_per_minute = 120
    seq.ticks_per_beat = 480

    seq.add_synth(synth)

    return Seq(seq, synth, driver)

class MarkedLyne(object):

    delayed = None
    # This default doesn't reflect anything meaningful; typically, fills take
    # 1-10 ticks, so this usually starts way too high. However, we want a
    # latency of less than 100 ticks for responsiveness.
    offset = 42

    def __init__(self, lyne, seq):
        self.lyne = lyne
        self.seq = seq

    def start(self, reactor):
        self.reactor = reactor
        self.attune()
        self.refill()

    def stop(self):
        if self.delayed:
            self.delayed.cancel()

    def attune(self):
        self.seq.sequencer.beats_per_minute = self.lyne.tempo
        self.seq.sequencer.ticks_per_beat = self.lyne.ticks_per_beat

        # Rewind to before the beginning of the lyne.
        self.offset -= self.lyne.begin()

    def refill(self):
        time = self.fill()
        self.delayed = self.reactor.callLater(time, self.refill)

    def fill(self):
        exporter = FSExporter(self.seq.sequencer, self.offset)
        # Perform the actual fill.
        length, elapsed = self.lyne.export(exporter)

        # Figure out what our next offset is going to be, based on how long
        # that took, and when the next sequence starts. A weight of 10 is
        # pretty decent.
        new_offset = mma(self.offset, elapsed, 10)

        # Always ensure that our next offset is far enough in advance that it
        # doesn't affect the next sequence.
        new_offset -= self.lyne.begin()

        # And now determine how long we should wait, in ticks, for the next
        # fill.
        time = length - elapsed + (self.offset - new_offset)

        print "Latency: %d/%d ticks (%d taken)" % (
                self.offset, new_offset, elapsed)

        # Ticks -> seconds.
        time /= self.lyne.ticks_per_beat * self.lyne.tempo / 60

        # Save the offset, and return the time.
        self.offset = new_offset
        return time

def just_go_already(lib, lyne, *soundfonts):
    library = Library(lib)

    with open(lyne, "rb") as f:
        lyne = Timelyne.from_lines(library, f)
        seq = create_sequencer(*soundfonts)
        return MarkedLyne(lyne, seq)

def play_a_bynder(bynder, *soundfonts):
    seq = create_sequencer(*soundfonts)
    return MarkedLyne(bynder, seq)

def demo():
    return just_go_already("groove", "groove.lyne", "gugs.sf2")
