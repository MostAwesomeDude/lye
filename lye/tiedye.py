"""
Twisted-specific Lye support functions and objects.

Also contains Fluidsynth stuff.
"""

from __future__ import division

from collections import namedtuple
import sys

from lye.library import Library
from lye.lyne import Timelyne

Seq = namedtuple("Seq", "sequencer, synthesizer, driver")

def create_sequencer(*soundfonts):
    """
    Obtain a Fluidsynth sequencer.

    Also return the synthesizer.
    """

    from fluidsynth import fluidsynth

    settings = fluidsynth.FluidSettings()
    settings.quality = "low"
    settings["audio.realtime-prio"] = 0
    settings["synth.audio-groups"] = 1
    settings["synth.ladspa.active"] = False
    settings["synth.verbose"] = False

    synth = fluidsynth.FluidSynth(settings)
    for soundfont in soundfonts:
        synth.load_soundfont(soundfont)

    driver = fluidsynth.FluidAudioDriver(settings, synth)

    seq = fluidsynth.FluidSequencer()
    seq.beats_per_minute = 120
    seq.ticks_per_beat = 480

    seq.add_synth(synth)

    return Seq(seq, synth, driver)

class MarkedLyne(object):

    delayed = None
    mark = 0

    def __init__(self, lyne, seq):
        self.lyne = lyne
        self.seq = seq

    def start(self, reactor):
        self.reactor = reactor
        self.refill()

    def stop(self):
        if self.delayed:
            self.delayed.cancel()

    def refill(self):
        time = self.fill()
        self.delayed = self.reactor.callLater(time, self.refill)

    def fill(self):
        length = self.lyne.to_fs(self.mark, self.seq.sequencer)
        length /= self.lyne.ticks_per_beat * self.lyne.tempo / 60
        return length

def just_go_already(lib, lyne, *soundfonts):
    library = Library(lib)

    with open(lyne, "rb") as f:
        lyne = Timelyne.from_lines(library, f)
        seq = create_sequencer(*soundfonts)
        return MarkedLyne(lyne, seq)
