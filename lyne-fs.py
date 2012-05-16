#!/usr/bin/env python

from __future__ import division

import sys

from twisted.internet import reactor

from lye.library import Library
from lye.lyne import Timelyne

if len(sys.argv) < 4:
    print "Usage:", sys.argv[0], "<library> <lyne> <soundfont>"
    sys.exit(1)

library = Library(sys.argv[1])

with open(sys.argv[2], "rb") as f:
    lyne = Timelyne.from_lines(library, f)

    from fluidsynth import fluidsynth

    settings = fluidsynth.FluidSettings()
    settings.quality = "low"

    synth = fluidsynth.FluidSynth(settings)
    synth.load_soundfont(sys.argv[3])

    driver = fluidsynth.FluidAudioDriver(settings, synth)

    sequencer = fluidsynth.FluidSequencer()
    sequencer.beats_per_minute = 120
    sequencer.ticks_per_beat = 480

    sequencer.add_synth(synth)

    def fill(mark):
        length = lyne.to_fs(mark, sequencer)
        mark += 1

        length /= lyne.ticks_per_beat * lyne.tempo / 60

        if mark >= len(lyne.marks[0]):
            reactor.callLater(length, reactor.stop)
        else:
            reactor.callLater(length, fill, mark)

    reactor.callLater(0, fill, 0)

    reactor.run()
