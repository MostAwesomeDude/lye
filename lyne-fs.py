#!/usr/bin/env python

from __future__ import division

import sys

from twisted.internet import reactor

from lye.library import Library
from lye.lyne import Timelyne
from lye.tiedye import create_sequencer

if len(sys.argv) < 4:
    print "Usage:", sys.argv[0], "<library> <lyne> <soundfont>"
    sys.exit(1)

library = Library(sys.argv[1])

with open(sys.argv[2], "rb") as f:
    lyne = Timelyne.from_lines(library, f)
    synth, seq = create_sequencer(sys.argv[3])

    def fill(mark):
        length = lyne.to_fs(mark, seq)
        mark += 1

        length /= lyne.ticks_per_beat * lyne.tempo / 60

        if mark >= len(lyne.marks[0]):
            reactor.callLater(length, reactor.stop)
        else:
            reactor.callLater(length, fill, mark)

    reactor.callLater(0, fill, 0)

    reactor.run()
