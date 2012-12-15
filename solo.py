#!/usr/bin/env python

import sys

from twisted.internet import reactor
from twisted.python.filepath import FilePath

from lye.lybrary import Lye
from lye.tiedye import play_a_bynder

if len(sys.argv) < 2:
    print "Usage:", sys.argv[0], "<snippet> <instrument>"
    sys.exit(0)

print "Loading..."

snippet = Lye(FilePath(sys.argv[1]))

instrument = " ".join(sys.argv[2:])

bound = snippet.bynder(instrument)

print "Playing..."

bound.ticks_per_beat = 120
bound.tempo = 120

ml = play_a_bynder(bound, "gugs.sf2")

reactor.callWhenRunning(ml.start, reactor)
reactor.run()
