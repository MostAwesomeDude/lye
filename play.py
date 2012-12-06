#!/usr/bin/env python

import sys

from twisted.internet import reactor
from twisted.python.filepath import FilePath
from twisted.python.reflect import namedAny

from lye.lybrary import Lybrary
from lye.tiedye import play_a_bynder

if len(sys.argv) < 2:
    print "Usage:", sys.argv[0], "<song>"
    sys.exit(0)

print "Loading..."

song = namedAny(sys.argv[1])

bound = song(Lybrary(FilePath("/home/simpson/lybrary")))

print "Playing..."

bound.ticks_per_beat = 120

ml = play_a_bynder(bound, "gugs.sf2")

reactor.callWhenRunning(ml.start, reactor)
reactor.run()
