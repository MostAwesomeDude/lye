#!/usr/bin/env python

from __future__ import division

import sys

from twisted.internet import reactor

from lye.library import Library
from lye.lyne import Timelyne
from lye.tiedye import create_sequencer, MarkedLyne

if len(sys.argv) < 4:
    print "Usage:", sys.argv[0], "<library> <lyne> <soundfont>"
    sys.exit(1)

library = Library(sys.argv[1])

with open(sys.argv[2], "rb") as f:
    lyne = Timelyne.from_lines(library, f)
    seq = create_sequencer(sys.argv[3])

    marked = MarkedLyne(lyne, seq)
    marked.start(reactor)

    reactor.run()
