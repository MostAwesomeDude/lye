#!/usr/bin/env python

import sys

from lye.export import FileExporter
from lye.library import Library
from lye.lyne import Timelyne

if len(sys.argv) < 4:
    print "Usage:", sys.argv[0], "<library> <lyne> <midi>"
    sys.exit(1)

library = Library(sys.argv[1])

with open(sys.argv[2], "rb") as f:
    lyne = Timelyne.from_lines(library, f)
    exporter = FileExporter(16, lyne.tempo, lyne.ticks_per_beat)
    elapsed, data = lyne.export(0, exporter)
    with open(sys.argv[3], "wb") as out:
        out.write(data)
