#!/usr/bin/env python

import sys

from lye.combyne import Bynder
from lye.grammar import LyeGrammar
from lye.pretty import pretty
from lye.tiedye import play_a_bynder
from lye.visitors import simplify_ast

if len(sys.argv) < 3:
    print "Usage:", sys.argv[0], "<snippet> <instrument>"
    sys.exit(0)

with open(sys.argv[1], "rb") as f:
    s = f.read()

instrument = " ".join(sys.argv[2:])

g = LyeGrammar(s)
ast = g.lye()
bound = Bynder(simplify_ast(ast), instrument)

print "AST (%s):" % instrument
print pretty(bound.specialized())

print "Playing..."

bound.tempo = 120
bound.ticks_per_beat = 120

ml = play_a_bynder(bound, "gugs.sf2")

from twisted.internet import reactor
reactor.callWhenRunning(ml.start, reactor)
reactor.run()
