#!/usr/bin/env python

import sys

from lye.combyne import Bynder
from lye.grammar import LyeGrammar
from lye.pretty import pretty
from lye.utilities import find_instrument
from lye.visitors import simplify_ast

if len(sys.argv) < 2:
    print "Usage:", sys.argv[0], "<snippet> [<instrument>]"
    sys.exit(0)

instrument = None

if len(sys.argv) >= 3:
    instrument = find_instrument(" ".join(sys.argv[2:]))

with open(sys.argv[1], "rb") as f:
    s = f.read()

g = LyeGrammar(s)
ast = g.lye()
bound = Bynder(simplify_ast(ast), instrument)

print "AST:"
print pretty(bound)

print "AST (%s):" % instrument
print pretty(bound.specialized())

print "Scheduled:"
print pretty(bound.schedule())
