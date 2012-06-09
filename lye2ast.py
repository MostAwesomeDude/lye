#!/usr/bin/env python

import sys

from lye.grammar import LyeGrammar
from lye.visitors import express_ast, simplify_ast

if len(sys.argv) < 2:
    print "Usage:", sys.argv[0], "<snippet> [<instrument>]"

with open(sys.argv[1], "rb") as f:
    s = f.read()

g = LyeGrammar(s)
ast = g.ast()
ast = simplify_ast(ast)

print "AST:"
print ast

if len(sys.argv) >= 3:
    instrument = " ".join(sys.argv[2:])
    ast = express_ast(ast, instrument)

    print "AST (%s):" % instrument
    print ast
