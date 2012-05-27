from pymeta.grammar import OMeta

from lye.ast import CLOSE_SLUR, OPEN_SLUR, Slur
from lye.visitors.visitor import hasfield, Visitor

slur_maker = """
open  ::= <exactly OPEN_SLUR>
close ::= <exactly CLOSE_SLUR>
not_close ::= <anything>:x ?( x is not CLOSE_SLUR ) => x
slur  ::= <anything>:x <open> <not_close>+:xs <close> => Slur([x] + xs)
slurify ::= (<slur> | <anything>)*
"""

class SlurMaker(Visitor):
    class Inner(OMeta.makeGrammar(slur_maker, globals())):
        pass

    def visit_generic(self, node):
        if hasfield(node, "exprs"):
            exprs, error = self.Inner(node.exprs).apply("slurify")
            node = node._replace(exprs=exprs)

        return node, True
