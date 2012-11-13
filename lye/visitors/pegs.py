from parsley import makeGrammar

from lye.ast import CLOSE_SLUR, OPEN_SLUR, Slur
from lye.visitors.visitor import hasfield, Visitor

slur_globals = {
    "OPEN_SLUR": OPEN_SLUR,
    "CLOSE_SLUR": CLOSE_SLUR,
    "Slur": Slur,
}

# The exactly() rule sucks really *really* hard for non-strings, so I hacked
# something a bit more fun into this one.

slur_maker = """
open = anything:x ?(x is OPEN_SLUR) -> x
close = anything:x ?(x is CLOSE_SLUR) -> x
not_close = ~close anything
slur  = anything:x open not_close+:xs close -> Slur([x] + xs)
slurify = (slur | anything)*
"""

_slurry = makeGrammar(slur_maker, slur_globals)

class SlurMaker(Visitor):

    def visit_generic(self, node):
        if hasfield(node, "exprs"):
            exprs = _slurry(node.exprs).slurify()
            node = node._replace(exprs=exprs)

        return node, True
