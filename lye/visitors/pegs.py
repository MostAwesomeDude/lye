from parsley import makeGrammar

from lye.ast import CLOSE_SLUR, OPEN_SLUR, Slur
from lye.visitors.visitor import hasfield, Visitor

slur_globals = {
    "OPEN_SLUR": OPEN_SLUR,
    "CLOSE_SLUR": CLOSE_SLUR,
    "Slur": Slur,
}

slur_maker = """
open  = exactly(OPEN_SLUR)
close = exactly(CLOSE_SLUR)
not_close = anything:x ?(x is not CLOSE_SLUR) -> x
slur  = anything:x open not_close+:xs close -> Slur([x] + xs)
slurify = (slur | anything)*
"""

_slurry = makeGrammar(slur_maker, slur_globals)

class SlurMaker(Visitor):

    def visit_generic(self, node):
        print node
        if hasfield(node, "exprs"):
            exprs = _slurry(node.exprs).slurify()
            node = node._replace(exprs=exprs)

        return node, True
