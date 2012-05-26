from warnings import warn

from lye.ast import TIE, SciNote
from lye.visitors.visitor import hasfield, Visitor

class LyeParseWarning(Warning):
    """
    A dubious token was encountered while parsing a melody.
    """

def peephole(*types):
    def inner(f):
        def do_peepholes(ast):
            if not hasfield(ast, "exprs"):
                return

            i = 0
            lt = len(types)
            while len(ast.exprs) >= i + lt:
                matched = False
                window = ast.exprs[i:i + lt]
                if all(isinstance(*args) for args in zip(window, types)):
                    ast.exprs[i:i + lt], matched = f(*window)

                if not matched:
                    i += 1

        def visit(self, ast):
            do_peepholes(ast)
            return ast, True

        return type(f.__name__, (Visitor,), {"visit_generic": visit})

    return inner

@peephole(SciNote, object, SciNote)
def TieSimplifier(before, tie, after):
    if tie is not TIE:
        return [before, tie, after], False

    if before.pitch == after.pitch:
        return ([before._replace(duration=before.duration + after.duration)],
                True)
    else:
        warn("Tie between differing pitches", LyeParseWarning)
        return [], False
