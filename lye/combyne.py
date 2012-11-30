from lye.ast import nt
from lye.visitors import express_ast
from lye.visitors.folds import NoteScheduler, fold

class Bynder(nt("Bynder", "ast instrument")):
    """
    A snippet of lye, processed and prepared for export or combination with
    other snippets.

    Bynders contain all of the data necessary for specializing an AST, as well
    as the AST itself and any trimmings.
    """

    def specialized(self):
        return self._replace(ast=express_ast(self.ast, self.instrument))

    def schedule(self):
        return fold(NoteScheduler, self.specialized().ast)
