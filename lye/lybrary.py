import os.path

from lye.combyne import Bynder, Drumlyne
from lye.grammar import LyeGrammar
from lye.melody import melody_from_ly
from lye.utilities import find_instrument
from lye.visitors import simplify_ast


class LybraryError(Exception):
    """
    Something went wrong while trying to do lybrarian stuff.
    """


class Lye(object):
    """
    A file with notes in it.
    """

    def __init__(self, path):
        self.path = path

    def bynder(self, instrument):
        instrument = find_instrument(instrument)
        with self.path.open("rb") as f:
            s = f.read()
            g = LyeGrammar(s)
            ast = g.lye()
            if instrument == "drums":
                return Drumlyne(simplify_ast(ast))
            else:
                return Bynder(simplify_ast(ast), instrument)

    def melody(self):
        with self.path.open("rb") as f:
            s = f.read()
            return melody_from_ly(s)


class Lybrary(object):
    """
    A folder with snippets.
    """

    def __init__(self, path):
        self.path = path
        if not self.path.exists():
            raise LybraryError("Path %r doesn't exist!" % self.path)

    def __div__(self, other):
        child = self.path.child(other)
        if child.exists():
            if child.isdir():
                return Lybrary(child)
            else:
                return Lye(child)
        else:
            child = self.path.child("%s.lye" % other)
            return Lye(child)

    def snippets(self):
        d = {}
        kids = self.path.globChildren("*.lye")
        for kid in kids:
            name = os.path.splitext(kid.basename())[0]
            d[name] = Lye(kid)
        return d
