from unittest import TestCase

from lye.ast import TIE, Drums, Music, SciNote
from lye.visitors.maps import DrumsTransformer
from lye.visitors.peephole import TieSimplifier

class TestDrumsTransformer(TestCase):

    def test_simplify_drums(self):
        ast = Drums(expr="test")
        result = DrumsTransformer().visit(ast)
        self.assertEqual(result, "test")

class TestTieSimplifier(TestCase):

    def test_no_ties(self):
        notes = Music([SciNote(42, 120, None), SciNote(42, 120, None)])
        result = TieSimplifier().visit(notes)
        self.assertEqual(result, notes)

    def test_valid_tie(self):
        notes = Music([SciNote(42, 120, None), TIE, SciNote(42, 120, None)])
        expected = Music([SciNote(42, 240, None)])
        result = TieSimplifier().visit(notes)
        self.assertEqual(result, expected)
