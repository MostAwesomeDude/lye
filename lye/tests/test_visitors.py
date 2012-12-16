from unittest import TestCase

from lye.ast import (CLOSE_SLUR, OPEN_SLUR, TIE, Drums, Music, Rest, SciNote,
                     Slur)
from lye.visitors.maps import DrumsTransformer, Legato
from lye.visitors.peephole import RestMerger, TieRemover
from lye.visitors.pegs import SlurMaker

class TestDrumsTransformer(TestCase):

    def test_simplify_drums(self):
        ast = Drums(expr="test")
        result = DrumsTransformer().visit(ast)
        self.assertEqual(result, "test")

class TestTieRemover(TestCase):

    def test_no_ties(self):
        notes = Music([SciNote(42, 120, None, None),
            SciNote(42, 120, None, None)])
        result = TieRemover().visit(notes)
        self.assertEqual(result, notes)

    def test_valid_tie(self):
        notes = Music([SciNote(42, 120, None, None), TIE,
            SciNote(42, 120, None, None)])
        expected = Music([SciNote(42, 240, None, None)])
        result = TieRemover().visit(notes)
        self.assertEqual(result, expected)

class TestRestMerger(TestCase):

    def test_merge_rests(self):
        rests = Music([Rest(100), Rest(20)])
        expected = Music([Rest(120)])
        result = RestMerger().visit(rests)
        self.assertEqual(result, expected)

class TestSlurMaker(TestCase):

    def test_make_slur(self):
        notes = Music(["a", OPEN_SLUR, "b", CLOSE_SLUR])
        expected = Music([Slur(["a", "b"])])
        result = SlurMaker().visit(notes)
        self.assertEqual(result, expected)

    def test_make_slur_multiple(self):
        notes = Music(["a", OPEN_SLUR, "b", "c", CLOSE_SLUR])
        expected = Music([Slur(["a", "b", "c"])])
        result = SlurMaker().visit(notes)
        self.assertEqual(result, expected)

class TestLegato(TestCase):

    def test_shorten_not_too_short(self):
        note = SciNote(duration=480, pitch=None, velocity=None,
                articulation=None)
        note, rest = Legato.shorten(note)
        self.assertEqual(470, note.duration)
        self.assertEqual(10, rest.duration)

    def test_shorten_just_right(self):
        note = SciNote(duration=30, pitch=None, velocity=None,
                articulation=None)
        note, rest = Legato.shorten(note)
        self.assertEqual(27, note.duration)
        self.assertEqual(3, rest.duration)
