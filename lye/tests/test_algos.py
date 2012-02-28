import unittest
from warnings import catch_warnings, simplefilter

from lye.algos import simplify_ties
from lye.grammar import Note
from lye.types import Marker

class TestSimplifyTies(unittest.TestCase):

    def test_no_ties(self):
        notes = [Note(42, 120), Note(42, 120)]
        expected = notes[:]
        simplify_ties(notes)
        self.assertEqual(notes, expected)

    def test_valid_tie(self):
        notes = [Note(42, 120), Marker("tie"), Note(42, 120)]
        expected = [Note(42, 240)]
        simplify_ties(notes)
        self.assertEqual(notes, expected)

    def test_invalid_tie_differing(self):
        notes = [Note(42, 120), Marker("tie"), Note(43, 120)]
        expected = notes[:]
        with catch_warnings(record=True) as w:
            simplefilter("always")
            simplify_ties(notes)
            self.assertEqual(notes, expected)
            self.assertEqual(len(w), 1)

    def test_invalid_tie_beginning(self):
        notes = [Marker("tie"), Note(43, 120)]
        expected = notes[:]
        with catch_warnings(record=True) as w:
            simplefilter("always")
            simplify_ties(notes)
            self.assertEqual(notes, expected)
            self.assertEqual(len(w), 1)

    def test_invalid_tie_end(self):
        notes = [Note(42, 120), Marker("tie")]
        expected = notes[:]
        with catch_warnings(record=True) as w:
            simplefilter("always")
            simplify_ties(notes)
            self.assertEqual(notes, expected)
            self.assertEqual(len(w), 1)
