import unittest
from warnings import catch_warnings, simplefilter

from lye.algos import pitch_to_number, simplify_ties
from lye.grammar import Note
from lye.types import Marker

class TestPitchToNumber(unittest.TestCase):

    def test_middle_c(self):
        self.assertEqual(pitch_to_number("c", 0, 0), 48)

class TestSimplifyTies(unittest.TestCase):

    def test_no_ties(self):
        notes = [Note(42, None, 120), Note(42, None, 120)]
        expected = notes[:]
        simplify_ties(notes)
        self.assertEqual(notes, expected)

    def test_valid_tie(self):
        notes = [Note(42, None, 120), Marker("tie"), Note(42, None, 120)]
        expected = [Note(42, None, 240)]
        simplify_ties(notes)
        self.assertEqual(notes, expected)

    def test_invalid_tie_differing(self):
        notes = [Note(42, None, 120), Marker("tie"), Note(43, None, 120)]
        expected = notes[:]
        with catch_warnings(record=True) as w:
            simplefilter("always")
            simplify_ties(notes)
            self.assertEqual(notes, expected)
            self.assertEqual(len(w), 1)

    def test_invalid_tie_beginning(self):
        notes = [Marker("tie"), Note(43, None, 120)]
        expected = notes[:]
        with catch_warnings(record=True) as w:
            simplefilter("always")
            simplify_ties(notes)
            self.assertEqual(notes, expected)
            self.assertEqual(len(w), 1)

    def test_invalid_tie_end(self):
        notes = [Note(42, None, 120), Marker("tie")]
        expected = notes[:]
        with catch_warnings(record=True) as w:
            simplefilter("always")
            simplify_ties(notes)
            self.assertEqual(notes, expected)
            self.assertEqual(len(w), 1)
