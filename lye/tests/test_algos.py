import unittest
from warnings import catch_warnings, simplefilter

from lye.algos import pitch_to_number, simplify_ties
from lye.ast import TIE, SciNote

class TestPitchToNumber(unittest.TestCase):

    def test_middle_c(self):
        self.assertEqual(pitch_to_number("c", 0, 0), 48)

class TestSimplifyTies(unittest.TestCase):

    def test_no_ties(self):
        notes = [SciNote(42, 120, None), SciNote(42, 120, None)]
        expected = notes[:]
        simplify_ties(notes)
        self.assertEqual(notes, expected)

    def test_valid_tie(self):
        notes = [SciNote(42, 120, None), TIE, SciNote(42, 120, None)]
        expected = [SciNote(42, 240, None)]
        simplify_ties(notes)
        self.assertEqual(notes, expected)

    def test_invalid_tie_differing(self):
        notes = [SciNote(42, 120, None), TIE, SciNote(43, 120, None)]
        expected = notes[:]
        with catch_warnings(record=True) as w:
            simplefilter("always")
            simplify_ties(notes)
            self.assertEqual(notes, expected)
            self.assertEqual(len(w), 1)

    def test_invalid_tie_beginning(self):
        notes = [TIE, SciNote(43, 120, None)]
        expected = notes[:]
        with catch_warnings(record=True) as w:
            simplefilter("always")
            simplify_ties(notes)
            self.assertEqual(notes, expected)
            self.assertEqual(len(w), 1)

    def test_invalid_tie_end(self):
        notes = [SciNote(42, 120, None), TIE]
        expected = notes[:]
        with catch_warnings(record=True) as w:
            simplefilter("always")
            simplify_ties(notes)
            self.assertEqual(notes, expected)
            self.assertEqual(len(w), 1)
