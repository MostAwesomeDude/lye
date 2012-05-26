import unittest
from warnings import catch_warnings, simplefilter

from lye.algos import pitch_to_number
from lye.ast import TIE, SciNote

class TestPitchToNumber(unittest.TestCase):

    def test_middle_c(self):
        self.assertEqual(pitch_to_number("c", 0, 0), 48)
