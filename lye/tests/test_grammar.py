import unittest

from lye.grammar import LyeGrammar

class TestLyeGrammar(unittest.TestCase):

    def test_lilypond_invalid(self):
        case = "\\invalid"
        grammar = LyeGrammar(case)
        grammar.apply("lilypond")
