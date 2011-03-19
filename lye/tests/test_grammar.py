import unittest

from lye.grammar import LyeGrammar

class TestLyeGrammar(unittest.TestCase):

    def test_lilypond_invalid(self):
        case = "\\invalid"
        grammar = LyeGrammar(case)
        grammar.apply("lilypond")

    def test_mode_changing_head(self):
        case = "\\drummode"
        grammar = LyeGrammar(case)
        grammar.apply("mode_changing_head")

    def test_mode_changing_head_with_context(self):
        case = "\\chords"
        grammar = LyeGrammar(case)
        grammar.apply("mode_changing_head_with_context")
