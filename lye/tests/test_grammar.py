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

    def test_context_def_mod(self):
        case = "\\consists"
        grammar = LyeGrammar(case)
        grammar.apply("context_def_mod")

    def test_command_event(self):
        case = "\\mark \\default"
        grammar = LyeGrammar(case)
        grammar.apply("command_event")
