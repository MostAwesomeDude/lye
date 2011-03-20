import unittest

from lye.grammar import LyeGrammar

class TestLyeGrammar(unittest.TestCase):

    def test_UNSIGNED(self):
        case = "12"
        grammar = LyeGrammar(case)
        result, error = grammar.apply("UNSIGNED")
        self.assertEqual(result, 12)

    def test_STRING(self):
        case = "test"
        grammar = LyeGrammar(case)
        result, error = grammar.apply("STRING")
        self.assertEqual(result, "test")

    def test_lilypond_invalid(self):
        case = "\\invalid"
        grammar = LyeGrammar(case)
        grammar.apply("lilypond")

    def test_lilypond_header(self):
        case = "\\header {}"
        grammar = LyeGrammar(case)
        result, error = grammar.apply("lilypond_header")
        self.assertEqual(result, [])

    def test_lilypond_header_hurp(self):
        case = "\\header { hurp = \"derp\" }"
        grammar = LyeGrammar(case)
        result, error = grammar.apply("lilypond_header")
        self.assertEqual(result, [])

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

    def test_chord_body(self):
        case = "<c4>"
        grammar = LyeGrammar(case)
        grammar.apply("chord_body")

    def test_command_event(self):
        case = "\\mark \\default"
        grammar = LyeGrammar(case)
        grammar.apply("command_event")

    def test_optional_rest(self):
        case = "\\rest"
        grammar = LyeGrammar(case)
        grammar.apply("optional_rest")

    def test_optional_rest_empty(self):
        case = ""
        grammar = LyeGrammar(case)
        grammar.apply("optional_rest")
