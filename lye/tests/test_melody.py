import unittest

from pymeta.runtime import ParseError

from lye.grammar import LyeGrammar
from lye.tests.helpers import ParsingMixin

class TestSalsaSnippets(unittest.TestCase, ParsingMixin):

    def test_shady_snippet(self):
        snippet = """\\relative d'' {
            r2.. d8 |
            g4 fis b4. d,8 | g4 fis a4. d,8 | g4 fis e d | d2.. d8 |
            g4 fis b4. d,8 | g4 fis a4. d,8 | g4 fis e d | d2.. bes8 |
            d2.. c8 | d1
        }
        """
        self.assertParses(snippet, LyeGrammar, "expr")

    def test_shiny_snippet_one(self):
        snippet = """\\relative c' {
            c2 d4 e | b'2 a |
            r4 c, d e | g f c d | c1
        }
        """
        self.assertParses(snippet, LyeGrammar, "expr")

    def test_shiny_snippet_two(self):
        snippet = """\\relative b' {
            b2. g8 b | ais8. fis ais8 e8. a e8 |
            g2. d8 g | eis8. cis eis8 c8. e c8 |
            d1 | dis8. b dis8 c8. es c8 | b1
        }
        """
        self.assertParses(snippet, LyeGrammar, "expr")
