import unittest

from lye.ast import Duration, SciNote
from lye.grammar import LyeGrammar
from lye.tests.helpers import ParsingMixin

class TestPrimitives(unittest.TestCase):

    def test_int(self):
        i, error = LyeGrammar("456").apply("int")
        self.assertEqual(i, 456)

    def test_pitch_es(self):
        pitch, error = LyeGrammar("es").apply("pitch")
        self.assertEqual(pitch, "es")

    def test_flat(self):
        accidental, error = LyeGrammar("es").apply("accidental")
        self.assertEqual(accidental, -1)

    def test_sharp(self):
        accidental, error = LyeGrammar("is").apply("accidental")
        self.assertEqual(accidental, 1)

    def test_sharp_repeated(self):
        accidental, error = LyeGrammar("isis").apply("accidental")
        self.assertEqual(accidental, 2)

class TestNote(unittest.TestCase, ParsingMixin):

    def test_rest(self):
        self.assertParses("r", LyeGrammar, "expr_rest")

    def test_note(self):
        note = self.assertParses("c", LyeGrammar, "expr_note")
        self.assertEqual(note.pitch, "c")

    def test_whole_note(self):
        note = self.assertParses("c1", LyeGrammar, "expr_note")
        self.assertEqual(note.duration, Duration(1, 0))

    def test_es(self):
        note = self.assertParses("es", LyeGrammar, "expr_note")
        self.assertEqual(note.pitch, "es")

    def test_dotted(self):
        note = self.assertParses("c4.", LyeGrammar, "expr_note")
        self.assertEqual(note.duration, Duration(4, 1))

    def test_double_dotted(self):
        note = self.assertParses("c4..", LyeGrammar, "expr_note")
        self.assertEqual(note.duration, Duration(4, 2))

class TestDrums(unittest.TestCase, ParsingMixin):

    def test_kit(self):
        k = self.assertParses("bd", LyeGrammar, "kit")
        self.assertEqual(k, "bd")

    def test_drum_mode(self):
        expr = self.assertParses("\\drums { sn }", LyeGrammar, "expr_drums")
        self.assertEqual(len(expr.expr.exprs), 1)
        self.assertEqual(expr.expr.exprs[0], SciNote(40, None, None))

class TestChords(unittest.TestCase, ParsingMixin):

    def test_chord(self):
        chord = self.assertParses("<c e g>", LyeGrammar, "expr_chord")

    def test_chord_duration(self):
        chord = self.assertParses("<c1 e g>", LyeGrammar, "expr_chord")

    def test_chord_spaces(self):
        chord = self.assertParses("< c e g >", LyeGrammar, "expr_chord")
