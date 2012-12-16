import unittest

from lye.ast import Duration, Rest, SciNote
from lye.grammar import LyeGrammar

class TestPrimitives(unittest.TestCase):

    def test_int(self):
        i = LyeGrammar("456").int()
        self.assertEqual(i, 456)

    def test_pitch_es(self):
        pitch = LyeGrammar("es").pitch()
        self.assertEqual(pitch, "es")

    def test_flat(self):
        accidental = LyeGrammar("es").accidental()
        self.assertEqual(accidental, -1)

    def test_sharp(self):
        accidental = LyeGrammar("is").accidental()
        self.assertEqual(accidental, 1)

    def test_sharp_repeated(self):
        accidental = LyeGrammar("isis").accidental()
        self.assertEqual(accidental, 2)

class TestNote(unittest.TestCase):

    def test_rest(self):
        rest = LyeGrammar("r").expr_rest()
        self.assertEqual(rest, Rest(None))

    def test_note(self):
        note = LyeGrammar("c").expr_note()
        self.assertEqual(note.pitch, "c")

    def test_whole_note(self):
        note = LyeGrammar("c1").expr_note()
        self.assertEqual(note.duration, Duration(1, 0))

    def test_es(self):
        note = LyeGrammar("es").expr_note()
        self.assertEqual(note.pitch, "es")

    def test_dotted(self):
        note = LyeGrammar("c4.").expr_note()
        self.assertEqual(note.duration, Duration(4, 1))

    def test_double_dotted(self):
        note = LyeGrammar("c4..").expr_note()
        self.assertEqual(note.duration, Duration(4, 2))

class TestDrums(unittest.TestCase):

    def test_kit(self):
        k = LyeGrammar("bd").kit()
        self.assertEqual(k, "bd")

    def test_drum_mode(self):
        expr = LyeGrammar("\\drums { sn }").expr_drums()
        self.assertEqual(len(expr.expr.exprs), 1)
        self.assertEqual(expr.expr.exprs[0], SciNote(40, None, None, None))

class TestChords(unittest.TestCase):

    def test_chord(self):
        chord = LyeGrammar("<c e g>").expr_chord()

    def test_chord_duration(self):
        chord = LyeGrammar("<c1 e g>").expr_chord()

    def test_chord_spaces(self):
        chord = LyeGrammar("< c e g >").expr_chord()
