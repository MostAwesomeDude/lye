import unittest

import lye

class TestNote(unittest.TestCase):

    def test_note(self):
        note, error = lye.LyGrammar("c").apply("note")
        self.assertEqual(note.pitch, 48)
        self.assertEqual(note.duration, 120)

    def test_whole_note(self):
        note, error = lye.LyGrammar("c1").apply("note")
        self.assertEqual(note.duration, 480)

    def test_es(self):
        note, error = lye.LyGrammar("es").apply("note")
        self.assertEqual(note.pitch, 51)

class TestCommands(unittest.TestCase):

    def test_relative(self):
        string, error = lye.LyGrammar(" \\relative").apply("relative")
        self.assertEqual(string, "\\relative")

class Test(unittest.TestCase):

    def test_measure(self):
        measure = lye.LyGrammar("c e g c' |").apply("measure")

    def test_measures(self):
        measures = lye.LyGrammar("e4 d c2 | e4 d c2 |").apply("measures")
