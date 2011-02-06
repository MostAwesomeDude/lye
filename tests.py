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

class TestNotes(unittest.TestCase):

    def test_notes(self):
        notes, error = lye.LyGrammar("c4 d e  d  c").apply("notes")
        for note, pitch in zip(notes, (48, 50, 52, 50, 48)):
            self.assertEqual(note.pitch, pitch)
            self.assertEqual(note.duration, 120)

class TestCommands(unittest.TestCase):

    def test_relative(self):
        string, error = lye.LyGrammar(" \\relative").apply("relative")
        self.assertEqual(string, "\\relative")

class TestMeasure(unittest.TestCase):

    def test_measure(self):
        measure = lye.LyGrammar("c e g c' |").apply("measure")

class TestMeasures(unittest.TestCase):

    def test_measures(self):
        measures = lye.LyGrammar("e4 d c2 | e4 d c2 |").apply("measures")

class TestChords(unittest.TestCase):

    def test_chord(self):
        chord, error = lye.LyGrammar("<c e g>").apply("chord")
        self.assertEqual(chord.pitches, [48, 52, 55])
        self.assertEqual(chord.duration, 120)
