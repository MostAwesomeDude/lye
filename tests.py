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

class TestMelody(unittest.TestCase):

    def test_melody_marker(self):
        melody, error = lye.LyGrammar("c e g c' |").apply("melody")
        self.assertEqual(melody.notes[0], (48, 0, 120))
        self.assertEqual(melody.notes[1], (52, 120, 240))
        self.assertEqual(melody.notes[2], (55, 240, 360))
        self.assertEqual(melody.notes[3], (60, 360, 480))

    def test_melody_multiple_markers(self):
        melody, error = lye.LyGrammar("e4 d c2 | e4 d c2 |").apply("melody")

class TestChords(unittest.TestCase):

    def test_chord(self):
        chord, error = lye.LyGrammar("<c e g>").apply("chord")
        self.assertEqual(chord.pitches, [48, 52, 55])
        self.assertEqual(chord.duration, 120)

    def test_chord_duration(self):
        chord, error = lye.LyGrammar("<c1 e g>").apply("chord")
        self.assertEqual(chord.duration, 480)

class TestMarker(unittest.TestCase):

    def test_marker_identity(self):
        grammar = lye.LyGrammar("|")
        marker, error = grammar.apply("marker")
        self.assertTrue(marker is grammar.marker)
