import unittest

from lye.grammar2 import LyGrammar

class TestNote(unittest.TestCase):

    def test_note(self):
        note, error = LyGrammar("c").apply("note")
        self.assertEqual(note.pitch, 48)
        self.assertEqual(note.duration, 120)

    def test_whole_note(self):
        note, error = LyGrammar("c1").apply("note")
        self.assertEqual(note.duration, 480)

    def test_es(self):
        note, error = LyGrammar("es").apply("note")
        self.assertEqual(note.pitch, 51)

    def test_note_spaces(self):
        note, error = LyGrammar(" c ").apply("note")
        self.assertEqual(note.pitch, 48)

    def test_note_modified_spaces(self):
        note, error = LyGrammar(" c'2. ").apply("note")
        self.assertEqual(note.pitch, 60)
        self.assertEqual(note.duration, 360)

    def test_rest(self):
        note, error = LyGrammar("r").apply("note")
        self.assertEqual(note.pitch, -1)

    def test_dots(self):
        note, error = LyGrammar("c4.").apply("note")
        self.assertEqual(note.duration, 180)

        note, error = LyGrammar("c4..").apply("note")
        self.assertEqual(note.duration, 270)

class TestNotes(unittest.TestCase):

    def test_notes_spaced(self):
        notes, error = LyGrammar("c4 d e  d  c").apply("notes")
        for note, pitch in zip(notes, (48, 50, 52, 50, 48)):
            self.assertEqual(note.pitch, pitch)
            self.assertEqual(note.duration, 120)

class TestChords(unittest.TestCase):

    def test_chord(self):
        chord, error = LyGrammar("<c e g>").apply("chord")
        self.assertEqual(chord.pitches, [48, 52, 55])
        self.assertEqual(chord.duration, 120)

    def test_chord_duration(self):
        chord, error = LyGrammar("<c1 e g>").apply("chord")
        self.assertEqual(chord.duration, 480)

    def test_chord_spaces(self):
        chord, error = LyGrammar(" < c e g > ").apply("chord")
        self.assertEqual(chord.pitches, [48, 52, 55])
        self.assertEqual(chord.duration, 120)

class TestCommands(unittest.TestCase):

    def test_relative(self):
        string, error = LyGrammar(" \\relative").apply("relative")
        self.assertEqual(string, "\\relative")

class TestMelody(unittest.TestCase):

    def test_melody_marker(self):
        melody, error = LyGrammar("c e g c' |").apply("melody")
        self.assertEqual(melody.notes[0], (48, 0, 120))
        self.assertEqual(melody.notes[1], (52, 120, 240))
        self.assertEqual(melody.notes[2], (55, 240, 360))
        self.assertEqual(melody.notes[3], (60, 360, 480))

    def test_melody_multiple_markers(self):
        melody, error = LyGrammar("e4 d c2 | e4 d c2 |").apply("melody")

    def test_melody_rests(self):
        melody, error = LyGrammar("f4 a b r | f4 a b r").apply("melody")
        self.assertEqual(melody.notes[3], (53, 480, 600))

class TestMarker(unittest.TestCase):

    def test_marker_identity(self):
        grammar = LyGrammar("|")
        marker, error = grammar.apply("marker")
        self.assertTrue(marker is grammar.marker)

class TestSalsaSnippets(unittest.TestCase):

    def test_shady_snippet(self):
        snippet = """\\relative d'' {
            r2.. d8 |
            g4 fis b4. d,8 | g4 fis a4. d,8 | g4 fis e d | d2.. d8 |
            g4 fis b4. d,8 | g4 fis a4. d,8 | g4 fis e d | d2.. bes8 |
            d2.. c8 | d1
        }
        """
        print LyGrammar(snippet).apply("melody")[0]

    def test_shiny_snippet_one(self):
        snippet = """\\relative c' {
            c2 d4 e | b'2 a |
            r4 c, d e | g f c d | c1
        }
        """
        LyGrammar(snippet).apply("melody")

    def test_shiny_snippet_two(self):
        snippet = """\\relative b' {
            b2. g8 b | ais8. fis ais8 e8. a e8 |
            g2. d8 g | eis8. cis eis8 c8. e c8 |
            d1 | dis8. b dis8 c8. es c8 | b1
        }
        """
        LyGrammar(snippet).apply("melody")
