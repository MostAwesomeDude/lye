import unittest

from lye.melody import melody_from_ly

class TestMelody(unittest.TestCase):

    def test_melody_marker(self):
        melody = melody_from_ly("c e g c' |")
        self.assertEqual(melody.notes[0], (48, 0, 120))
        self.assertEqual(melody.notes[1], (52, 120, 240))
        self.assertEqual(melody.notes[2], (55, 240, 360))
        self.assertEqual(melody.notes[3], (60, 360, 480))

    def test_melody_multiple_markers(self):
        melody_from_ly("e4 d c2 | e4 d c2 |")

    def test_melody_rests(self):
        melody = melody_from_ly("f4 a b r | f4 a b r")
        self.assertEqual(melody.notes[3], (53, 480, 600))

class TestSalsaSnippets(unittest.TestCase):

    def test_shady_snippet(self):
        snippet = """\\relative d'' {
            r2.. d8 |
            g4 fis b4. d,8 | g4 fis a4. d,8 | g4 fis e d | d2.. d8 |
            g4 fis b4. d,8 | g4 fis a4. d,8 | g4 fis e d | d2.. bes8 |
            d2.. c8 | d1
        }
        """
        melody_from_ly(snippet)

    def test_shiny_snippet_one(self):
        snippet = """\\relative c' {
            c2 d4 e | b'2 a |
            r4 c, d e | g f c d | c1
        }
        """
        melody_from_ly(snippet)

    def test_shiny_snippet_two(self):
        snippet = """\\relative b' {
            b2. g8 b | ais8. fis ais8 e8. a e8 |
            g2. d8 g | eis8. cis eis8 c8. e c8 |
            d1 | dis8. b dis8 c8. es c8 | b1
        }
        """
        melody_from_ly(snippet)
