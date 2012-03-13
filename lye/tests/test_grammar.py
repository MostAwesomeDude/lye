import unittest

from pymeta.runtime import ParseError

from lye.grammar import LyGrammar

class ParsingMixin(object):

    def assertParses(self, data, grammar, rule):
        try:
            return grammar(data).apply(rule)[0]
        except ParseError, pe:
            assert False, pe.formatError(data)

class TestPrimitives(unittest.TestCase):

    def test_int(self):
        i, error = LyGrammar("456").apply("int")
        self.assertEqual(i, 456)

    def test_pitch_es(self):
        pitch, error = LyGrammar("es").apply("pitch")
        self.assertEqual(pitch, "es")

    def test_flat(self):
        accidental, error = LyGrammar("es").apply("accidental")
        self.assertEqual(accidental, "es")

    def test_sharp(self):
        accidental, error = LyGrammar("is").apply("accidental")
        self.assertEqual(accidental, "is")

    def test_sharp_repeated(self):
        accidental, error = LyGrammar("isis").apply("accidental")
        self.assertEqual(accidental, "isis")

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

    def test_dotted(self):
        note, error = LyGrammar("c4.").apply("note")
        self.assertEqual(note.duration, 180)

    def test_double_dotted(self):
        note, error = LyGrammar("c4..").apply("note")
        self.assertEqual(note.duration, 210)

class TestDrums(unittest.TestCase, ParsingMixin):

    def test_kit(self):
        k = self.assertParses("bd", LyGrammar, "kit")
        self.assertEqual(k, "bd")

    def test_drum_raw_negative(self):
        self.assertRaises(ParseError, LyGrammar("bd").apply, "drum")

    def test_drum_raw(self):
        g = LyGrammar("bd")
        g.drums = True
        k, error = g.apply("drum")
        self.assertEqual(k, (36, None, 120))

    def test_drum_mode(self):
        m = self.assertParses("\\drums { sn }", LyGrammar, "melody")
        self.assertEqual(len(m), 1)
        self.assertEqual(m[0], (40, None, 120))

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

    def test_chord_protonote(self):
        chord, error = LyGrammar("<c e g>").apply("protonote")
        self.assertEqual(chord.pitches, [48, 52, 55])
        self.assertEqual(chord.duration, 120)

    def test_chord_melody_protonote(self):
        chords, error = LyGrammar("<c e g> <d f a>").apply("melody")
        self.assertEqual(chords[0].pitches, [48, 52, 55])
        self.assertEqual(chords[0].duration, 120)
        self.assertEqual(chords[1].pitches, [50, 53, 57])
        self.assertEqual(chords[1].duration, 120)

    def test_chord_melody_protonote_duration(self):
        chords, error = LyGrammar("<c8 e g> <d16 f a>").apply("melody")
        self.assertEqual(chords[0].pitches, [48, 52, 55])
        self.assertEqual(chords[0].duration, 60)
        self.assertEqual(chords[1].pitches, [50, 53, 57])
        self.assertEqual(chords[1].duration, 30)

class TestCommands(unittest.TestCase):

    def test_relative(self):
        string, error = LyGrammar("\\relative").apply("relative")
        self.assertEqual(string, "\\relative")

    def test_relative_leading_space(self):
        string, error = LyGrammar(" \\relative").apply("relative")
        self.assertEqual(string, "\\relative")

    def test_times(self):
        string, error = LyGrammar("\\times").apply("times")
        self.assertEqual(string, "\\times")

class TestMarker(unittest.TestCase):

    def test_measure_name(self):
        grammar = LyGrammar("|")
        marker, error = grammar.apply("marker")
        self.assertEqual(marker.name, "measure")

    def test_partial_name(self):
        grammar = LyGrammar("\\partial")
        marker, error = grammar.apply("marker")
        self.assertEqual(marker.name, "partial")

class TestDirectives(unittest.TestCase, ParsingMixin):

    def test_times_melody(self):
        notes = self.assertParses("\\times 2/3 { c8 d c }", LyGrammar,
            "melody")
        self.assertEqual(notes[0].duration, 40)
