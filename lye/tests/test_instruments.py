from unittest import TestCase

from lye.instruments import can_legato, sci_to_midi

class TestSciToMidi(TestCase):

    def test_middle_c(self):
        self.assertEqual(sci_to_midi("C4"), 60)

    def test_a0(self):
        self.assertEqual(sci_to_midi("A0"), 21)

    def test_c8(self):
        self.assertEqual(sci_to_midi("C8"), 108)

    def test_flat(self):
        self.assertEqual(sci_to_midi("Cb4"), 59)

class TestCanLegato(TestCase):

    def test_piano_cannot_legato(self):
        self.assertFalse(can_legato("acoustic grand"))

    def test_guitar_can_legato(self):
        self.assertTrue(can_legato("overdriven guitar"))
