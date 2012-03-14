from unittest import TestCase

from lye.MidiFile import writeVarLength

class TestWriteVarLength(TestCase):

    def test_zero(self):
        i = 0x0
        o = [0x0]
        self.assertEqual(writeVarLength(i), o)

    def test_single(self):
        i = 0xc8
        o = [0x81, 0x48]
        self.assertEqual(writeVarLength(i), o)

    def test_multiple(self):
        i = 0x100000
        o = [0xc0, 0x80, 0x00]
        self.assertEqual(writeVarLength(i), o)
