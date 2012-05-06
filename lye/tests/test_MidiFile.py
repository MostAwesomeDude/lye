from unittest import TestCase

from lye.MidiFile import writeVarLength

class TestWriteVarLength(TestCase):

    def test_zero(self):
        i = 0x0
        o = "\x00"
        self.assertEqual(writeVarLength(i), o)

    def test_single(self):
        i = 0xc8
        o = "\x81\x48"
        self.assertEqual(writeVarLength(i), o)

    def test_multiple(self):
        i = 0x100000
        o = "\xc0\x80\x00"
        self.assertEqual(writeVarLength(i), o)
