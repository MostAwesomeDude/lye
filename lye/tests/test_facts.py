from unittest import TestCase

from lye.facts import can_legato

class TestCanLegato(TestCase):

    def test_piano_cannot_legato(self):
        self.assertFalse(can_legato("acoustic grand"))

    def test_guitar_can_legato(self):
        self.assertTrue(can_legato("overdriven guitar"))
