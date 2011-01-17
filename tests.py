import unittest

import lye

class Test(unittest.TestCase):

    def test_note(self):
        note = lye.LyGrammar("c1").apply("note")

    def test_measure(self):
        measure = lye.LyGrammar("c e g c' |").apply("measure")

    def test_measures(self):
        measures = lye.LyGrammar("e4 d c2 | e4 d c2 |").apply("measures")
