"""
AST transformations which fold up an AST into a single value.
"""

from lye.visitors.visitor import Visitor


class ChordCounter(Visitor):
    """
    Determine the longest Chord in an expression.
    """

    length = 0

    def visit_Chord(self, chord):
        self.length = max(self.length, len(chord.notes))
        return chord, False
