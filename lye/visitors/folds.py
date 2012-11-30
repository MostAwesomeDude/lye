"""
AST transformations which fold up an AST into a single value.
"""

from lye.ast import nt

from lye.visitors.visitor import Visitor

ScheduledNote = nt("ScheduledNote", "pitch, velocity, begin, duration")

class Foldable(Visitor):
    """
    A visitor that can be folded to produce a single value.
    """

    result = None

def fold(foldable, ast, *args, **kwargs):
    f = foldable(*args, **kwargs)
    f.visit(ast)
    return f.result

class ChordCounter(Foldable):
    """
    Determine the longest Chord in an expression.
    """

    result = 0

    def visit_Chord(self, chord):
        self.result = max(self.result, len(chord.notes))
        return chord, False

class NoteScheduler(Foldable):
    """
    Attach correct beginning times to notes in an AST node.

    Rests are discarded. Chords are broken up. Voices are recursed and
    correctly added.
    """

    marker = 0
    offset = 0
    previous = 0

    def __init__(self):
        self.notes = []

    @property
    def result(self):
        return self.notes, self.marker

    def visit_Chord(self, chord):
        begin = self.marker
        self.marker = begin + chord.notes[0].duration

        for note in chord.notes:
            self.notes.append(ScheduledNote(note.pitch, note.velocity, begin,
                note.duration))
        return chord, False

    def visit_SciNote(self, scinote):
        self.previous = begin = self.marker
        self.marker = begin + scinote.duration

        self.notes.append(ScheduledNote(scinote.pitch, scinote.velocity,
            begin, scinote.duration))
        return scinote, False

    def visit_PitchBend(self, pitchbend):
        self.notes.append((0, pitchbend.value,
            self.previous + pitchbend.offset, 0))
        return pitchbend, False

    def visit_Rest(self, rest):
        self.marker += rest.duration
        return rest, False

    def visit_Voices(self, voices):
        for voice in voices.exprs:
            inner_scheduled, inner_relative = fold(NoteScheduler, voice)
            self.marker = max(inner_relative, self.marker)
            self.notes.extend(inner_scheduled)
        return voices, False
