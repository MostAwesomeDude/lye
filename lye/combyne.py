from lye.ast import nt
from lye.instruments import NEAREST, fit, numbered_instruments
from lye.utilities import make_velocity
from lye.visitors import express_ast
from lye.visitors.folds import NoteScheduler, fold

class Bynder(nt("Bynder", "ast instrument")):
    """
    A snippet of lye, processed and prepared for export or combination with
    other snippets.

    Bynders contain all of the data necessary for specializing an AST, as well
    as the AST itself and any trimmings.
    """

    def specialized(self):
        ast = express_ast(self.ast, self.instrument)
        ast = fit(ast, self.instrument, NEAREST)
        return self._replace(ast=ast)

    def schedule(self):
        return fold(NoteScheduler, self.specialized().ast)

    def export_to_channel(self, channel, exporter):
        notes, elapsed = self.schedule()

        exporter.pc(channel, 0, numbered_instruments[self.instrument])

        for pitch, velocity, begin, duration in notes:
            if pitch == 0 and duration == 0:
                # Hax'd pitch bend data.
                exporter.bend(channel, begin, velocity)
            else:
                velocity = make_velocity(velocity)
                exporter.note(channel, begin, duration, pitch, velocity)
        return elapsed

    def export(self, mark, exporter):
        elapsed = self.export_to_channel(0, exporter)
        return elapsed, exporter.commit()


class Combyned(object):
    """
    Combine two Bynders to play them simultaneously.
    """

    def __init__(self, *bynders):
        self.bynders = bynders

    def export(self, mark, exporter):
        elapsed = 0
        for i, bynder in enumerate(self.bynders):
            elapsed = max(elapsed, bynder.export_to_channel(i, exporter))
        return elapsed, exporter.commit()
