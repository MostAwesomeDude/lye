from lye.ast import nt
from lye.instruments import NEAREST, fit, numbered_instruments
from lye.utilities import find_instrument, make_velocity
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

        instrument = find_instrument(self.instrument)
        exporter.pc(channel, 0, numbered_instruments[instrument])

        for pitch, velocity, begin, duration in notes:
            if pitch == 0 and duration == 0:
                # Hax'd pitch bend data.
                exporter.bend(channel, begin, velocity)
            else:
                velocity = make_velocity(velocity)
                exporter.note(channel, begin, duration, pitch, velocity)
        return elapsed, exporter.commit()

    def export(self, mark, exporter):
        return self.export_to_channel(0, exporter)
