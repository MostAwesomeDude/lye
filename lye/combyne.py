from lye.ast import nt
from lye.instruments import NEAREST, fit
from lye.visitors import express_ast
from lye.visitors.folds import NoteScheduler, fold

# XXX duped
def make_velocity(s):
    l = "pp p mp mf f ff".split()
    i = l.index(s)
    i = i * 18 + 19
    return i

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
