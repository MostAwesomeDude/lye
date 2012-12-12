from fractions import gcd

from lye.ast import nt
from lye.instruments import NEAREST, fit, numbered_instruments
from lye.utilities import make_velocity
from lye.visitors import express_ast
from lye.visitors.folds import NoteScheduler, fold


def multipliers(numbers):
    denominator = reduce(gcd, numbers)
    numerator = max(numbers) // denominator
    lcm = denominator * numerator
    return [lcm // number for number in numbers]


class _Bynd(object):
    """
    Common functionality.
    """

    repeat = 1

    def __mul__(self, other):
        bound = self._replace()
        bound.repeat = self.repeat * other
        return bound

    def __or__(self, other):
        return Combyned(self, other)

    def __and__(self, other):
        mine = self.length() * self.repeat
        theirs = other.length() * self.repeat
        mine, theirs = multipliers((mine, theirs))

        self = self * mine
        other = other * theirs

        return Combyned(self, other)

    def length(self):
        return self.schedule()[1] * self.repeat

    def begin(self):
        return self.schedule()[0][0].begin


class Bynder(_Bynd, nt("Bynder", "ast instrument")):
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
        notes, length = self.schedule()
        elapsed = 0

        exporter.pc(channel, self.begin(),
                numbered_instruments[self.instrument])

        for i in range(self.repeat):
            for pitch, velocity, begin, duration in notes:
                begin += elapsed

                if pitch == 0 and duration == 0:
                    # Hax'd pitch bend data.
                    exporter.bend(channel, begin, velocity)
                else:
                    velocity = make_velocity(velocity)
                    exporter.note(channel, begin, duration, pitch, velocity)
            elapsed += length
        return elapsed

    def export(self, exporter):
        elapsed = self.export_to_channel(0, exporter)
        return elapsed, exporter.commit()


class Drumlyne(_Bynd, nt("Drumlyne", "ast")):

    def schedule(self):
        return fold(NoteScheduler, self.ast)

    def export_to_channel(self, channel, exporter):
        notes, length = self.schedule()
        elapsed = 0

        for i in range(self.repeat):
            # Ignore all channels; drums are always on channel 9.
            for pitch, velocity, begin, duration in notes:
                begin += elapsed

                velocity = make_velocity(velocity)
                exporter.note(9, begin, duration, pitch, velocity)
            elapsed += length
        return elapsed

    def export(self, exporter):
        elapsed = self.export_to_channel(9, exporter)
        return elapsed, exporter.commit()


class Combyned(object):
    """
    Combine two or more Bynders to play them simultaneously.
    """

    def __init__(self, *bynders):
        self.bynders = bynders

    def __or__(self, other):
        if isinstance(other, Combyned):
            return Combyned(*(self.bynders + other.bynders))
        elif isinstance(other, _Bynd):
            return Combyned(other, *self.bynders)
        else:
            raise RuntimeError("Can't combyne %r and %r" % (self, other))

    def __ror__(self, other):
        if isinstance(other, Combyned):
            self.bynders += other.bynders
        elif isinstance(other, _Bynd):
            self.bynders += (other,)
        else:
            raise RuntimeError("Can't combyne %r and %r" % (self, other))

    def __and__(self, other):
        bynders = self.bynders

        if isinstance(other, Combyned):
            bynders += other.bynders
        elif isinstance(other, _Bynd):
            bynders += (other,)
        else:
            raise RuntimeError("Can't combyne %r and %r" % (self, other))

        ms = multipliers([b.length() for b in bynders])
        return Combyned(*(b * m for (b, m) in zip(bynders, ms)))

    def __rand__(self, other):
        bynders = self.bynders

        if isinstance(other, Combyned):
            bynders += other.bynders
        elif isinstance(other, _Bynd):
            bynders += (other,)
        else:
            raise RuntimeError("Can't combyne %r and %r" % (self, other))

        ms = multipliers([b.length() for b in bynders])
        self.bynders = tuple(*(b * m for (b, m) in zip(bynders, ms)))

    def begin(self):
        return min(b.begin() for b in self.bynders)

    def export(self, exporter):
        elapsed = 0
        for i, bynder in enumerate(self.bynders):
            elapsed = max(elapsed, bynder.export_to_channel(i, exporter))
        return elapsed, exporter.commit()
