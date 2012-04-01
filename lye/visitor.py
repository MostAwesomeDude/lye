from fractions import Fraction
from operator import mul

from lye.algos import pitch_to_number, simplify_ties
from lye.ast import Duration, Music, SciNote

def hasfield(obj, name):
    return hasattr(obj, "_fields") and name in obj._fields

class Visitor(object):
    """
    An object that visits every node in an AST.
    """

    def visit_generic(self, node):
        """
        Visit a node with no type information.

        This method is a good point for polymorphic or duck-typed
        transformations.
        """

        return node, True

    def visit(self, node):
        """
        Recursively visit every node in an AST.
        """

        method = getattr(self, "visit_%s" % node.__class__.__name__,
            self.visit_generic)
        node, recurse = method(node)

        if recurse and hasfield(node, "expr"):
            node = node._replace(expr=self.visit(node.expr))

        if recurse and hasfield(node, "exprs"):
            exprs = [self.visit(expr) for expr in node.exprs]
            node = node._replace(exprs=exprs)

        if recurse and hasfield(node, "notes"):
            notes = [self.visit(note) for note in node.notes]
            node = node._replace(notes=notes)

        return node

class DurationVisitor(Visitor):
    """
    Tool for filling out durations in a Lye AST.
    """

    tpb = 120
    duration = Duration(4, 0)

    def undot_duration(self, duration, dots):
        """
        Turn duration into a number of ticks, and apply dots, if any.
        """

        # Multiply ticks per beat by four since ly assumes that "1" is a whole
        # note, not a quarter note, but beats are quarter notes.
        duration = self.tpb * 4 // duration

        dotted = duration
        while dots:
            dotted /= 2
            duration += dotted
            dots -= 1
        return duration

    def fill_duration(self, d):
        """
        Fill durations forward.
        """

        if d is not None:
            self.duration = d
        return self.duration

    def visit_generic(self, ast):
        """
        Simplify and fill in durations along an AST.
        """

        if hasfield(ast, "duration"):
            duration = self.fill_duration(ast.duration)
            duration = self.undot_duration(duration.length, duration.dots)
            ast = ast._replace(duration=duration)

        return ast, True

class TimesVisitor(Visitor):
    """
    Apply Times nodes and turn them into Music nodes.
    """

    def __init__(self):
        self.tuplets = [Fraction(1, 1)]
        self.refresh_scale()

    def refresh_scale(self):
        self.scale = reduce(mul, self.tuplets)

    def push_tuplet(self, tuplet):
        self.tuplets.append(tuplet)
        self.refresh_scale()

    def pop_tuplet(self):
        retval = self.tuplets.pop()
        self.refresh_scale()
        return retval

    def visit_Times(self, times):
        """
        Turn Times into Music.
        """

        self.push_tuplet(times.fraction)
        node = self.visit(Music([times.expr]))
        self.pop_tuplet()
        return node, False

    def visit_generic(self, ast):
        """
        Apply Times across Durations.
        """

        if hasfield(ast, "duration") and self.scale != Fraction(1, 1):
            duration = int(ast.duration * self.scale)
            ast = ast._replace(duration=duration)

        return ast, True

class Relativizer(Visitor):
    """
    Apply Relative octaves to Notes.
    """

    previous = None

    relative_dict = dict(zip("cdefgab", range(7)))
    relative_dict["es"] = relative_dict["e"]

    def visit_Note(self, note):
        if self.previous:
            # Unpack the note, relativize it, repack it.
            ppitch, poctave = self.previous
            octave = note.octave + poctave

            if abs(self.relative_dict[ppitch] -
                self.relative_dict[note.pitch]) > 3:
                if self.relative_dict[note.pitch] > 4:
                    octave -= 1
                else:
                    octave += 1

            self.previous = note.pitch, octave
            note = note._replace(octave=octave)

        return note, True

    def visit_Relative(self, relative):
        if self.previous:
            raise Exception("Nested Relative nodes!")

        self.previous = relative.pitch, relative.octave
        expr = self.visit(relative.expr)
        self.previous = None

        return expr, False

class MusicFlattener(Visitor):
    """
    Flatten Music expressions.
    """

    def visit_Music(self, music):
        # Flatten singleton Music expressions.
        while len(music.exprs) == 1:
            music = music.exprs[0]
        # Inline Music within Music.
        flattening = True
        while flattening:
            flattening = False
            exprs = []
            for expr in music.exprs:
                if isinstance(expr, Music):
                    exprs.extend(expr.exprs)
                    flattening = True
                else:
                    exprs.append(expr)
            music = music._replace(exprs=exprs)
        return music, True

class NoteTransformer(Visitor):
    """
    Turn Notes into SciNotes.
    """

    def visit_Note(self, note):
        number = pitch_to_number(note.pitch, note.accidental, note.octave)
        return SciNote(number, note.duration), True

class TieRemover(Visitor):
    """
    Find and remove ties from streams of notes.
    """

    def visit_generic(self, node):
        if hasfield(node, "exprs"):
            simplify_ties(node.exprs)
        return node, True

def simplify_ast(ast):
    ast = DurationVisitor().visit(ast)
    ast = TimesVisitor().visit(ast)
    ast = Relativizer().visit(ast)
    ast = MusicFlattener().visit(ast)
    ast = NoteTransformer().visit(ast)
    ast = TieRemover().visit(ast)
    return ast
