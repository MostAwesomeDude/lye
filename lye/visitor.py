from lye.ast import Duration

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

        return node

    def visit(self, node):
        """
        Recursively visit every node in an AST.
        """

        method = getattr(self, "visit_%s" % node.__class__.__name__,
            self.visit_generic)
        node = method(node)

        if "expr" in node._fields:
            node = node._replace(expr=self.visit(node.expr))

        if "exprs" in node._fields:
            exprs = [self.visit(expr) for expr in node.exprs]
            node = node._replace(exprs=exprs)

        return node

class DurationWalker(Visitor):
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

        if "duration" in ast._fields:
            duration = self.fill_duration(ast.duration)
            duration = self.undot_duration(duration.length, duration.dots)
            ast = ast._replace(duration=duration)

        return ast
