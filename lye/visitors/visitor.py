"""
AST visitor.
"""

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
