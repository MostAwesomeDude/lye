from lye.visitors.maps import (ChordSorter, DrumsTransformer, DurationVisitor,
                               DynamicRemover, MusicFlattener,
                               NoteTransformer, Relativizer, TimesVisitor,
                               VoicesTransformer)
from lye.visitors.peephole import TieRemover

stages = (
    DrumsTransformer,
    VoicesTransformer,
    DurationVisitor,
    TimesVisitor,
    Relativizer,
    MusicFlattener,
    NoteTransformer,
    DynamicRemover,
    ChordSorter,
    TieRemover,
)

def simplify_ast(ast):
    for stage in stages:
        ast = stage().visit(ast)
    return ast
