from lye.visitors.maps import (ChordSorter, DrumsTransformer, DurationVisitor,
                               DynamicRemover, Express, MusicFlattener,
                               NoteTransformer, Relativizer, TimesVisitor,
                               VoicesTransformer)
from lye.visitors.peephole import RestMerger, TieRemover

stages = (
    # Get our ASTs into the shape we want. Mostly, do our folds and eliminate
    # certain nodes.
    # Drums -> ().
    DrumsTransformer,
    # Music -> Voice inside Voice.
    VoicesTransformer,
    # Fold durations.
    DurationVisitor,
    # Times -> Music. Must come after durations.
    TimesVisitor,
    # Relative -> Music.
    Relativizer,
    # Remove any spare Musics. Must be done before peepholes.
    MusicFlattener,
    # Note -> SciNote.
    NoteTransformer,
    # Fold Dynamics. Must come after SciNotes.
    DynamicRemover,
    # Sort Chord contents. Must come after SciNotes.
    ChordSorter,
    # Add expressions.
    Express,
    # Peepholes!
    # Remove TIEs.
    TieRemover,
    # Merge Rests.
    RestMerger,
)

def simplify_ast(ast):
    for stage in stages:
        ast = stage().visit(ast)
    return ast
