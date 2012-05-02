from collections import namedtuple
from warnings import warn

from lye.ast import MEASURE, PARTIAL, TIE, Chord, SciNote, Rest, Voices

ScheduledNote = namedtuple("ScheduledNote", "pitch, begin, duration")

class LyeParseWarning(Warning):
    """
    A dubious token was encountered while parsing a melody.
    """

def pitch_to_number(pitch, accidental, octave):
    """
    Convert an absolute pitch to its MIDI/scientific number.
    """

    # es requires a special case, because it can either be spelled es or ees.
    pitch_dict = dict(zip("cxdxefxgxaxb", range(48, 60)))
    pitch_dict["es"] = 51
    del pitch_dict["x"]

    i = pitch_dict[pitch]
    return i + accidental + (octave * 12)

def simplify_ties(notes):
    """
    Find ties in a stream of notes and simplify them.

    The notes must be SciNotes.

    The simplification is done in-place, in linear time.
    """

    pos = 0
    while True:
        try:
            pos = notes.index(TIE, pos)
        except ValueError:
            # No more ties. We're done.
            break

        if pos == -1:
            break
        elif pos == 0:
            warn("Tie at beginning of note stream", LyeParseWarning)
            pos += 1
            continue
        elif pos == len(notes) - 1:
            warn("Tie at end of note stream", LyeParseWarning)
            break

        before = notes[pos - 1]
        after = notes[pos + 1]

        if not isinstance(before, SciNote) or not isinstance(after, SciNote):
            continue

        if before.pitch == after.pitch:
            before = before._replace(
                duration=before.duration + after.duration)
            notes[pos - 1] = before
            del notes[pos:pos + 2]
        else:
            warn("Tie between differing pitches", LyeParseWarning)
            pos += 1

def schedule_notes(node, tpb, beginning=0):
    """
    Attach correct beginning times to notes in an AST node.

    Rests are discarded. Chords are broken up. Voices are recursed and
    correctly added.
    """

    relative_marker = beginning
    partial = False
    partial_offset = 0
    scheduled = []

    if isinstance(node, Voices):
        for voice in node.exprs:
            inner_scheduled, inner_relative = schedule_notes(voice, tpb,
                    beginning)
            relative_marker = max(inner_relative, relative_marker)
            scheduled.extend(inner_scheduled)
    else:
        for i, expr in enumerate(node.exprs):
            if expr is MEASURE:
                remainder = ((relative_marker - partial_offset) % tpb)
                if remainder and not partial:
                    print "Marker is off by %d" % remainder
                # Start the next bar.
                partial = False
                partial_offset = remainder

            elif expr is PARTIAL:
                partial = True

            elif isinstance(expr, Chord):
                begin = relative_marker
                relative_marker = begin + expr.notes[0].duration

                for note in expr.notes:
                    scheduled.append(ScheduledNote(note.pitch, begin,
                        note.duration))

            elif isinstance(expr, SciNote):
                begin = relative_marker
                relative_marker = begin + expr.duration

                scheduled.append(ScheduledNote(expr.pitch, begin,
                    expr.duration))

            elif isinstance(expr, Rest):
                begin = relative_marker
                relative_marker = begin + expr.duration

    return scheduled, relative_marker
