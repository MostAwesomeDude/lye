from collections import namedtuple

from lye.ast import (MEASURE, PARTIAL, Chord, PitchBend, SciNote, Rest,
                     Voices)

ScheduledNote = namedtuple("ScheduledNote", "pitch, velocity, begin, duration")

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
                    scheduled.append(ScheduledNote(note.pitch, note.velocity,
                        begin, note.duration))

            elif isinstance(expr, SciNote):
                begin = relative_marker
                relative_marker = begin + expr.duration
                prev_note = begin

                scheduled.append(ScheduledNote(expr.pitch, expr.velocity,
                    begin, expr.duration))

            elif isinstance(expr, PitchBend):
                scheduled.append((0, expr.value, prev_note + expr.offset, 0))

            elif isinstance(expr, Rest):
                begin = relative_marker
                relative_marker = begin + expr.duration

    return scheduled, relative_marker
