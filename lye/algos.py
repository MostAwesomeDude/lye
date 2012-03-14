from warnings import warn

from lye.types import Marker

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

    The simplification is done in-place, in linear time.
    """

    pos = 0
    while True:
        try:
            pos = notes.index(Marker("tie"), pos)
        except ValueError:
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

        if before.pitch == after.pitch:
            before = before._replace(
                duration=before.duration + after.duration)
            notes[pos - 1] = before
            del notes[pos:pos + 2]
        else:
            warn("Tie between differing pitches", LyeParseWarning)
            pos += 1
