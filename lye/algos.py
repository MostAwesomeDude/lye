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
