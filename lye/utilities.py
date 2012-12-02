from lye.instruments import instruments as midi_instruments

def make_velocity(s):
    l = "pp p mp mf f ff".split()
    i = l.index(s)
    i = i * 18 + 19
    return i

def find_instrument(name):
    """
    Attempt to fully qualify a MIDI instrument name.
    """

    name = name.lower()
    found = []

    for instrument in midi_instruments:
        if instrument.startswith(name):
            found.append(instrument)

    if len(found) < 1:
        raise Exception("Couldn't match any instruments for %s" % name)
    elif len(found) > 1:
        raise Exception("Found multiple instruments for %s: %s"
            % (name, found))
    return found[0]
