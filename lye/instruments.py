from collections import defaultdict

from lye.ast import Chord, SciNote
from lye.visitor import Transposer

# Here's how pitches work. I'm writing it down largely because I keep
# forgetting.
# There are three different notations for pitches. First, there's Lilypond's
# notation. Octaves start at C. Middle C is c' and so c is actually Low C. In
# this scheme, the lowest note on a keyboard is a,,, and the highest is
# c'''''.
# Next, there is scientific notation. In this, Middle C is C4. Octaves start
# at C (mercifully). Keyboards start at A0 and go to C8.
# Finally, MIDI's numbering notation. This is the confusing one. Middle C is
# 60. Keyboards go from 21 to 108.

def sci_to_midi(s):
    pitch, octave = s
    return "CxDxEFxGxAxB".index(pitch) + (int(octave) * 12) + 12

def pair_sci_to_midi(bottom, top):
    return sci_to_midi(bottom), sci_to_midi(top)

sci = pair_sci_to_midi

instruments = [
    "acoustic grand",
    "bright acoustic",
    "electric grand",
    "honky-tonk",
    "electric piano 1",
    "electric piano 2",
    "harpsichord",
    "clav",
    "celesta",
    "glockenspiel",
    "music box",
    "vibraphone",
    "marimba",
    "xylophone",
    "tubular bells",
    "dulcimer",
    "drawbar organ",
    "percussive organ",
    "rock organ",
    "church organ",
    "reed organ",
    "accordion",
    "harmonica",
    "concertina",
    "acoustic guitar (nylon)",
    "acoustic guitar (steel)",
    "electric guitar (jazz)",
    "electric guitar (clean)",
    "electric guitar (muted)",
    "overdriven guitar",
    "distorted guitar",
    "guitar harmonics",
    "acoustic bass",
    "electric bass (finger)",
    "electric bass (pick)",
    "fretless bass",
    "slap bass 1",
    "slap bass 2",
    "synth bass 1",
    "synth bass 2",
    "violin",
    "viola",
    "cello",
    "contrabass",
    "tremolo strings",
    "pizzicato strings",
    "orchestral strings",
    "timpani",
    "string ensemble 1",
    "string ensemble 2",
    "synthstrings 1",
    "synthstrings 2",
    "choir aahs",
    "voice oohs",
    "synth voice",
    "orchestra hit",
    "trumpet",
    "trombone",
    "tuba",
    "muted trumpet",
    "french horn",
    "brass section",
    "synthbrass 1",
    "synthbrass 2",
    "soprano sax",
    "alto sax",
    "tenor sax",
    "baritone sax",
    "oboe",
    "english horn",
    "bassoon",
    "clarinet",
    "piccolo",
    "flute",
    "recorder",
    "pan flute",
    "blown bottle",
    "shakuhachi",
    "whistle",
    "ocarina",
    "lead 1 (square)",
    "lead 2 (sawtooth)",
    "lead 3 (calliope)",
    "lead 4 (chiff)",
    "lead 5 (charang)",
    "lead 6 (voice)",
    "lead 7 (fifths)",
    "lead 8 (bass+lead)",
    "pad 1 (new age)",
    "pad 2 (warm)",
    "pad 3 (polysynth)",
    "pad 4 (choir)",
    "pad 5 (bowed)",
    "pad 6 (metallic)",
    "pad 7 (halo)",
    "pad 8 (sweep)",
    "fx 1 (rain)",
    "fx 2 (soundtrack)",
    "fx 3 (crystal)",
    "fx 4 (atmosphere)",
    "fx 5 (brightness)",
    "fx 6 (goblins)",
    "fx 7 (echoes)",
    "fx 8 (sci-fi)",
    "sitar",
    "banjo",
    "shamisen",
    "koto",
    "kalimba",
    "bagpipe",
    "fiddle",
    "shanai",
    "tinkle bell",
    "agogo",
    "steel drums",
    "woodblock",
    "taiko drum",
    "melodic tom",
    "synth drum",
    "reverse cymbal",
    "guitar fret noise",
    "breath noise",
    "seashore",
    "bird tweet",
    "telephone ring",
    "helicopter",
    "applause",
    "gunshot",
]

numbered_instruments = dict((k, i) for i, k in enumerate(instruments))

# Default to the normal bounds of a keyboard.
bounds = defaultdict(lambda: sci("A0", "C8"))

bounds.update({
    "acoustic guitar (nylon)": sci("E2", "E6"),
    "acoustic guitar (steel)": sci("E2", "E6"),
    "electric guitar (jazz)": sci("E2", "E6"),
    "electric guitar (clean)": sci("E2", "E6"),
    "electric guitar (muted)": sci("E2", "E6"),
    "overdriven guitar": sci("B1", "E6"),
    "distorted guitar": sci("B1", "E6"),
    "guitar harmonics": sci("E3", "E6"),
    "acoustic bass": sci("E1", "G4"),
    "electric bass (finger)": sci("B0", "G4"),
    "electric bass (pick)": sci("B0", "G4"),
    "fretless bass": sci("E1", "G4"),
    "slap bass 1": sci("B0", "G4"),
    "slap bass 2": sci("B0", "G4"),
    "synth bass 1": sci("E1", "G4"),
    "synth bass 2": sci("E1", "G4"),
    "trumpet": sci("C4", "C6"), # R-K
    "trombone": (34, 70), # Bb1 - Bb4 (R-K)
    "muted trumpet": sci("C4", "C6"), # R-K
    "soprano sax": (56, 87), # Ab3 - Eb6
    "alto sax": (49, 80), # Db3 - Ab5
    "tenor sax": (44, 75), # Ab2 - Eb5
    "baritone sax": (37, 68), # Db2 - Ab4
    "shakuhachi": sci("A3", "C8"), # Unknown upper range
})

NEAREST, LOWEST, HIGHEST = range(3)

def top_margin(melody, bound):
    pitches = []
    for expr in melody.exprs:
        if isinstance(expr, SciNote):
            pitches.append(expr.pitch)
        elif isinstance(expr, Chord):
            pitches.append(expr.notes[0].pitch)
    i = max(pitches)
    return bound - i

def bottom_margin(melody, bound):
    pitches = []
    for expr in melody.exprs:
        if isinstance(expr, SciNote):
            pitches.append(expr.pitch)
        elif isinstance(expr, Chord):
            pitches.append(expr.notes[-1].pitch)
    i = min(pitches)
    return i - bound

def fit(melody, instrument, strategy=NEAREST):
    bottom_bound, top_bound = bounds[instrument]
    top = top_margin(melody, top_bound)
    bottom = bottom_margin(melody, bottom_bound)
    if top + bottom < 0:
        raise Exception("Couldn't ever fit this melody!")

    if strategy == NEAREST:
        if top > 0 and bottom > 0:
            return melody
        elif top < 0:
            strategy = HIGHEST
        else:
            strategy = LOWEST

    if strategy == LOWEST:
        octaves = -1 * (bottom // 12)
    elif strategy == HIGHEST:
        octaves = top // 12

    adjustment = octaves * 12
    if adjustment:
        melody = Transposer(adjustment).visit(melody)
    return melody
