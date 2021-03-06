from fractions import Fraction
from operator import attrgetter, mul

from lye.algos import pitch_to_number
from lye.ast import Duration, Music, PitchBend, Rest, SciNote, Voice
from lye.facts import can_legato
from lye.visitors.visitor import Visitor, hasfield


class DrumsTransformer(Visitor):
    """
    Drums aren't actually meaningful as a node at the moment, so simplify them
    by returning their contents.
    """

    def visit_Drums(self, drums):
        return drums.expr, True


class VoicesTransformer(Visitor):
    """
    Children of Voices are always Music. Specializing them to Voice adds
    semantic information which Music does not provide for later consumers.
    """

    def visit_Voices(self, voices):
        for i, music in enumerate(voices.exprs):
            voice = Voice(music.exprs)
            voices.exprs[i] = voice
        return voices, True


class DurationVisitor(Visitor):
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

        f = Fraction(1, 2 ** dots)
        f = Fraction(2, 1) - f
        duration *= f

        if duration.denominator != 1:
            raise Exception("Couldn't simplify %s" % duration)

        return duration.numerator

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

        if hasfield(ast, "duration"):
            duration = self.fill_duration(ast.duration)
            duration = self.undot_duration(duration.length, duration.dots)
            ast = ast._replace(duration=duration)

        return ast, True


class TimesVisitor(Visitor):
    """
    Apply Times nodes and turn them into Music nodes.
    """

    def __init__(self):
        self.tuplets = [Fraction(1, 1)]
        self.refresh_scale()

    def refresh_scale(self):
        self.scale = reduce(mul, self.tuplets)

    def push_tuplet(self, tuplet):
        self.tuplets.append(tuplet)
        self.refresh_scale()

    def pop_tuplet(self):
        retval = self.tuplets.pop()
        self.refresh_scale()
        return retval

    def visit_Times(self, times):
        """
        Turn Times into Music.
        """

        self.push_tuplet(times.fraction)
        node = self.visit(Music([times.expr]))
        self.pop_tuplet()
        return node, False

    def visit_generic(self, ast):
        """
        Apply Times across Durations.
        """

        if hasfield(ast, "duration") and self.scale != Fraction(1, 1):
            duration = int(ast.duration * self.scale)
            ast = ast._replace(duration=duration)

        return ast, True


class Relativizer(Visitor):
    """
    Apply Relative octaves to Notes.
    """

    previous = []

    relative_dict = dict(zip("cdefgab", range(7)))
    relative_dict["es"] = relative_dict["e"]

    def relativize(self, note):
        if self.previous:
            # Unpack the note, relativize it, repack it.
            ppitch, poctave = self.previous[-1]
            octave = note.octave + poctave

            if abs(self.relative_dict[ppitch] -
                self.relative_dict[note.pitch]) > 3:
                if self.relative_dict[note.pitch] >= 4:
                    octave -= 1
                else:
                    octave += 1

            self.previous[-1] = note.pitch, octave
            note = note._replace(octave=octave)
        return note

    def visit_Chord(self, chord):
        if chord.notes:
            notes = chord.notes
            if self.previous:
                # Relativize the first note ahead of time.
                notes[0] = self.relativize(notes[0])

            # Push a new relative context just for this chord, and relativize.
            self.previous.append((notes[0].pitch, notes[0].octave))
            for i, note in enumerate(notes[1:]):
                notes[i] = self.relativize(note)
            self.previous.pop()
        return chord, False

    def visit_Note(self, note):
        note = self.relativize(note)
        return note, True

    def visit_Relative(self, relative):
        self.previous.append((relative.pitch, relative.octave))
        expr = self.visit(relative.expr)
        self.previous.pop()

        return expr, False


class MusicFlattener(Visitor):
    """
    Flatten Music expressions.
    """

    def __init__(self, instrument=None):
        pass

    @staticmethod
    def flatten_inner(node):
        """
        Flatten all Music expressions within this node.

        The node must have an "exprs" attribute.
        """

        flattening = True
        while flattening:
            flattening = False
            exprs = []
            for expr in node.exprs:
                if isinstance(expr, Music):
                    exprs.extend(expr.exprs)
                    flattening = True
                else:
                    exprs.append(expr)
            node = node._replace(exprs=exprs)
        return node

    def visit_Music(self, music):
        # Flatten singleton Music expressions.
        while len(music.exprs) == 1:
            music = music.exprs[0]
        # Inline Music within Music.
        music = self.flatten_inner(music)
        return music, True

    def visit_Voice(self, voice):
        # Flatten Music inside Voice because Voice is just Music with a
        # fancier name.
        voice = self.flatten_inner(voice)
        return voice, True


class NoteTransformer(Visitor):
    """
    Turn Notes into SciNotes.
    """

    def visit_Note(self, note):
        number = pitch_to_number(note.pitch, note.accidental, note.octave)
        return SciNote(number, note.duration, None, note.articulation), True


class DynamicRemover(Visitor):
    """
    Apply Dynamics to SciNotes, consuming the Dynamics.
    """

    volume = "mf"

    def visit_SciNote(self, scinote):
        scinote = scinote._replace(velocity=self.volume)
        return scinote, False

    def visit_Dynamic(self, dynamic):
        self.volume = dynamic.mark

        return None, False


class ArticulateDynamics(Visitor):
    """
    Apply articulations to SciNotes.

    The articulations are preserved for later passes as well.
    """

    dynamics = "pp p mp mf f ff".split()

    def visit_SciNote(self, scinote):
        ds = self.dynamics

        if scinote.articulation == ">":
            # Accent.
            if scinote.velocity != ds[-1]:
                velocity = ds[ds.index(scinote.velocity) + 1]
                scinote = scinote._replace(velocity=velocity)
        return scinote, False


class ChordSorter(Visitor):
    """
    Ensure Chords have their notes in strictly decreasing order.
    """

    def visit_Chord(self, chord):
        chord.notes.sort(key=attrgetter("pitch"), reverse=True)
        return chord, False


class Multiply(Visitor):
    """
    Repeat all sequences of notes by a given amount.
    """

    def __init__(self, multiplier):
        self.multiplier = multiplier

    def visit_Music(self, music):
        music = music._replace(exprs=music.exprs * self.multiplier)
        return music, False

    def visit_Voices(self, voices):
        exprs = []
        for voice in voices.exprs:
            voice = voice._replace(exprs=voice.exprs * self.multiplier)
            exprs.append(voice)
        voices = voices._replace(exprs=exprs)
        return voices, False


class Transposer(Visitor):
    """
    Adjust an entire expression's pitches by a given amount.
    """

    def __init__(self, adjustment):
        self.adjustment = adjustment

    def visit_SciNote(self, scinote):
        scinote = scinote._replace(pitch=scinote.pitch + self.adjustment)
        return scinote, False


class HarmonySplitter(Visitor):
    """
    Split a harmony of chords into a single voice.

    Extra voices are done in one of two ways. For single notes, the note is
    doubled up by all voices. For chords with too few notes for the number of
    voices, modulus math is used to evenly distribute the chord across all
    participating voices. Don't forget to fit!
    """

    def __init__(self, voice):
        self.voice = voice

    def visit_Chord(self, chord):
        index = self.voice % len(chord.notes)
        return chord.notes[index], False


class Legato(Visitor):
    """
    Alter lengths of notes in order to make them more expressive.

    Only works on SciNotes.

    In the process, swallow up things like slurs.
    """

    def __init__(self, instrument):
        self.inst = instrument

    @staticmethod
    def shorten(scinote):
        # The rules are simple.
        # 1) Ideally, the new duration is 90% of the old duration.
        # 2) The total elapsed time in this voice must be unchanged.
        # 3) 1 <= x <= 10 for all times x that are removed from the note.

        off = max(1, min(10, scinote.duration // 10))
        on = scinote.duration - off
        scinote = scinote._replace(duration=on)
        rest = Rest(off)

        return scinote, rest

    def visit_SciNote(self, scinote):
        """
        Slightly shorten notes.

        We are guaranteed to never reach here inside Slurs, because we also
        visit Slurs and refuse to recurse inside them.
        """

        scinote, rest = self.shorten(scinote)

        # Don't recurse further; it isn't gonna work very well.
        return Music([scinote, rest]), False

    def visit_Chord(self, chord):
        """
        Shorten the chord slightly.
        """

        notes = []

        for note in chord.notes:
            note, rest = self.shorten(note)
            notes.append(note)

        chord = chord._replace(notes=notes)
        return Music([chord, rest]), False

    def visit_Slur(self, slur):
        """
        Attempt to pitch-bend to victory.

        Also apply shortening to the tail of the slur.
        """

        def fail():
            # Cut the tail off the end and return the whole shebang.
            last, rest = self.shorten(slur.exprs[-1])
            return Music(slur.exprs[:-1] + [last, rest]), False

        # If we can't do legato on this instrument, bail.
        if not can_legato(self.inst):
            return fail()

        first = slur.exprs[0]
        exprs = slur.exprs[1:]

        for expr in exprs:
            if abs(expr.pitch - first.pitch) > 12:
                # Bail.
                return fail()

        first = first._replace(
                duration=sum(expr.duration for expr in slur.exprs))
        first, rest = self.shorten(first)
        offset = 0
        out = [first]

        for expr in exprs:
            offset += expr.duration
            out.append(PitchBend(offset,
                (8191 // 12) * (expr.pitch - first.pitch)))

        out.append(rest)

        return Music(out), False
