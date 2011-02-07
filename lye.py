import logging

#import fluidsynth

import pymeta.grammar
import pymeta.runtime

log = logging.getLogger("salsa.ly")

class Note(object):
    """
    The fundamental unit of composition.
    """

    def __init__(self, pitch, duration):
        self.pitch = pitch
        self.duration = duration

    def __repr__(self):
        return "Note(%d, %d)" % (self.pitch, self.duration)

    __str__ = __repr__

class Chord(Note):
    """
    A Note list.
    """

    def __init__(self, notes):
        self.pitches = [note.pitch for note in notes]
        self.duration = notes[0].duration

    def __repr__(self):
        return "Chord(%r, %d)" % (self.pitches, self.duration)

    __str__ = __repr__

class Marker(object):
    """
    A singleton representing a measure marker.
    """

class Measure(object):
    def __init__(self, notes):
        self.notes = notes

    def __nonzero__(self):
        return any(self.notes)

    def __repr__(self):
        return "Measure(%r)" % self.notes

    __str__ = __repr__

class Melody(object):

    ticks_per_beat = 480

    def __init__(self, notes):
        self.notes = notes
        self.schedule_notes()

    def __nonzero__(self):
        return any(self.notes)

    def __repr__(self):
        return "Melody(%r)" % self.notes

    __str__ = __repr__

    def schedule_notes(self):
        """
        Schedule notes by turning their durations into absolute begin, end
        pairs.
        """

        relative_marker = 0
        scheduled = []

        for note in self.notes:
            if isinstance(note, Marker):
                if relative_marker != 0:
                    print "Marker is off by %d" % relative_marker
                continue

            elif isinstance(note, Chord):
                begin = relative_marker
                end = begin + note.duration
                relative_marker = end

                for pitch in note.pitches:
                    scheduled.append((pitch, begin, end))

            else:
                begin = relative_marker
                # XXX fudge?
                end = begin + note.duration
                relative_marker = end

                scheduled.append((note.pitch, begin, end))

            while relative_marker >= self.ticks_per_beat:
                relative_marker -= self.ticks_per_beat

        self.notes = scheduled

    def play(self, sequencer):
        """
        Sends the melody to `sequencer`.
        """

        # XXX
        tpb = sequencer.ticks_per_beat
        # Each item in the seq is (fluidsynth.FS, (dest, destname))
        # We just want the dest
        dest = sequencer.items()[0][1][0]
        # XXX this fudge value might not be needed?
        ticks = sequencer.ticks + 10

        for measure in self.measures:
            for pitch, begin, end in measure.notes:
                if pitch != -1:
                    event = fluidsynth.FluidEvent()
                    event.dest = dest
                    # XXX ? pitch vel duration
                    event.note(0, pitch, 127, end - begin)
                    sequencer.send(event, ticks + begin)

            # XXX
            ticks += 120 * 4

# es requires a special case, because it can either be spelled es or ees.
pitch_dict = dict(zip("cxdxefxgxaxb", range(48, 60)))
pitch_dict["es"] = 51
del pitch_dict["x"]

relative_dict = dict(zip("cdefgab", range(7)))
relative_dict["es"] = relative_dict["e"]

grammar = """
relative ::= <token '\\\\relative'>
begin_relative ::= <relative> <spaces>
                   <pitch>:p <accidental>? <octave>?:o <spaces> '{'
                 => self.open_brace("relative", (p, o if o else ""))

close_brace ::= '}' => self.close_brace()

directive ::= <begin_relative> | <close_brace>

sharp ::= 'i' 's' => "is"
flat ::= 'e' 's' => "es"
accidental ::= (<sharp> | <flat>)+:a => "".join(a)

pitch ::= ('r' | 'c' | 'd' | <flat> | 'e' | 'f' | 'g' | 'a' | 'b')

octave ::= ('\'' | ',')+:o => "".join(o)

duration ::= (<digit>+):d '.'*:dots
           => self.undot_duration(int("".join(d)), len(dots))

note ::= <pitch>:p <accidental>?:a <octave>?:o <duration>?:d
       => Note(self.abs_pitch_to_number(p, a, o),
           self.check_duration(d))

notes ::= <spaces>? <note>:n (<spaces> <note>)*:ns
          => [n] + ns

chord ::= <token '<'> <spaces>? <notes>:ns <token '>'> => Chord(ns)

marker ::= <token '|'> => self.marker

protonote ::= (<note> | <chord> | <marker>)

protonote_cluster ::= <spaces>? <protonote>:pn (<spaces>? <protonote>)*:pns
                    => [pn] + pns

melody ::= <directive>? <protonote_cluster>:m <directive>? => Melody(m)
"""

class LyGrammar(pymeta.grammar.OMeta.makeGrammar(grammar, globals())):
    """
    Class providing parsing and lexing of pseudo-Lilypond streams into data
    structures that can be passed to other high-level libraries.

    Like with standard Lilypond, the default octave starts at C3 (48).
    """

    marker = Marker()

    ticks_per_beat = 120
    """
    Number of ticks per beat.
    """

    def __init__(self, *args, **kwargs):
        super(LyGrammar, self).__init__(*args, **kwargs)

        self.duration = self.ticks_per_beat

        self.brace_stack = []
        self.relative = None

    def open_brace(self, name, arg=None):
        self.brace_stack.append(
            lambda self: setattr(self, name, None))
        setattr(self, name, arg)

    def close_brace(self):
        self.brace_stack.pop()(self)

    def undot_duration(self, duration, dots):
        """
        Turn duration into a number of ticks, and apply dots, if any.
        """

        duration = self.ticks_per_beat * 4 / duration
        while dots:
            dots -= 1
            duration *= 1.5
        return int(duration)

    def abs_pitch_to_number(self, pitch, accidental, octave):
        """
        Convert an absolute pitch to its MIDI/scientific number.
        """

        if pitch == "r":
            # Rests are forced to -1
            return -1
        accidental = accidental if accidental else ""
        octave = octave if octave else ""

        if self.relative:
            rel_pitch, rel_octave = self.relative
            octave += rel_octave
            if abs(relative_dict[rel_pitch] - relative_dict[pitch]) > 3:
                if relative_dict[pitch] > 4:
                    octave += ","
                else:
                    octave += "'"
            self.relative = pitch, octave

        n = pitch_dict[pitch]

        while accidental:
            if accidental.startswith("es"):
                accidental = accidental[2:]
                n -= 1
            elif accidental.startswith("is"):
                accidental = accidental[2:]
                n += 1
            else:
                log.error("Unknown symbol %s while lexing accidental"
                    % accidental)
                break

        while octave:
            if octave[0] == ",":
                octave = octave[1:]
                n -= 12
            elif octave[0] == "'":
                octave = octave[1:]
                n += 12
            else:
                log.error("Unknown symbol %s while lexing octave" % octave)
                break

        return n

    def check_duration(self, d):
        if d:
            self.duration = d
        return self.duration

#print LyGrammar("c4 d e  d  c").apply("notes")
#print LyGrammar("c e g c' |").apply("measure")
#print LyGrammar("e4 d c2 | e4 d c2 |").apply("measures")

def chords_from_ly(s):
    """
    Make a `Chords` from a ly string.
    """

    chords = LyGrammar(s).apply("chords")
    if not chords:
        log.error("Failed chords %s" % s)
    return chords

def melody_from_ly(s):
    """
    Make a `Melody` from a ly string.
    """

    melody = LyGrammar(s).apply("melody")
    if not melody:
        log.error("Failed melody %s" % s)
    return melody
