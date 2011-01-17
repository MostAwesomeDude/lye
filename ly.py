#!/usr/bin/env python

"""
Class providing parsing and lexing of pseudo-Lilypond streams into data
structures that can be passed to other high-level libraries.

Like with standard Lilypond, the default octave starts at C3 (48).
"""

import logging

import fluidsynth

import pymeta.grammar
import pymeta.runtime

log = logging.getLogger("salsa.ly")

class Measure(object):
    def __init__(self, notes):
        self.notes = notes
        self.schedule_notes()

    def __nonzero__(self):
        return any(self.notes)

    def __repr__(self):
        return "Measure(%s)" % self.notes

    def schedule_notes(self):
        """
        Schedule notes by turning their durations into absolute begin, end
        pairs.
        """

        relative_marker = 0

        for i, (pitch, duration) in enumerate(self.notes):
            begin = relative_marker
            # XXX fudge?
            end = begin + duration

            self.notes[i] = pitch, begin, end

            relative_marker += duration

class Melody(object):
    def __init__(self, measures):
        self.measures = measures

    def __nonzero__(self):
        return any(self.measures)

    def __repr__(self):
        return "Melody(%s)" % self.measures

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

class Chords(Melody):
    def __init__(self, measures):
        self.measures = []
        self.chord_measures = measures
        self.chords_to_notes()

    def chords_to_notes(self):
        self.measures = self.chord_measures

# es requires a special case, because it can either be spelled es or ees.
pitch_dict = dict(zip("cxdxefxgxaxb", range(48, 60)))
pitch_dict["es"] = 51

relative_dict = dict(zip("cdefgab", range(7)))
relative_dict["es"] = relative_dict["e"]

grammar = """
relative ::= '\\\\' 'r' 'e' 'l' 'a' 't' 'i' 'v' 'e' => "\\relative"
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
       => (self.abs_pitch_to_number(p, a, o),
           self.check_duration(d))

measure ::= <spaces>? <note>:n (<spaces> <note>)*:ns <spaces>?
          => Measure([n] + ns)

measures ::= <measure>:m ('|' <measure>)*:ms => [m] + ms

melody ::= <directive>? <measures>:m <directive>? => Melody(m)

chord ::= <pitch>:p <accidental>?:a <duration>?:d <spaces>
        => (self.abs_pitch_to_number(p, a, ""),
            self.check_duration(d))

chords ::= <spaces>? <chord>+:c => Chords([Measure(c)])
"""

class LyGrammar(pymeta.grammar.OMeta.makeGrammar(grammar, globals())):

    def __init__(self, *args, **kwargs):
        super(LyGrammar, self).__init__(*args, **kwargs)

        self.tpb = 120
        self.duration = self.tpb

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

        duration = self.tpb * 4 / duration
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

# print LyGrammar("c1").apply("note")
# print LyGrammar("c4 d e  d  c").apply("notes")
# print LyGrammar("c e g c' |").apply("measure")
# print LyGrammar("e4 d c2 | e4 d c2 |").apply("measures")

def chords_from_ly(s):
    """
    Make a `Chords` from a ly string.
    """

    chords = LyGrammar(s).apply("chords")
    if not chords:
        print "[ly] [!!] Failed chords %s" % s
    return chords

def melody_from_ly(s):
    """
    Make a `Melody` from a ly string.
    """

    melody = LyGrammar(s).apply("melody")
    if not melody:
        log.error("Failed melody %s" % s)
    return melody
