===
Lye
===

Lye is a simple, straightforward library for expressing musical grooves as
compositions of small snippets of music. It could be thought of as an
extremely complex music box.

Lye takes in snippets of Lilypond markup written in a strict set of Lilypond,
called Lye, and outputs MIDI data, either in realtime or to a MIDI file for
later playback.

Fluidsynth is required, along with my Python bindings for Fluidsynth. The
bindings can be found on PyPI under "fluidsynth".

The Lye Language
================

Lye is a strict subset of Lilypond. The following features are supported:

 * Notes

   * Pitches
   * Accidentals
   * Octaves
   * Durations
   * Ties (``~``)

 * Rests (``r4``)
 * Chords (``<c e g>``)
 * Voices (``<< { c d e d } { e f g f } >>``)
 * Dynamics (``\\ff``)
 * Tuplets (``\\times``)
 * Drums mode (``\\drums``)
 * Relative mode (``\\relative``)

Embedded Scheme is **not** supported, and will not be supported. Lye is not
for typesetting, it is for performance.

lyne.py
=======

lyne.py is the main entry point for Lye at the moment. It reads in a timelyne
and produces a MIDI file. To run it, try:

 $ ./lyne.py <library> <lyne> <midi>

For example, to compile the example groove, try:

 $ ./lyne.py groove groove.lyne groove.mid

lyne-fs.py
==========

lyne-fs.py is an experimental Twisted-Fluidsynth blend which renders a
timelyne to Fluidsynth.

 $ ./lyne-fs.py <library> <lyne> <soundfont>

Bugs
====

Lynes can't be rewound. The information in lynes about instrument changes only
goes forwards, not backwards. This isn't considered super-important yet and
would be easy to address.

Fluidsynth is a fickle mistress. Drums don't play, some melodies are rendered
staccato, etc.
