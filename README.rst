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

lyne.py
=======

lyne.py is the main entrypoint for Lye at the moment. It reads in a timelyne
and produces a MIDI file. To run it, try:

 $ ./lyne.py <library> <lyne> <midi>

For example, to compile the example groove, try:

 $ ./lyne.py groove groove.lyne groove.mid
