"""
Twisted-specific Lye support functions and objects.

Also contains Fluidsynth stuff.
"""

from collections import namedtuple

Seq = namedtuple("Seq", "sequencer, synthesizer, driver")

def create_sequencer(*soundfonts):
    """
    Obtain a Fluidsynth sequencer.

    Also return the synthesizer.
    """

    from fluidsynth import fluidsynth

    settings = fluidsynth.FluidSettings()
    settings.quality = "low"
    settings["audio.realtime-prio"] = 0
    settings["synth.audio-groups"] = 1
    settings["synth.ladspa.active"] = False
    settings["synth.verbose"] = True

    synth = fluidsynth.FluidSynth(settings)
    for soundfont in soundfonts:
        synth.load_soundfont(soundfont)

    driver = fluidsynth.FluidAudioDriver(settings, synth)

    seq = fluidsynth.FluidSequencer()
    seq.beats_per_minute = 120
    seq.ticks_per_beat = 480

    seq.add_synth(synth)

    return Seq(seq, synth, driver)
