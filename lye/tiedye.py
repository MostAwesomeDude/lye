"""
Twisted-specific Lye support functions and objects.
"""

def create_sequencer(*soundfonts):
    """
    Obtain a Fluidsynth sequencer.

    Also return the synthesizer.
    """

    from fluidsynth import fluidsynth

    settings = fluidsynth.FluidSettings()
    settings.quality = "low"

    synth = fluidsynth.FluidSynth(settings)
    for soundfont in soundfonts:
        synth.load_soundfont(soundfont)

    fluidsynth.FluidAudioDriver(settings, synth)

    sequencer = fluidsynth.FluidSequencer()
    sequencer.beats_per_minute = 120
    sequencer.ticks_per_beat = 480

    sequencer.add_synth(synth)

    return synth, sequencer
