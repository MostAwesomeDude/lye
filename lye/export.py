from __future__ import division

from StringIO import StringIO

from fluidsynth.fluidsynth import FluidEvent

from lye.MidiFile import MIDIFile

class FileExporter(object):
    """
    Export MIDI data to a file.
    """

    def __init__(self, channels, tempo, tpb):
        self.f = MIDIFile(channels, ticksPerBeat=tpb)
        self.f.addTrackName(0, 0, "Lye")
        self.f.addTempo(0, 0, tempo)

        self.tpb = tpb

    def commit(self):
        sio = StringIO()
        self.f.writeFile(sio)
        return sio.getvalue()

    def note(self, channel, time, duration, pitch, velocity):
        time /= self.tpb
        duration /= self.tpb
        self.f.addNote(0, channel, pitch, time, duration, velocity)

    def bend(self, channel, time, value):
        pass

    def pan(self, channel, time, amount):
        time /= self.tpb
        self.f.addControllerEvent(0, channel, time, 0x0a, amount)

    def pc(self, channel, time, program):
        time /= self.tpb
        self.f.addProgramChange(0, channel, time, program)

    def volume(self, channel, time, amount):
        time /= self.tpb
        self.f.addControllerEvent(0, channel, time, 0x07, amount)

class FSExporter(object):
    """
    Export MIDI data to a Fluidsynth sequencer.
    """

    def __init__(self, seq, offset):
        self.seq = seq
        self._offset = offset
        self.offset = seq.ticks + offset

        # Each item in the seq is (fluidsynth.FS, (dest, destname))
        # We just want the dest right now.
        self.dest = seq.items()[0][1][0]

    def commit(self):
        return self.seq.ticks - (self.offset - self._offset)

    def note(self, channel, time, duration, pitch, velocity):
        event = FluidEvent()
        event.dest = self.dest
        event.noteon(channel, pitch, velocity)
        self.seq.send(event, self.offset + time)

        event = FluidEvent()
        event.dest = self.dest
        event.noteoff(channel, pitch)
        self.seq.send(event, self.offset + time + duration)

    def bend(self, channel, time, value):
        event = FluidEvent()
        event.dest = self.dest
        event.pitch_bend(channel, value + 8192)
        self.seq.send(event, self.offset + time)

    def pan(self, channel, time, amount):
        pass

    def pc(self, channel, time, program):
        event = FluidEvent()
        event.dest = self.dest
        event.pc(channel, program)
        self.seq.send(event, self.offset + time)

    def volume(self, channel, time, amount):
        event = FluidEvent()
        event.dest = self.dest
        event.volume(channel, amount)
        self.seq.send(event, self.offset + time)
