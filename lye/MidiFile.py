#-----------------------------------------------------------------------------
# Name:        MidiFile.py
# Purpose:     MIDI file manipulation utilities
#
# Author:      Mark Conway Wirt <emergentmusics) at (gmail . com>
#
# Created:     2008/04/17
# Copyright:   (c) 2009 Mark Conway Wirt
# License:     Please see License.txt for the terms under which this
#              software is distributed.
#-----------------------------------------------------------------------------

import math
import struct
import sys

controllerEventTypes = {
    'pan' : 0x0a,
}

class MIDIEvent:
    '''
    The class to contain the MIDI Event (placed on MIDIEventList.
    '''
    def __init__(self):
        self.type='unknown'
        self.time=0
        self.ord = 0

    def __cmp__(self, other):
        ''' Sorting function for events.'''
        if self.time < other.time:
            return -1
        elif self.time > other.time:
            return 1
        else:
            if self.ord < other.ord:
                return -1
            elif self.ord > other.ord:
                return 1
            else:
                return 0

class GenericEvent():
    '''The event class from which specific events are derived
    '''
    def __init__(self,time):
        self.time = time
        self.type = 'Unknown'



    def __eq__(self, other):
        '''
        Equality operator for Generic Events and derived classes.

        In the processing of the event list, we have need to remove duplicates. To do this
        we rely on the fact that the classes are hashable, and must therefore have an
        equality operator (__hash__() and __eq__() must both be defined).

        This is the most embarrassing portion of the code, and anyone who knows about OO
        programming would find this almost unbelievable. Here we have a base class that
        knows specifics about derived classes, thus breaking the very spirit of
        OO programming.

        I suppose I should go back and restructure the code, perhaps removing the derived
        classes altogether. At some point perhaps I will.
        '''
        if self.time != other.time or self.type != other.type:
            return False

        # What follows is code that encodes the concept of equality for each derived
        # class. Believe it f you dare.

        if self.type == 'note':
            if self.pitch != other.pitch or self.channel != other.channel:
                return False
        if self.type == 'tempo':
            if self.tempo != other.tempo:
                return False
        if self.type == 'programChange':
            if self.programNumber != other.programNumber or self.channel != other.channel:
                return False
        if self.type == 'trackName':
            if self.trackName != other.trackName:
                return False
        if self.type == 'controllerEvent':
            if self.parameter1 != other.parameter1 or \
                self.channel != other.channel or \
                self.eventType != other.eventType:
                return False

        if self.type == 'SysEx':
            if self.manID != other.manID:
                return False

        if self.type == 'UniversalSysEx':
            if self.code != other.code or\
                self.subcode != other.subcode or \
                self.sysExChannel != other.sysExChannel:
                return False

        return True

    def __hash__(self):
        '''
        Return a hash code for the object.
        '''

        return hash(self.time)

class Note(GenericEvent):
    '''A class that encapsulates a note
    '''
    def __init__(self,channel, pitch,time,duration,volume):

        GenericEvent.__init__(self,time)
        self.pitch = pitch
        self.duration = duration
        self.volume = volume
        self.type = 'note'
        self.channel = channel

    def compare(self, other):
        '''Compare two notes for equality.
        '''
        if self.pitch == other.pitch and \
            self.time == other.time and \
            self.duration == other.duration and \
            self.volume == other.volume and \
            self.type == other.type and \
            self.channel == other.channel:
                return True
        else:
                return False


class Tempo(GenericEvent):
    '''A class that encapsulates a tempo meta-event
    '''
    def __init__(self,time,tempo):

        GenericEvent.__init__(self,time)
        self.type = 'tempo'
        self.tempo = int(60000000 / tempo)

class ProgramChange(GenericEvent):
    '''A class that encapsulates a program change event.
    '''

    def __init__(self,  channel,  time,  programNumber):
        GenericEvent.__init__(self, time,)
        self.type = 'programChange'
        self.programNumber = programNumber
        self.channel = channel

class SysExEvent(GenericEvent):
    '''A class that encapsulates a System Exclusive  event.
    '''

    def __init__(self,  time,  manID,  payload):
        GenericEvent.__init__(self, time,)
        self.type = 'SysEx'
        self.manID = manID
        self.payload = payload

class UniversalSysExEvent(GenericEvent):
    '''A class that encapsulates a Universal System Exclusive  event.
    '''

    def __init__(self,  time,  realTime,  sysExChannel,  code,  subcode,  payload):
        GenericEvent.__init__(self, time,)
        self.type = 'UniversalSysEx'
        self.realTime = realTime
        self.sysExChannel = sysExChannel
        self.code = code
        self.subcode = subcode
        self.payload = payload

class ControllerEvent(GenericEvent):
    '''A class that encapsulates a program change event.
    '''

    def __init__(self,  channel,  time,  eventType,  parameter1,):
        GenericEvent.__init__(self, time,)
        self.type = 'controllerEvent'
        self.parameter1 = parameter1
        self.channel = channel
        self.eventType = eventType

class TrackName(GenericEvent):
    '''A class that encapsulates a program change event.
    '''

    def __init__(self,  time,  trackName):
        GenericEvent.__init__(self, time,)
        self.type = 'trackName'
        self.trackName = trackName

class MIDITrack:
    '''A class that encapsulates a MIDI track
    '''

    def __init__(self, ticksPerBeat, removeDuplicates,  deinterleave):
        '''Initialize the MIDITrack object.
        '''

        self.ticksPerBeat = ticksPerBeat

        self.headerString = struct.pack('cccc','M','T','r','k')
        self.dataLength = 0 # Is calculated after the data is in place
        self.MIDIdata = ""
        self.closed = False
        self.eventList = []
        self.MIDIEventList = []
        self.remdep = removeDuplicates
        self.deinterleave = deinterleave

    def addNoteByNumber(self,channel, pitch,time,duration,volume):
        '''Add a note by chromatic MIDI number
        '''
        self.eventList.append(Note(channel, pitch,time,duration,volume))

    def addControllerEvent(self,channel,time,eventType, paramerter1):
        '''
        Add a controller event.
        '''

        self.eventList.append(ControllerEvent(channel,time,eventType, \
                                             paramerter1))

    def addTempo(self,time,tempo):
        '''
        Add a tempo change (or set) event.
        '''
        self.eventList.append(Tempo(time,tempo))

    def addSysEx(self,time,manID, payload):
        '''
        Add a SysEx event.
        '''
        self.eventList.append(SysExEvent(time, manID,  payload))

    def addUniversalSysEx(self,time,code, subcode, payload,  sysExChannel=0x7F,  \
        realTime=False):
        '''
        Add a Universal SysEx event.
        '''
        self.eventList.append(UniversalSysExEvent(time, realTime,  \
            sysExChannel,  code,  subcode, payload))

    def addProgramChange(self,channel, time, program):
        '''
        Add a program change event.
        '''
        self.eventList.append(ProgramChange(channel, time, program))

    def addTrackName(self,time,trackName):
        '''
        Add a track name event.
        '''
        self.eventList.append(TrackName(time,trackName))

    def changeNoteTuning(self,  tunings,   sysExChannel=0x7F,  realTime=False,  \
        tuningProgam=0):
        '''Change the tuning of MIDI notes
        '''
        payload = struct.pack('>B',  tuningProgam)
        payload = payload + struct.pack('>B',  len(tunings))
        for (noteNumber,  frequency) in tunings:
            payload = payload + struct.pack('>B',  noteNumber)
            MIDIFreqency = frequencyTransform(frequency)
            for byte in MIDIFreqency:
                payload = payload + struct.pack('>B',  byte)

        self.eventList.append(UniversalSysExEvent(0, realTime,  sysExChannel,\
            8,  2, payload))

    def processEventList(self):
        '''
        Process the event list, creating a MIDIEventList

        For each item in the event list, one or more events in the MIDIEvent
        list are created.
        '''

        # Loop over all items in the eventList

        for thing in self.eventList:
            if thing.type == 'note':
                event = MIDIEvent()
                event.type = "NoteOn"
                event.time = thing.time * self.ticksPerBeat
                event.pitch = thing.pitch
                event.volume = thing.volume
                event.channel = thing.channel
                event.ord = 3
                self.MIDIEventList.append(event)

                event = MIDIEvent()
                event.type = "NoteOff"
                event.time = (thing.time + thing.duration) * self.ticksPerBeat
                event.pitch = thing.pitch
                event.volume = thing.volume
                event.channel = thing.channel
                event.ord = 2
                self.MIDIEventList.append(event)

            elif thing.type == 'tempo':
                event = MIDIEvent()
                event.type = "Tempo"
                event.time = thing.time * self.ticksPerBeat
                event.tempo = thing.tempo
                event.ord = 3
                self.MIDIEventList.append(event)

            elif thing.type == 'programChange':
                event = MIDIEvent()
                event.type = "ProgramChange"
                event.time = thing.time * self.ticksPerBeat
                event.programNumber = thing.programNumber
                event.channel = thing.channel
                event.ord = 1
                self.MIDIEventList.append(event)

            elif thing.type == 'trackName':
                event = MIDIEvent()
                event.type = "TrackName"
                event.time = thing.time * self.ticksPerBeat
                event.trackName = thing.trackName
                event.ord = 0
                self.MIDIEventList.append(event)

            elif thing.type == 'controllerEvent':
                event = MIDIEvent()
                event.type = "ControllerEvent"
                event.time = thing.time * self.ticksPerBeat
                event.eventType = thing.eventType
                event.channel = thing.channel
                event.paramerter1 = thing.parameter1
                event.ord = 1
                self.MIDIEventList.append(event)

            elif thing.type == 'SysEx':
                event = MIDIEvent()
                event.type = "SysEx"
                event.time = thing.time * self.ticksPerBeat
                event.manID = thing.manID
                event.payload = thing.payload
                event.ord = 1
                self.MIDIEventList.append(event)

            elif thing.type == 'UniversalSysEx':
                event = MIDIEvent()
                event.type = "UniversalSysEx"
                event.realTime = thing.realTime
                event.sysExChannel = thing.sysExChannel
                event.time = thing.time * self.ticksPerBeat
                event.code = thing.code
                event.subcode = thing.subcode
                event.payload = thing.payload
                event.ord = 1
                self.MIDIEventList.append(event)

            else:
                print "Error in MIDITrack: Unknown event type"
                sys.exit(2)

        # Assumptions in the code expect the list to be time-sorted.
        # self.MIDIEventList.sort(lambda x, y: x.time - y.time)

        self.MIDIEventList.sort(lambda x, y: int( 1000 * (x.time - y.time)))

        if self.deinterleave:
            self.deInterleaveNotes()

    def removeDuplicates(self):
        '''
        Remove duplicates from the eventList.

        This function will remove duplicates from the eventList. This is necessary
        because we the MIDI event stream can become confused otherwise.

        All of the events need to be hashable and comparable.
        '''

        # Get a unique set of the events...
        s = set(self.eventList)

        # ...And now sort them, primarily on time, secondarily on type.
        self.eventList = sortByTimeAndType(s)

    def closeTrack(self):
        '''Called to close a track before writing

        This function should be called to "close a track," that is to
        prepare the actual data stream for writing. Duplicate events are
        removed from the eventList, and the MIDIEventList is created.

        Called by the parent MIDIFile object.
        '''

        if self.closed == True:
            return
        self.closed = True

        if self.remdep:
            self.removeDuplicates()


        self.processEventList()

    def writeMIDIStream(self):
        '''
        Write the meta data and note data to the packed MIDI stream.
        '''

        #Process the events in the eventList

        self.writeEventsToStream()

        # Write MIDI close event.

        self.MIDIdata = self.MIDIdata + struct.pack('BBBB',0x00,0xFF, \
            0x2F,0x00)

        # Calculate the entire length of the data and write to the header

        self.dataLength = struct.pack('>L',len(self.MIDIdata))

    def writeEventsToStream(self):
        '''
        Write the events in MIDIEvents to the MIDI stream.
        '''

        for event in self.MIDIEventList:
            if event.type == "NoteOn":
                code = 0x9 << 4 | event.channel
                self.MIDIdata += writeVarLength(event.time)
                self.MIDIdata += struct.pack('>BBB', code, event.pitch,
                        event.volume)
            elif event.type == "NoteOff":
                code = 0x8 << 4 | event.channel
                self.MIDIdata += writeVarLength(event.time)
                self.MIDIdata += struct.pack('>BBB', code, event.pitch,
                        event.volume)
            elif event.type == "Tempo":
                code = 0xFF
                subcode = 0x51
                fourbite = struct.pack('>L', event.tempo)
                threebite = fourbite[1:4]       # Just discard the MSB
                self.MIDIdata += writeVarLength(event.time)
                self.MIDIdata += struct.pack('>BBB', code,
                        subcode, 0x03)
                self.MIDIdata += threebite
            elif event.type == 'ProgramChange':
                code = 0xC << 4 | event.channel
                self.MIDIdata += writeVarLength(event.time)
                self.MIDIdata += struct.pack('>BB', code, event.programNumber)
            elif event.type == 'TrackName':
                self.MIDIdata += writeVarLength(event.time)
                # Meta event 0xff, event type 0x03
                self.MIDIdata += struct.pack('>BB', 0xFF, 0x03)
                self.MIDIdata += writeVarLength(len(event.trackName))
                self.MIDIdata += event.trackName
            elif event.type == "ControllerEvent":
                code = 0xB << 4 | event.channel
                self.MIDIdata += writeVarLength(event.time)
                self.MIDIdata += struct.pack('>BBB', code, event.eventType,
                        event.paramerter1)
            elif event.type == "SysEx":
                code = 0xF0
                self.MIDIdata += writeVarLength(event.time)
                self.MIDIdata += struct.pack('>B', code)

                self.MIDIdata += writeVarLength(len(event.payload) + 2)

                self.MIDIdata += struct.pack('>B', event.manID)
                self.MIDIdata += event.payload
                self.MIDIdata += struct.pack('>B', 0xF7)
            elif event.type == "UniversalSysEx":
                code = 0xF0
                self.MIDIdata += writeVarLength(event.time)
                self.MIDIdata += struct.pack('>B', code)

                # Do we need to add a length?
                self.MIDIdata += writeVarLength(len(event.payload) + 5)

                if event.realTime:
                    self.MIDIdata += struct.pack('>B', 0x7F)
                else:
                    self.MIDIdata += struct.pack('>B', 0x7E)

                self.MIDIdata += struct.pack('>BBB', event.sysExChannel,
                    event.code, event.subcode)
                self.MIDIdata += event.payload
                self.MIDIdata += struct.pack('>B', 0xF7)

    def deInterleaveNotes(self):
        '''Correct Interleaved notes.

        Because we are writing multiple notes in no particular order, we
        can have notes which are interleaved with respect to their start
        and stop times. This method will correct that. It expects that the
        MIDIEventList has been time-ordered.
        '''

        tempEventList = []
        stack = {}

        for event in self.MIDIEventList:

            if event.type == 'NoteOn':
                if stack.has_key(str(event.pitch)+str(event.channel)):
                    stack[str(event.pitch)+str(event.channel)].append(event.time)
                else:
                    stack[str(event.pitch)+str(event.channel)] = [event.time]
                tempEventList.append(event)
            elif event.type == 'NoteOff':
                if len(stack[str(event.pitch)+str(event.channel)]) > 1:
                    event.time = stack[str(event.pitch)+str(event.channel)].pop()
                    tempEventList.append(event)
                else:
                    stack[str(event.pitch)+str(event.channel)].pop()
                    tempEventList.append(event)
            else:
                tempEventList.append(event)

        self.MIDIEventList = sortByTimeAndType(tempEventList)

    def adjustTime(self,origin):
        '''
        Adjust Times to be relative, and zero-origined
        '''

        if len(self.MIDIEventList) == 0:
            return
        tempEventList = []

        runningTime = 0

        for event in self.MIDIEventList:
            adjustedTime = event.time - origin
            event.time = adjustedTime - runningTime
            runningTime = adjustedTime
            tempEventList.append(event)

        self.MIDIEventList = tempEventList

    def writeTrack(self,fileHandle):
        '''
        Write track to disk.
        '''

        if not self.closed:
            self.closeTrack()

        fileHandle.write(self.headerString)
        fileHandle.write(self.dataLength)
        fileHandle.write(self.MIDIdata)


class MIDIHeader:
    '''
    Class to encapsulate the MIDI header structure.

    This class encapsulates a MIDI header structure. It isn't used for much,
    but it will create the appropriately packed identifier string that all
    MIDI files should contain. It is used by the MIDIFile class to create a
    complete and well formed MIDI pattern.

    '''
    def __init__(self, numTracks, ticksPerBeat):
        ''' Initialize the data structures
        '''
        self.headerString = struct.pack('cccc','M','T','h','d')
        self.headerSize = struct.pack('>L',6)
        # Format 1 = multi-track file
        self.format = struct.pack('>H',1)
        self.numTracks = struct.pack('>H',numTracks)
        self.ticksPerBeat = struct.pack('>H', ticksPerBeat)


    def writeFile(self,fileHandle):
        fileHandle.write(self.headerString)
        fileHandle.write(self.headerSize)
        fileHandle.write(self.format)
        fileHandle.write(self.numTracks)
        fileHandle.write(self.ticksPerBeat)

class MIDIFile:
    '''Class that represents a full, well-formed MIDI pattern.

    This is a container object that contains a header, one or more tracks,
    and the data associated with a proper and well-formed MIDI pattern.

    Calling:

        MyMIDI = MidiFile(tracks, removeDuplicates=True,  deinterleave=True)

        normally

        MyMIDI = MidiFile(tracks)

    Arguments:

        tracks: The number of tracks this object contains

        removeDuplicates: If true (the default), the software will remove duplicate
        events which have been added. For example, two notes at the same channel,
        time, pitch, and duration would be considered duplicate.

        deinterleave: If True (the default), overlapping notes (same pitch, same
        channel) will be modified so that they do not overlap. Otherwise the sequencing
        software will need to figure out how to interpret NoteOff events upon playback.
    '''

    def __init__(self, numTracks, ticksPerBeat=128, removeDuplicates=True,
        deinterleave=True):
        '''
        Initialize the class
        '''

        # ticksPerBeat is the number of "ticks" (time measurement in the MIDI
        # file) that corresponds to one beat. This number is somewhat
        # arbitrary, but should be chosen to provide adequate temporal
        # resolution.
        self.ticksPerBeat = ticksPerBeat

        self.header = MIDIHeader(numTracks, ticksPerBeat)

        self.tracks = list()
        self.numTracks = numTracks
        self.closed = False

        for i in range(0,numTracks):
            self.tracks.append(MIDITrack(ticksPerBeat, removeDuplicates,  deinterleave))


    # Public Functions. These (for the most part) wrap the MIDITrack functions, where most
    # Processing takes place.

    def addNote(self,track, channel, pitch,time,duration,volume):
        """
        Add notes to the MIDIFile object

        Use:
            MyMIDI.addNotes(track,channel,pitch,time, duration, volume)

        Arguments:
            track: The track to which the note is added.
            channel: the MIDI channel to assign to the note. [Integer, 0-15]
            pitch: the MIDI pitch number [Integer, 0-127].
            time: the time (in beats) at which the note sounds [Float].
            duration: the duration of the note (in beats) [Float].
            volume: the volume (velocity) of the note. [Integer, 0-127].
        """
        self.tracks[track].addNoteByNumber(channel, pitch, time, duration, volume)

    def addTrackName(self,track, time,trackName):
        """
        Add a track name to a MIDI track.

        Use:
            MyMIDI.addTrackName(track,time,trackName)

        Argument:
            track: The track to which the name is added. [Integer, 0-127].
            time: The time at which the track name is added, in beats [Float].
            trackName: The track name. [String].
        """
        self.tracks[track].addTrackName(time,trackName)

    def addTempo(self,track, time,tempo):
        """
        Add a tempo event.

        Use:
            MyMIDI.addTempo(track, time, tempo)

        Arguments:
            track: The track to which the event is added. [Integer, 0-127].
            time: The time at which the event is added, in beats. [Float].
            tempo: The tempo, in Beats per Minute. [Integer]
        """
        self.tracks[track].addTempo(time,tempo)

    def addProgramChange(self,track, channel, time, program):
        """
        Add a MIDI program change event.

        Use:
            MyMIDI.addProgramChange(track,channel, time, program)

        Arguments:
            track: The track to which the event is added. [Integer, 0-127].
            channel: The channel the event is assigned to. [Integer, 0-15].
            time: The time at which the event is added, in beats. [Float].
            program: the program number. [Integer, 0-127].
        """
        self.tracks[track].addProgramChange(channel, time, program)

    def addControllerEvent(self,track, channel,time,eventType, paramerter1):
        """
        Add a MIDI controller event.

        Use:
            MyMIDI.addControllerEvent(track, channel, time, eventType, parameter1)

        Arguments:
            track: The track to which the event is added. [Integer, 0-127].
            channel: The channel the event is assigned to. [Integer, 0-15].
            time: The time at which the event is added, in beats. [Float].
            eventType: the controller event type.
            parameter1: The event's parameter. The meaning of which varies by event type.
        """
        self.tracks[track].addControllerEvent(channel,time,eventType, paramerter1)

    def changeNoteTuning(self,  track,  tunings,   sysExChannel=0x7F,  \
                         realTime=False,  tuningProgam=0):
        """
        Change a note's tuning using SysEx change tuning program.

        Use:
            MyMIDI.changeNoteTuning(track,[tunings],realTime=False, tuningProgram=0)

        Arguments:
            track: The track to which the event is added. [Integer, 0-127].
            tunings: A list of tuples in the form (pitchNumber, frequency).
                     [[(Integer,Float]]
            realTime: Boolean which sets the real-time flag. Defaults to false.
            sysExChannel: do note use (see below).
            tuningProgram: Tuning program to assign. Defaults to zero. [Integer, 0-127]

        In general the sysExChannel should not be changed (parameter will be depreciated).

        Also note that many software packages and hardware packages do not implement
        this standard!
        """
        self.tracks[track].changeNoteTuning(tunings,   sysExChannel,  realTime,\
                                              tuningProgam)

    def writeFile(self,fileHandle):
        '''
        Write the MIDI File.

        Use:
            MyMIDI.writeFile(filehandle)

        Arguments:
            filehandle: a file handle that has been opened for binary writing.
        '''

        self.header.writeFile(fileHandle)

        #Close the tracks and have them create the MIDI event data structures.
        self.close()

        #Write the MIDI Events to file.
        for i in range(0,self.numTracks):
            self.tracks[i].writeTrack(fileHandle)

    def addSysEx(self,track, time, manID, payload):
        """
        Add a SysEx event

        Use:
            MyMIDI.addSysEx(track,time,ID,payload)

        Arguments:
            track: The track to which the event is added. [Integer, 0-127].
            time: The time at which the event is added, in beats. [Float].
            ID: The SysEx ID number
            payload: the event payload.

        Note: This is a low-level MIDI function, so care must be used in
        constructing the payload. It is recommended that higher-level helper
        functions be written to wrap this function and construct the payload if
        a developer finds him or herself using the function heavily.
        """
        self.tracks[track].addSysEx(time,manID, payload)

    def addUniversalSysEx(self,track,  time,code, subcode, payload,  \
                          sysExChannel=0x7F,  realTime=False):
        """
        Add a Universal SysEx event.

        Use:
            MyMIDI.addUniversalSysEx(track, time, code, subcode, payload,\
                                      sysExChannel=0x7f, realTime=False)

        Arguments:
            track: The track to which the event is added. [Integer, 0-127].
            time: The time at which the event is added, in beats. [Float].
            code: The even code. [Integer]
            subcode The event sub-code [Integer]
            payload: The event payload. [Binary string]
            sysExChannel: The SysEx channel.
            realTime: Sets the real-time flag. Defaults to zero.

        Note: This is a low-level MIDI function, so care must be used in
        constructing the payload. It is recommended that higher-level helper
        functions be written to wrap this function and construct the payload if
        a developer finds him or herself using the function heavily. As an example
        of such a helper function, see the changeNoteTuning function, both here and
        in MIDITrack.
        """

        self.tracks[track].addUniversalSysEx(time,code, subcode, payload,  sysExChannel,\
                                               realTime)

    def shiftTracks(self,  offset=0):
        """Shift tracks to be zero-origined, or origined at offset.

        Note that the shifting of the time in the tracks uses the MIDIEventList -- in other
        words it is assumed to be called in the stage where the MIDIEventList has been
        created. This function, however, it meant to operate on the eventList itself.
        """
        origin = 1000000 # A little silly, but we'll assume big enough

        for track in self.tracks:
                if len(track.eventList) > 0:
                    for event in track.eventList:
                        if event.time < origin:
                            origin = event.time

        for track in self.tracks:
            tempEventList = []
            #runningTime = 0

            for event in track.eventList:
                adjustedTime = event.time - origin
                #event.time = adjustedTime - runningTime + offset
                event.time = adjustedTime + offset
                #runningTime = adjustedTime
                tempEventList.append(event)

            track.eventList = tempEventList

    #End Public Functions ########################

    def close(self):
        '''Close the MIDIFile for further writing.

        To close the File for events, we must close the tracks, adjust the time to be
        zero-origined, and have the tracks write to their MIDI Stream data structure.
        '''

        if self.closed == True:
            return

        for i in range(0,self.numTracks):
            self.tracks[i].closeTrack()
            # We want things like program changes to come before notes when they are at the
            # same time, so we sort the MIDI events by their ordinality
            self.tracks[i].MIDIEventList.sort()

        origin = self.findOrigin()

        for i in range(0,self.numTracks):
            self.tracks[i].adjustTime(origin)
            self.tracks[i].writeMIDIStream()

        self.closed = True


    def findOrigin(self):
        '''Find the earliest time in the file's tracks.append.
        '''
        origin = 1000000 # A little silly, but we'll assume big enough

    # Note: This code assumes that the MIDIEventList has been sorted, so this should be insured
    # before it is called. It is probably a poor design to do this.
    # TODO: -- Consider making this less efficient but more robust by not assuming the list to be sorted.

        for track in self.tracks:
                if len(track.MIDIEventList) > 0:
                    if track.MIDIEventList[0].time < origin:
                        origin = track.MIDIEventList[0].time


        return origin

def writeVarLength(i):
    '''Accept an input, and write a MIDI-compatible variable length stream

    The MIDI format is a little strange, and makes use of so-called variable
    length quantities. These quantities are a stream of bytes. If the most
    significant bit is 1, then more bytes follow. If it is zero, then the
    byte in question is the last in the stream
    '''

    i = int(i)

    if i == 0:
        return "\x00"

    if not 0 < i <= 0xfffffff:
        raise ValueError("%d is out of range" % i)

    b = []

    # Spool bits into the array.
    while i:
        b.append(0x80 | (i & 0x7f))
        i >>= 7

    # Clear that final bit.
    b[0] &= ~0x80

    # And we're finished.
    return "".join(chr(x) for x in reversed(b))

def frequencyTransform(freq):
    '''Returns a three-byte transform of a frequencyTransform
    '''
    resolution = 16384
    freq = float(freq)
    dollars = 69 + 12 * math.log(freq/(float(440)), 2)
    firstByte = int(dollars)
    lowerFreq = 440 * pow(2.0, ((float(firstByte) - 69.0)/12.0))
    if freq != lowerFreq:
        centDif = 1200 * math.log( (freq/lowerFreq), 2)
    else:
        centDif = 0
    cents = round(centDif/100 * resolution) # round?
    secondByte = min([int(cents)>>7, 0x7F])
    thirdByte = cents - (secondByte << 7)
    thirdByte = min([thirdByte, 0x7f])
    if thirdByte == 0x7f and secondByte == 0x7F and firstByte == 0x7F:
        thirdByte = 0x7e

    thirdByte = int(thirdByte)
    return [firstByte,  secondByte,  thirdByte]

def returnFrequency(freqBytes):
    '''The reverse of frequencyTransform. Given a byte stream, return a frequency.
    '''
    resolution = 16384.0
    baseFrequency = 440 * pow(2.0, (float(freqBytes[0]-69.0)/12.0))
    frac = (float((int(freqBytes[1]) << 7) + int(freqBytes[2])) * 100.0) / resolution
    frequency = baseFrequency * pow(2.0, frac/1200.0)
    return frequency


def sortByTimeAndType(i):
    """
    Sort an iterable and return a list.

    The criteria: Primary sort by time, secondary sort by type.
    """

    return list(sorted(i, key=lambda x: (x.type, int(1000 * x.time))))


__all__ = (
    "MIDIEvent",
    "MIDIFile",
    "MIDIHeader",
    "MIDITrack",
)
