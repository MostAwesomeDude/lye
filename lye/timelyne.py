from pymeta.grammar import OMeta
from pymeta.runtime import ParseError

from lye.instruments import instruments as midi_instruments

def find_instrument(name):
    """
    Attempt to fully qualify a MIDI instrument name.
    """

    name = name.lower()
    found = []

    for instrument in midi_instruments:
        if instrument.startswith(name):
            found.append(instrument)

    if len(found) < 1:
        raise Exception("Couldn't match any instruments for %s" % name)
    elif len(found) > 1:
        raise Exception("Found multiple instruments for %s: %s"
            % (name, found))
    return found[0]

LYNE, INSTRUMENT = range(2)

grammar = """
punctuation ::= '(' | ')'
word ::= (<letterOrDigit> | <punctuation>)+:w => "".join(w)
bareWord ::= <spaces> <word>:w (<spaces> <word>)*:ws
           => " ".join([w] + ws).strip()

extras ::= (<token "|"> <bareWord>)*

instrument ::= <token ">"> <bareWord>:word <extras>:words
       => INSTRUMENT, [word] + words

lyne ::= <token "&"> <bareWord>:word <extras>:words
       => LYNE, [word] + words

timelyne ::= (<instrument> | <lyne>)+
"""

class LyneGrammar(OMeta.makeGrammar(grammar, globals())):
    pass

class Timelyne(object):
    """
    A song assembled from Lye snippets.
    """

    def __init__(self, library):
        self.channels = [[] for chaff in range(16)]
        self.library = library

    @classmethod
    def from_file(cls, library, data):
        """
        Parse a song from a file.
        """

        self = cls(library)
        try:
            g = LyneGrammar(data)
            parsed, error = g.apply("timelyne")
        except ParseError, pe:
            raise Exception("Couldn't parse: %s" % pe.formatError(data))
        else:
            print "Parsed!"
            print error

        for directive, tokens in parsed:
            if directive == INSTRUMENT:
                self.set_instruments(tokens)
            elif directive == LYNE:
                self.add_lynes(tokens)

        return self

    def set_instruments(self, instruments):
        for i, instrument in enumerate(instruments):
            instrument = find_instrument(instrument)
            print "Setting %d to %s" % (i, instrument)
            self.channels[i].append((INSTRUMENT, instrument))

    def add_lynes(self, names):
        for i, name in enumerate(names):
            snippet = self.library.snippets()[name]
            melody = snippet.melody()
            print "Setting %d to %s" % (i, melody)
            self.channels[i].append((LYNE, melody))
