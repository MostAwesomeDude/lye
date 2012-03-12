from collections import namedtuple

class Marker(object):
    """
    A singleton representing a measure marker.
    """

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "Marker(%r)" % self.name

    __str__ = __repr__

    def __eq__(self, other):
        return self.name == getattr(other, "name", None)

Note = namedtuple("Note", "pitch, begin, duration")
