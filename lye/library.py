from glob import iglob
import os.path

from lye.melody import melody_from_ly

class Snippet(object):
    """
    A file with notes in it.
    """

    def __init__(self, filename):
        self.filename = filename

    def melody(self):
        with open(self.filename, "rb") as f:
            s = f.read()
            return melody_from_ly(s)

class Library(object):
    """
    A folder with snippets.
    """

    def __init__(self, path):
        self.path = os.path.abspath(path)

    def snippets(self):
        d = {}
        i = iglob(os.path.join(self.path, "*.snippet"))
        for fullname in i:
            name = os.path.splitext(os.path.basename(fullname))[0]
            d[name] = Snippet(fullname)
        return d
