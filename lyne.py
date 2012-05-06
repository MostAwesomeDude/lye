import sys

from lye.timelyne import Timelyne
from lye.library import Library

library = Library(sys.argv[1])

with open(sys.argv[2], "rb") as f:
    lyne = Timelyne.from_lines(library, f)
    data = lyne.to_midi()
    with open(sys.argv[3], "wb") as out:
        out.write(data)
