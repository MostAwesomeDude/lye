import sys

from lye.timelyne import Timelyne
from lye.library import Library

library = Library("groove")

with open(sys.argv[1], "rb") as f:
    lyne = Timelyne.from_lines(library, f)
    data = lyne.to_midi()
    with open(sys.argv[2], "wb") as out:
        out.write(data)
