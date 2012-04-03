import sys

from lye.timelyne import Timelyne
from lye.library import Library

f = open(sys.argv[1], "rb")
library = Library("groove")
lyne = Timelyne.from_lines(library, f)
