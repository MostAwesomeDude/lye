import sys

from lye.timelyne import Timelyne
from lye.library import Library

f = open(sys.argv[1], "rb")
library = Library("groove")
Timelyne.from_file(library, f.read())
