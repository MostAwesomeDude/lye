import re

from pyDatalog.pyDatalog import Datalog_engine

pl = Datalog_engine()

pl.load("""
+ legato(acoustic_guitar__nylon_)
+ legato(acoustic_guitar__steel_)
+ legato(electric_guitar__jazz_)
+ legato(electric_guitar__clean_)
+ legato(electric_guitar__muted_)
+ legato(overdriven_guitar)
+ legato(distorted_guitar)
""")

def can_legato(instrument):
    i = re.sub("[() ]", "_", instrument)
    return bool(pl.ask("legato(%s)" % i))
