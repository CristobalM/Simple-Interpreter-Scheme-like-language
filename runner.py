#!/usr/bin/python
from simple_interpreter import run
import sys


if len(sys.argv) > 1:
    s = str(sys.argv[1])
    ss = s.replace("\n", "")
    ss = ss.replace("\r", "")
    aux = ss
    while aux != ss:
        aux = ss
        ss = ss.replace("  ", " ")
    print '"' + run(ss) + '"'
else:
    print "No hay para interpretar"
