#!/usr/bin/env python
from qcml import QCML
import argparse

parser = argparse.ArgumentParser(description="Loads an external problem description to parse.")
parser.add_argument('-f', dest='file', help="file to parse", required=True)

# TODO: allow generation of code when dimensions are abstract
# group = parser.add_mutually_exclusive_group(required=True)
# group.add_argument("--ansi-c", help="generate ansi-C code; prints to command line", action="store_true")
# group.add_argument("--python", help="generate python code; prints to command line", action="store_true")

args = parser.parse_args()

print "Reading", args.file
with open(args.file, "r") as f:
    prob = f.read()
print prob

raw_input("press ENTER to parse....")
p = QCML(debug=True)
p.parse(prob)        

raw_input("press ENTER to canonicalize....")
p.canonicalize()
