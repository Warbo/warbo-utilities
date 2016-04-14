#!/usr/bin/env python

from __future__ import print_function
import re
from shutil     import move
from subprocess import check_output
from os         import listdir, makedirs
from os.path    import basename, dirname, exists, isfile, isdir
import sys


def msg(m):
    print(m, file=sys.stderr)

def do_move(src, dest):
    print("mv " + shellquote(src) + " " + shellquote(dest))

def shellquote(s):
    return "'" + s.replace("'", "'\\''") + "'"

# Read in cached CRCs
crcmap = {}
try:
    with open('.crcs', 'r') as f:
        for line in f.read().splitlines():
            bits = line.split("\t")
            if len(bits) == 2:
                crcmap[bits[1]] = bits[0]
            else:
                msg("Dodgy line in .crcs: " + line)
except IOError:
    msg("No .crcs cache found")

def get_crc(path):
    # Use cached version if available
    if path in crcmap:
        return crcmap[line]

    # Calculate CRC
    msg("Calculating CRC of " + path)
    output = check_output(["avconv", "-i", path, "-f", "crc", "-"])
    crc    = filter(lambda l: "CRC" in l, output.splitlines())[0]

    # Cache for future reference
    crcs[path] = crc
    with open(".crcs", "a") as f:
        f.write(crc + "\t" + path + "\n")

    return crc

def compare_files(f1, f2):
    if not isfile(f1):
        msg("Can't compare non-existent '" + f1 + "' to '" + f2 + "'")
        sys.exit(1)
    if not isfile(f2):
        msg("Can't compare non-existent '" + f2 + "' to '" + f1 + "'")
        sys.exit(1)

    is_audio = False
    lower1 = f1.lower()
    lower2 = f2.lower()

    for ext in ["mp3", "wma", "aac", "ogg", "m4a"]:
        if lower1.endswith(ext) and lower2.endswith(ext):
            is_audio = True

    if is_audio:
        src = get_crc(f1)
        dst = get_crc(f2)
        if src == dst:
            print(f1 + " is a duplicate of " + f2)
            if isdir("DUPES"):
                d = dirname(f2)
                fname = basename(f2)
                makedirs("DUPES/" + d)
                do_move(f2, "DUPES/" + d + "/" + fname)
        else:
            print(f1 + " doesn't match CRC of " + f2)
    else:
        print("Path '" + f1 + "' looks like a dupe of '" + f2 + "'")

# Read lines from stdin like "COMPARE\tfoo\tbar" and compare foo with bar
for line in sys.stdin:
    if line.startswith("COMPARE"):
        bits = line.split('\t')
        if len(bits) == 3:
            compare_files(bits[1], bits[2][:-1]) # Chomp newline
        else:
            msg("Dodgy stdin line: " + line)
