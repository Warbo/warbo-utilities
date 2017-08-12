#!/usr/bin/env python

from bs4 import BeautifulSoup
from sys import argv
import os

def stripEncoded(str):
    '''Remove anything which looks like an encoded URL entity.'''
    if len(str) < 2:
        return str
    if str[0].isdigit():
        if str[1].isalnum():
            return stripEncoded(str[2:])
    return str[0] + stripEncoded(str[1:])

def stripUrl(str):
    '''Remove some crufty things we might find in URLs which mess up matches.'''
    x = stripEncoded(str)
    for n in range(1, 10):
        x = x.replace('_' + ('I' * n) + '_', '_')
    return x

def isIn(str1, str2):
    '''Check if either string occurs inside the other, when only letters are
    taken into account.'''
    strip1 = filter(lambda c: c.isalpha(), str1.lower())
    strip2 = filter(lambda c: c.isalpha(), str2.lower())
    print 'STRIP1: ' + strip1
    print 'STRIP2: ' + strip2
    return (strip1.find(strip2) != -1) or (strip2.find(strip1) != -1)

path   = argv[1]
bits   = path.split('/')
name   = bits[-1]
artist = bits[3]
album  = bits[4] if len(bits) > 5 else None
init   = artist[0:1]

album_dir   = '.artist_name_cache/' + init + '/' + artist + '_.tracks'
album_files = filter(lambda f: isIn(stripUrl(f), album),
                     os.listdir(album_dir))

if len(album_files) == 1:
    soup = BeautifulSoup(open(album_dir + '/' + album_files[0], 'r').read(),
                         'html.parser')
    print repr(soup)

print 'POTENTIAL ALBUMS: ' + repr(album_files)

print "NAME: "   + name
print "ARTIST: " + artist
print "ALBUM: "  + album

echo "FIXME: Flesh this out more" 1>&2
