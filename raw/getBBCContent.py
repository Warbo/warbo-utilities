#!/usr/bin/env python3
try:
    from BeautifulSoup import BeautifulSoup
except ImportError:
    from bs4 import BeautifulSoup
import datetime
import feedparser
import hashlib
import os
import pickle
import PyRSS2Gen
import subprocess
import sys
import time
import urllib2

msg = lambda x: sys.stderr.write(x + '\n')

###

identifier = 'story-body'
def stripCrap(html):
    """Removes navigation, sidebars, etc. from the given string of HTML."""
    parsed_html = BeautifulSoup(html)
    story       = parsed_html.body.find('div', attrs={'class':identifier})
    if story is None:
        return html

    for args in [
        {'name':'ul', 'attrs':{'class':'sharetools'}},
        {'name':'script'}
    ]:
        for crap in story.findAll(**args):
            crap.extract()

    return '<html><head/><body>{0}</body></html>'.format(repr(story))

def testStripCrap():
    import gzip
    with gzip.open(os.getenv('HTML_EXAMPLE'), 'rb') as f:
        content = f.read()
    html = BeautifulSoup(stripCrap(content))
    got  = html.body.div['class']
    assert got == [identifier], repr({
        'error'      : 'Expected top-level div to have identifier class',
        'identifier' : identifier,
        'got'        : got,
        'html'       : html})

###

def htmlToText(html):
    """Render the given string of HTML to a plain text form."""
    proc = subprocess.Popen(['html2text', '-nobs', '-ascii'],
                            stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE)
    out, _ = proc.communicate(stripCrap(html))
    return out

def testHtmlToText():
    html = '<html><head /><body><p>Hello &amp; goodbye!</p></body></html>'
    text = htmlToText(html)
    want = 'Hello & goodbye!'
    assert text.strip() == want.strip(), repr({
        'error' : 'Did not render as expected',
        'html'  : html,
        'text'  : text.strip(),
        'want'  : want.strip()})

###

cache = '/tmp/bbcnews-cached'
def getEntry(entry):
    """Fetch the page linked to by the given entry."""
    url  = entry.id
    path = cache + '/00-' + hashlib.md5(url).hexdigest()
    if os.path.exists(path):
        with open(path, 'r') as f:
            return pickle.load(f)

    msg('Fetching ' + url)
    response = urllib2.urlopen(url)
    data     = {'url'     : response.geturl(),
                'content' : response.read()}
    time.sleep(2)  # For courtesy

    if os.path.exists(cache):
        msg('Caching to ' + path)
        with open(path, 'w') as f:
            pickle.dump(data, f)
    return data

from functools import reduce
def processEntry(entry):
    """Replaces the content of entry with a rendered version of the page."""
    escape = lambda x: reduce(lambda x, pair: x.replace(*pair),
                              [('&', '&amp;'),
                               ('<', '&lt;' ),
                               ('>', '&gt;' )],
                              x)

    content       = getEntry(entry)
    entry.summary = escape(htmlToText(stripCrap(content['content'])))
    return entry

def renderToRss(feed):
    """Render feedparser data to RSS, taken from
    https://stackoverflow.com/a/191899/884682"""
    def dateOf(x):
        data = x.modified_parsed if hasattr(x, 'modified_parsed') else \
               x.published_parsed
        return datetime.datetime(*(data[:6]))

    items = [
        PyRSS2Gen.RSSItem(
            title       = x.title,
            link        = x.link,
            description = x.summary,
            guid        = x.link,
            pubDate     = dateOf(x))
        for x in feed.entries]

    rss = PyRSS2Gen.RSS2(
        title          = feed['feed'].get("title"),
        link           = feed['feed'].get("link"),
        description    = feed['feed'].get("description"),

        language       = feed['feed'].get("language"),
        copyright      = feed['feed'].get("copyright"),
        managingEditor = feed['feed'].get("managingEditor"),
        webMaster      = feed['feed'].get("webMaster"),
        pubDate        = feed['feed'].get("pubDate"),
        lastBuildDate  = feed['feed'].get("lastBuildDate"),

        categories     = feed['feed'].get("categories"),
        generator      = feed['feed'].get("generator"),
        docs           = feed['feed'].get("docs"),

        items          = items)

    return rss.to_xml()

if os.getenv('RUN_TESTS') is None:
    feed         = feedparser.parse(sys.stdin.read())
    feed.entries = filter(lambda e: '/av/' not in getEntry(e)['url'],
                          feed.entries)
    feed.entries = list(map(processEntry, feed.entries))
    print(renderToRss(feed))
else:
    testHtmlToText()
    testStripCrap()
