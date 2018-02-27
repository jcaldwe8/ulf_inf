"""
Python 2.7, Gene Kim, 2/26/2018

Downloads the top 100 list of books from Project Gutenberg for the list of
interest in plain text form.
"""

import argparse
from HTMLParser import HTMLParser
from subprocess import call
import urllib2

ROOT_PAGE = "https://www.gutenberg.org"
TOP_100_PAGE = "/browse/scores/top"
FLAG_TO_NUM = {"day" : "1", "week" : "7", "month" : "30"}

# Parses arguments and returns the result of parser.parse_args() from argparse.
def parse_arguments():
  parser = argparse.ArgumentParser(description='Downloads the top 100 book list from Project Gutenberg in plain text form..')
  parser.add_argument('-ltype', '--listtype', choices=['day', 'week', 'month'], required=True)
  parser.add_argument('-tpath', '--targetpath', type=str, required=True)
  args = parser.parse_args()
  return args

args = parse_arguments()

# Class for parsing the paths for the top 100 books. The relative paths are 
# stored in the output init argument.
class TopGutenbergParser(HTMLParser):
  def __init__(self, output):
    HTMLParser.__init__(self)
    self.__recording = False # Flag to indicate if we're in the list that we want to record.
    self.__out = output

  def handle_starttag(self, tag, attrs):
    if tag == "h2":
      for name, value in attrs:
        if name == "id" and value == "books-last{}".format(FLAG_TO_NUM[args.listtype]):
          self.__recording = True
        else:
          self.__recording = False
    if tag == "a" and self.__recording:
      for name, value in attrs:
        if name == "href" and value[:8] == "/ebooks/":
          self.__out.append(value)

# Same as TopGutenbergParser, but saves the relative path to the exact text
# file for the given book page.
class GutenbergBookParser(HTMLParser):
  def __init__(self, output):
    HTMLParser.__init__(self)
    self.__out = output

  def handle_starttag(self, tag, attrs):
    if tag == "a":
      for name, value in attrs:
        if name == "href" and \
            (value[-4:] == ".txt" or value[-10:] == ".txt.utf-8"):
          self.__out.append(value)

# Returns the text file path for the relative path of the book info page.
def get_textfilepath(relpath):
  print "Finding textfile for " + relpath
  text = urllib2.urlopen(ROOT_PAGE + relpath).read()
  results = []
  gbparser = GutenbergBookParser(results)
  gbparser.feed(text)
  assert len(results) <= 1, "Multiple text files found for single book\npage: {}\nresults: {}".format(ROOT_PAGE + relpath, results)
  if len(results) == 0:
    print "No text file found for: {}".format(ROOT_PAGE + relpath)
    return None
  else:
    return results[0]


# Get book list.
print "Extracting book paths from list..."
toppagetxt = urllib2.urlopen(ROOT_PAGE + TOP_100_PAGE).read()
bookpaths = []
tgparser = TopGutenbergParser(bookpaths)
tgparser.feed(toppagetxt)

# Find the text file for the book pages.
print "Extracting textfile paths from book pages..."
booktextpaths = [get_textfilepath(bp) for bp in bookpaths]

# Download all the text files.
print "Downloading books..."
for btp in booktextpaths:
  if btp:
    call(["wget", "https:" + btp, "-P", args.targetpath])

