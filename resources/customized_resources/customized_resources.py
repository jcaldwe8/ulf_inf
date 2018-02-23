"""
Python 2.7

Python interface to the customized language resources.

Currently available resources:
  Implicative list from the 2009 paper by Karl Stratos (with minor edits)
  English lemma to verb form list by Yasumasa Someya (plus custom additions) 

TODO: make this the same interface as original resources and use a flag to switch between them.
"""

import os

STRATOS_IMPL_LIST_FILE = "stratos-impl-list-corrected-2-8-2018.txt"
E_LEMMA_FILE = "e_lemma_reformatted.txt"
E_LEMMA_CUSTOM_FILE= "gene_custom_e_lemma_list.txt"

# Concerts the relative path, p, into an absolute path relative to the location
# of this script.  This is used assuming the immediate file structure will be
# static, whereas the location of the caller may vary since this script is
# intended to be a module.
def local_path(p):
  return os.path.join(os.path.dirname(__file__), p)

# Loads a lexical resource according to given absolute filepath, given
# delimiter for an entry, entry parse function, and line commenting string.
# The comments are filtered first at a line level, after which the file is 
# split according to the delim paramter and parsed using the parsefn.
# Returns a list of parsed entries.
#
# Comments are assumed to start the line.
# prestrip : whether or not to strip the entries before parsefn
# filterfn : boolean function for pre-parsefn texts (called after stripping if prestrip=True).
def load_resource(filepath, delim="\n", parsefn=lambda x: x, \
    comment="//", prestrip=False, filterfn=lambda x: True):
  lines = file(filepath, 'r').read().splitlines()
  lines = [l for l in lines if not l.startswith(comment)]
  f = "\n".join(lines)
  raw_entries = f.split(delim)
  if prestrip:
    raw_entries = [e.strip() for e in raw_entries]
  entries = [parsefn(re) for re in raw_entries if filterfn(e)]
  return entries

# Predefined filter function for empty strings.
def emptystr_filterfn(string):
  return string != ''

# Load the implicative list by Karl Stratos (or a list with similar structure).
def load_stratos_impl_list():
  # Read in entries -- default parsefn simply returns the string.
  return load_resource(local_path(STRATOS_IMPL_LIST_FILE), comment=";", \
      prestrip=True, filterfn=emptystr_filterfn)

# Load the English lemma dictionary that maps from a lemma to its derivations.
# Based on the 1998 list written by Yasumasa Someya. 
# Format of each entry (line):
#   [key] -> [value1],[value2],...,[valuen]
def load_e_lemma_dictionary():
  # Line parser.
  def parse_e_lemma_line(l):
    k, v = l.split(" -> ")
    return (k, v.split(","))

  # Load raw resources into lists.
  orig_entries = load_resource(local_path(E_LEMMA_FILE), \
      parsefn=parse_e_lemma_line, comment=";", prestrip=True, \
      filterfn=emptystr_filterfn)
  custom_entries = load_resource(local_path(E_LEMMA_CUSTOM_FILE), \
      parsefn=parse_e_lemma_line, comment=";", prestrip=True, \
      filterfn=emptystr_filterfn)
  # Combine resources and turn into a dict.
  orig_entries.extend(custom_entries)
  return dict(orig_entries)

# Simple flatten function.
def flatten(ll):
  return [item for sublist in ll for item in sublist]

# Load list of all derivational forms of the implicatives.
#   Takes the stratos implicatives list and expands it to include the e_lemma
#   list derivations.
# TODO: update to uppen or delete
def load_stratos_impl_forms():
  impl_words = load_stratos_impl_list()
  elemma_map = load_e_lemma_dictionary()
  impl_forms = flatten([elemma_map[w] for w in impl_words if w in elemma_map])
  impl_words.extend(impl_forms)
  return impl_words


