"""
Gene Kim, 2/16/2018
Python 2.7

Python utility functions for generating regex patterns from intuitive classes
(e.g. lexical resource lists).
"""

# TODO: add a way to signal that the tense doesn't matter
# TODO: add a way to signal that capitalization doesn't matter (either for first letter or for all or uniform)

import sys
import re

SENT_END_PUNCT = [".", "!", "?", "!!", "!!!", "!?"]

DEFAULT_DEFS = {
  "<endpunct>" : SENT_END_PUNCT
    }

# Generates a regex alternatives expression from the given list.
#   strlst: ["a", "b", "c"]
#   return: "a|b|c"
# NB: The strings are escaped so they are matched exactly.
def regex_alternatives(strlst):
  return '|'.join([re.escape(s) for s in strlst])

# Generates the regex value.
# If the value is a 
#   [str] -> alternatives of escaped versions of the strings
#   str   ->
#     starts with "<regex>" -> literal version of string with "<regex>" stripped
#     otherwise -> escaped version of the string
#   otherwise -> undefined ; TODO: add other types as appropriate.
def def_value_gen(val):
  if type(val) == list:
    return regex_alternatives(val)
  elif type(val) == str:
    if val.startswith("<regex>"):
      return val[7:]
    else:
      return re.escape(val)
  else:
    raise ValueError


# Generates a regular expression from a regex schema and definition.
#   schema : schema for a regular expression
#   defs   : definition for replacements in the schema (key -> value)
#   use_defaults : whether to use the default defs if possible
#
# Substrings in the schema that match the definition keys are replaced by the
# regex associated with the value.  The replacements are done in order of defs and
# left-to-right in the schema until exhaustion.
def gen_regex(schema, defs, use_defaults=False):
  for k, v in defs.iteritems():
    if schema.find(k) > -1:
      return gen_regex(schema.replace(k, def_value_gen(v), 1), defs, use_defaults)
  if use_defaults:
    for k, v in DEFAULT_DEFS.iteritems():
      if schema.find(k) > -1:
        return gen_regex(schema.replace(k, def_value_gen(v), 1), defs, use_defaults)
  return schema

