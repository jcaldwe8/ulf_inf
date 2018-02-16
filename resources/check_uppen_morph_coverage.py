"""
Python 2.7

"""

import sys

# Paths to local modules.
UPPEN_MORPH_PATH = "uppen_morph_analysis"
CUSTOMIZED_RESOURCE_PATH = "customized_resources"

# Load local functions.
sys.path.append(UPPEN_MORPH_PATH)
from uppen_morph import load_morph_file, filter_morph_data

sys.path.append(CUSTOMIZED_RESOURCE_PATH)
from customized_resources import load_stratos_impl_list, load_e_lemma_dictionary, load_stratos_impl_forms


# Checks via the following three step process.
# Checks coverage of the Uppen Morph dataset over the Stratos implicative verbs +
# e_lemma derivations.  e_lemma derivations have full coverage over the
# implicatives list, but does not specify what sort of derivation is being
# performed.  The Uppen Morph dataset on the other hand seems to have either
# incorrect or a differing interpretation of lemma, so we cannot directly use the
# source lemma in that dataset.
# 
# 1. Load a mapping from e_lemma derivations to Stratos implicatives.
# 2. Load the Uppen Morph dataset and get all back-pointers through the map constructed in 1.
# 3. Check for coverage between the back-pointers and original Stratos implicatives.
def check_v1():
  # 1. Load a mapping from e_lemma derivations to Stratos implicatives.
  implst = load_stratos_impl_list()
  elmap = load_e_lemma_dictionary()
  
  deriv_to_imp = { impder : imp for imp in implst if imp in elmap for impder in elmap[imp] }
  
  # 2. Load the Uppen Morph dataset and get all back-pointers through the map constructed in 1.
  # NB: we just care about PAST and PPART verbs so filter by those...
  morphs = load_morph_file()
  pastmorphs = filter_morph_data(morphs, poses=["V"], features=["PAST", "PPART"])
  pasttokens = [m.token for m in pastmorphs]
  
  um_past_imps = [deriv_to_imp[pt] for pt in pasttokens if pt in deriv_to_imp]
  
  # 3. Check for coverage between the back-pointers and original Stratos implicatives.
  print "Implicatives without past/past participle derivations in Uppen Morph"
  i = 0
  for st_imp in implst:
    if st_imp not in um_past_imps:
      print st_imp
      i += 1
  print "{} out of {} implicatives not covered".format(i, len(implst))

# Checks using only the Uppen Morph data (no e_lemma data).
# After looking at the Uppen data more carefully, it seems that I read the data
# incorrectly.  I still noticed erros, but if there are few enough errors
# relating to the implicatives list, we can fix them by hand as we did with the
# e_lemma list.
def check_v2():
  # 1. Load Stratos implicatives list.
  implst = load_stratos_impl_list()

  # 2. Load in the Uppen morphology data filter to only include PAST and PPART
  # lemmas.
  morphs = load_morph_file()
  pastmorphs = filter_morph_data(morphs, poses=["V"], features=["PAST"])
  ppartmorphs = filter_morph_data(morphs, poses=["V"], features=["PPART"])
  
  # 3. Reorganize to map from the lemma to derived form.
  past_ltom = { li.lemma : m.token  for m in pastmorphs for li in m.lemmas }
  ppart_ltom = { li.lemma : m.token  for m in ppartmorphs for li in m.lemmas }

  # 4. Find all implicatives that aren't in this mapping.
  #    Also, write mapping to file to check...
  past_not_found = [imp for imp in implst if imp not in past_ltom]
  ppart_not_found = [imp for imp in implst if imp not in ppart_ltom]

  print "Stratos implicatives without PAST forms in the Uppen Morph dataset"
  for imp in past_not_found:
    print imp
  print "Stratos implicatives without PPART forms in the Uppen Morph dataset"
  for imp in ppart_not_found:
    print imp


  found_subset = ['{} -> {}'.format(l, t) for l, t in past_ltom.iteritems() if l in implst]
  out = file('past_impl_lst_uppen_mapping.temp', 'w')
  out.write('\n'.join(found_subset))
  out.close()
  found_subset = ['{} -> {}'.format(l, t) for l, t in ppart_ltom.iteritems() if l in implst]
  out = file('ppart_impl_lst_uppen_mapping.temp', 'w')
  out.write('\n'.join(found_subset))
  out.close()

if __name__ == '__main__':
  check_v2()

