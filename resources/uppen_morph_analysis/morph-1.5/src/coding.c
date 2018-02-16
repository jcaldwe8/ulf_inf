/*
 * Copyright 1992, 1993 The University of Pennsylvania
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of U. of Pennsylvania not be used in 
 * advertising or publicity pertaining to distribution of the software without 
 * specific, written prior permission.  U. of Pennsylvania makes no 
 * representations about the suitability of this software for any purpose.  
 * It is provided "as is" without express or implied warranty.
 *
 * THE UNIVERSITY OF PENNSYLVANIA DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS 
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, 
 * IN NO EVENT SHALL THE UNIVERSITY OF PENNSYLVANIA BE LIABLE FOR ANY SPECIAL, 
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM 
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE 
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 */

/*  This file contains the procedures for encoding and decoding the 
 *  morphology inflectional information to be stored in a DB file.
 *  Available inflectional tags are stored in an array (found in the
 *  morph.h file).  An encoded entry consists of 1 byte containing the
 *  index into the inflectional array, 1 byte containing the number of
 *  initial letters that the keyword and entry have in common, and the remaining
 *  bytes containing the remaining letters of the entry that it does not
 *  share with the key word.  Multiple entries of a single key are separated
 *  by a hash mark ('#').  There are no spaces between entries, and no hash
 *  mark ends or begins the entries.
 *  6/1/92
 */
/* xchang changed the estring structure on 10/96, to make the coding mechanism
   satisfy the french requirement.  From one byte estring[0] to 2 bytes 
   estring[0] and estring[1], which can code estring[1]*256 + estring[0]
   features instead of only 256.  But there is a minor thing requiring attention,
   we use 255 CODE_ZERO to represent 0 for the estring[1].  10/30/96
 */

#include <stdio.h>
#ifdef BSD
#include <strings.h>
#else
#include <string.h>
#endif
#include "morph.h"

/* 
#define DEBUG
*/

/* Takes the keyword, encoded string, size of encoded string, and returns the
 * decoded string in d_string. 
 */
int 
decode(
       char *keyword, 
       unsigned char *e_string, 
       int e_string_size, 
       char *d_string
       )
{
  char hash_string[DATABUFFER];
  unsigned char *hashplace, current_place;

  strcpy(d_string, "\0");
  *hash_string = '\0';
  /* in the case of multiple entries, pull out one at a time by looking for the
     hash marks */
  while ((hashplace = strchr(e_string+3, SEP_CHAR)) != NULL)
    /*  e_string+3 to skip the infl_array encoding and the # of common letters */
    { 
      /* is the inflection encoding within the array parameters? */
      if ((e_string[0] < 0) || 
	  ((int)(e_string[0]+(e_string[1]==CODE_ZERO ? 0 : e_string[1])*256) > END_INFLECTIONS))
	fprintf(stderr, "ERROR: Cannot decode: %s \t%s\n", keyword, e_string);
      else
	{
	  /* copy over the appropriate number of characters from the key word */
	  if ((int) e_string[2] != CODE_ZERO)
	    strncat(d_string, keyword, (int) e_string[2]);
	  /* copy over any additional characters */
	  strncat(d_string, e_string+3, hashplace-e_string-3);
	  strcat(d_string, "\t");
	  /* copy over the inflectional string for the inflections array */
	  strcat(d_string, inflections[(int) e_string[0]+(e_string[1]==CODE_ZERO ? 0 : e_string[1])*256]);
	  /* since there are multiple entries, place the hash mark to separate */
	  /* from the next entry. */
	  strcat(d_string, SEP_STRING);
	  /* advance to next entry */
	  e_string = hashplace+1;
	}
    }
  /* either there is only one entry, or this is the last entry */
  if ((e_string[0] < 0) || 
      ((int)(e_string[0]+(e_string[1]==CODE_ZERO ? 0 : e_string[1])*256) > END_INFLECTIONS))
    {
      fprintf(stderr, "Cannot decode: %s \t%s\n", keyword, e_string);
      fprintf(stderr, "The code %d *256 + %d\n third:%d,The string: %s\n", 
	      (unsigned char) e_string[1], (unsigned int) e_string[0], e_string[3], e_string[4]);
    }   /* xchang 11/96 added to debug the new coding scheme */
  else 
    {  /* see comments in above section */
      if ((int) e_string[2] != CODE_ZERO)
	strncat(d_string, keyword, (int) e_string[2]);
      strncat(d_string, e_string+3, e_string_size - 3);
      strcat(d_string, "\t");
      strcat(d_string, inflections[(int)e_string[0]+(e_string[1]==CODE_ZERO ? 0 : e_string[1])*256]);
    }
}


/* This procedure takes a key word and an associated entry, and returns
 * the encoded string in estring, and the length in estring_size.  This
 * procedure encodes one entry at a time, so for multiple entries, the 
 * splitting should be done before this procedure is called.  The entry
 * should be in the form infl_info root_form, e.g. 'N sg box'.  This
 * is for compatibility with the .lex files orginally generated, and a
 * procedure 'flip' is provided in the morphlibrary to change entries 
 * e.g. 'box N sg' to the expected form.
 */
int 
encode(
       char *keyword,
       char *contentword,
       unsigned char *estring,
       int *estring_size
       )
{
  int current_size, common_length, success;
  char infl_info[DATABUFFER];
  char *rootword, *split, *rest_infl;

  unsigned int codeint=0;    /*  ;;xchang 96/10/30 */
  estring[0]=0;
  estring[1]= (unsigned char) CODE_ZERO;

  current_size = 0;
  success = 1;
  *infl_info = '\0';
  split = (char *) strrchr(contentword, '\t');  /* find the root word */
  /* separate out the inflection info */
  strncat(infl_info, contentword, strlen(contentword)- strlen(split));  
  rootword = split+1;
  rest_infl = strchr(infl_info, ' ');  
  /* Pull out the part-of-speech so that the rest of the inflection string can */
  /* be sorted.  */
  if ((rest_infl != NULL) && (strchr(rest_infl+1, ' ') != NULL))
    /* sorting kills the v/v contraction code */
    /* success = sort_infl(rest_infl+1); */
    success = success;
  if (!success) return(0);
  
  /* look up the string in the inflectional array */
  /*  if (!(estring[0] = (unsigned char) find_infl_code(infl_info))) */
  if (!(codeint = (unsigned int) find_infl_code(infl_info) ) )
    {
      fprintf(stderr, "ERROR: Could not encode inflectional string: %s %s\n",
	      rootword, infl_info);
      return(0);
    }

  /* xchang 10/96 added below 2 statements, with the codeint variable */
  estring[0] = (unsigned char) codeint % 256 ;
  if (codeint > 256)  estring[1] = (unsigned char) codeint / 256 ;
  else  estring[1] =  (unsigned char) CODE_ZERO; 

  /* Find the inital substring in common bwetween the key and rootwords,
     then place the length of the substring into the encoded_string, along
     with any remaining characters in the root word.  */
  common_length = find_common_substring(keyword, rootword);

  /* xchang 10/96, shift the estring[] one byte rightward to make room for code */
  if (common_length == 0)
    estring[2] = (unsigned char) CODE_ZERO;
  else
    estring[2] = (unsigned char) common_length;
  strcpy(estring+3, rootword+common_length);
  *estring_size = strlen(rootword)-common_length + 3;

  return(1);
  
}


/*  This procedure finds the initial common substring between the root
    and key word, and the returns the length of that string. */
int 
find_common_substring(char *keyword, char *rootword)
{
  int common_length, k_length, r_length;
  
  k_length = strlen(keyword);
  r_length = strlen(rootword);
  
  if (k_length <= r_length)
    common_length = k_length;
  else common_length = r_length;
  
  while (strncmp(keyword, rootword, common_length)) 
    common_length--;
  return(common_length);
  
}

/* This procedure uses the part of speech to restrict the search to 
   related inflections.  The start and end point for each part of 
   speech is maintained in morph.h as well.  */
int 
find_infl_code(unsigned char *infl_info)
{
  switch (infl_info[0]) 
    {
    case 'A':
      if (infl_info[1] == 'd') 
	return(get_infl_code(infl_info, ADV_START, ADV_END));         /* Adverb */
      else return(get_infl_code(infl_info, A_START, A_END));          /* Adjective */
    case 'C': 
      if (infl_info[2] == 'm') return(COMP);                          /* Complementizer */
      else return(CONJ);                                              /* Conjunction */
    case 'D': return (get_infl_code(infl_info, D_START, D_END));      /* Determiner */
    case 'I': return(INTERJ);                                         /* Interjection */
    case 'N': 
      if (infl_info[1] == 'V') 
	return(get_infl_code(infl_info, NVC_START, NVC_END));         /* Noun/Verb Contr. */
      else return (get_infl_code(infl_info, N_START, N_END));         /* Noun */
    case 'P':
      if (infl_info[2] == 'e') return(PREP);                          /* Preposition */
      else if (infl_info[1] == 'u') return (PUNCT);                   /* Punctation */
      else if (infl_info[1] == 'a') return (PART);                    /* Particle */
      else if (infl_info[3] == 'n')
	return (get_infl_code(infl_info, PRON_START, PRON_END));      /* Pronoun */
      else return(get_infl_code(infl_info, PROPN_START, PROPN_END));  /* Proper Noun */
    case 'V': 
      if (infl_info[1] == 'V')
	return(get_infl_code(infl_info, VVC_START, VVC_END));      /* Verb/Verb Contr. */
      else return (get_infl_code(infl_info, V_START, V_END));      /* Verb */
    case 'G':
      return(GENITIVE);                                        /* Genitive */
    default:                                                          /* Not Found */
      return(0);
      
    }
  
}

/* Search the inflectional array (see morph.h) for the code number of 
   the inflection string.  The start and end points of the search
   are passed in.  Within each part-of-speech, the search is simply sequential. */
int 
get_infl_code (
	       unsigned char *infl_info,
	       int start,
	       int end
	       )
{
  int current;
    
#if defined(DEBUG)
  fprintf(stderr,"%s %d %d\n",infl_info,start,end);
#endif
  current = start;
  while ((current <= end) && (strcmp(inflections[current], infl_info)))
#if defined(DEBUG)
    {
      fprintf(stderr,"%d %s %s %d\n",current,inflections[current],infl_info,
	      strcmp(inflections[current],infl_info));
      current++;
    }
#else
  current++;
#endif
  if (current > end) return(0);
  else return(current);
}

      
/* This procedure sorts the infl markers associated with each entry.
   This relieves someone putting together a database by hand from
   worrying about getting the infl markers in the correct order.
   Since the match is done with a simple ASCII comparision, it
   is important that the markers be in the same order.  The part of
   speech should already have been removed before passing the string
   onto this procedure.  The unsorted string is replaced by the sorted one. */
int sort_infl(char *infl_info)
{
  char *infl_array[MAX_INFLECTIONS];
  int infl_length[MAX_INFLECTIONS];
  char infl_holder[DATABUFFER];
  char *first_string, *second_string;
  int first_int, second_int;
  int  num_infl, sorted, need_swap, i;
  

  num_infl = 0;
  /* Set up infl_array to point to the beginning of each inflection.  Set the */
  /* corresponding entry in infl_length to its length.  There must be at least */
  /* 2 inflections, not counting the part of speech, for sort_infl to be called. */
  *(infl_array+num_infl) = infl_info;
  while ((first_string = strchr(*(infl_array+num_infl), ' ')) != NULL) 
    {
      *(infl_length+num_infl) = first_string-*(infl_array+num_infl);
      num_infl++;
      *(infl_array+num_infl) = first_string+1;
    }
  *(infl_length+num_infl) = strlen(*(infl_array+num_infl));
  
  /* Sort algorithm is designed to be very simple with little overhead since */
  /* there are so few elements in the infl_array.  If there become */
  /* significantly more, the algorithm will need to be changed.  It is a simple */
  /* Bubble Sort. */
  sorted = 0;
  while (!sorted)
    {
      sorted = 1;
      for (i = 0; i < num_infl; i++) 
	{
	  need_swap = 0;
	  first_string = *(infl_array+i);
	  second_string = *(infl_array+i+1);
	  first_int = *(infl_length+i);
	  second_int = *(infl_length+i+1);

	  if (*first_string == *second_string)
	    {
	      need_swap = strncmp(first_string,second_string, (first_int<=second_int)
				  ? first_int : second_int );
	    }
	  
	  if ((*first_string > *second_string) || (need_swap > 0))
	    { 
	      *(infl_array+i+1) = first_string;
	      *(infl_length+i+1) = first_int;
	      *(infl_array+i) = second_string;
	      *(infl_length+i) = second_int;
	      sorted = 0;
	    }
	}
    }

  /* Place the now sorted infl markers from the array back into a single */
  /* string. */
  *infl_holder = '\0';
  for (i = 0; i < num_infl; i++) 
    {
      strncat(infl_holder, *(infl_array+i), *(infl_length+i));
      strcat(infl_holder, " ");
    }
  strncat(infl_holder, *(infl_array+num_infl), *(infl_length+num_infl));
    
  /* Replace the unsorted string with the sorted one */
  strcpy(infl_info, infl_holder);
  return(1);
}
