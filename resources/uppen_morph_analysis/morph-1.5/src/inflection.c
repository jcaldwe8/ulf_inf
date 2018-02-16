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

/*  This file contains the procedures for automatically creating the standard */
/*  inflections of Nouns, Proper Nouns, Verbs, and Adjectives.  These routines */
/*  are used by xmdbm, the Morphology Database Maintenance program.  They are */
/*  called when *STANDARD INFLECTION* is chosen as an option when adding new */
/*  information to the file.  */

#include "morph.h"

/*  This procedure creates the 4 standard inflected forms of a Noun - 3sg, 3sg */
/*  GEN, 3pl, 3pl GEN.  The baseword is the word that is inflected, which is   */
/*  normally the same as the root word, but not always.  This allows for       */
/*  alternate spellings, as well as hyphenatation, to have the same rootword   */
/*  while inflecting the surface form correctly.  Special provisions are made  */
/*  for the following classes of words:                                    */
/*    1) end in 's'                                                            */
/*    2) end in 'y', preceded by a consonant.                                  */
int create_noun_inflections(baseword, rootword, keyarray, contentarray)
     char *baseword;
     char *rootword;
     char keyarray[MAX_STD_INFL][DATABUFFER];
     char contentarray[MAX_STD_INFL][DATABUFFER];
{
  char *endofstring;
  
  endofstring = baseword+strlen(baseword) - 1;

  /* Initialize the strings, because strcpy doesn't put '\0' on a string */
  *keyarray[0] = '\0';
  *keyarray[1] = '\0';
  *keyarray[2] = '\0';
  *keyarray[3] = '\0';
  *contentarray[0] = '\0';
  *contentarray[1] = '\0';
  *contentarray[2] = '\0';
  *contentarray[3] = '\0';

  /*  3rd singular */
  strcat(keyarray[0], baseword);
  strcat(contentarray[0], rootword);
  strcat(contentarray[0], "\tN 3sg");
  
  /* 3rd singular Genitive */
  strcat(keyarray[1], baseword);
  strcat(keyarray[1], "'s");
  strcat(contentarray[1], rootword);
  strcat(contentarray[1], "\tN 3sg GEN");
  
  /* 3rd plural and 3rd plural Genitive */
  if (*endofstring == 's')
    {                            /* ends in 's' */
      strcat(keyarray[2], baseword);
      strcat(keyarray[2], "es");
      strcat(keyarray[3], baseword);
      strcat(keyarray[3], "es'");
    }
  else if ((*endofstring == 'y') && (!strchr("aeiou", *(endofstring -1))))  
    {                            /* ends in Consonant - 'y' */
      strncat(keyarray[2], baseword, strlen(baseword)-1);
      strcat(keyarray[2], "ies");
      strncat(keyarray[3], baseword, strlen(baseword)-1);
      strcat(keyarray[3], "ies'");
    }
  else
    {
      strcat(keyarray[2], baseword);
      strcat(keyarray[2], "s");
      strcat(keyarray[3], baseword);
      strcat(keyarray[3], "s'");
    }
  strcat(contentarray[2], rootword);
  strcat(contentarray[2], "\tN 3pl");
  strcat(contentarray[3], rootword);
  strcat(contentarray[3], "\tN 3pl GEN");

  return(4);
}


/*  This procedure creates the 3 standard inflected forms of an Adjective -    */
/*  normal, comparative, and superlative..  The baseword is the word that is   */
/*  inflected, which is normally the same as the root word, but not always.    */
/*  This allows for alternate spellings, as well as hyphenatation, to have the */
/*  same rootword while inflecting the surface form correctly.  Special        */
/*  provisions are made for the following classes of words:                */
/*    1) end in 'e'                                                            */
/*    2) end in 'y', preceded by a consonant.                                  */
int create_adj_inflections(baseword, rootword, keyarray, contentarray)
     char *baseword;
     char *rootword;
     char keyarray[MAX_STD_INFL][DATABUFFER];
     char contentarray[MAX_STD_INFL][DATABUFFER];
{
  char *endofstring;
  
  endofstring = baseword+strlen(baseword) - 1;

  /* Intialize the strings, because strcpy doesn't put '\0' on a string */
  *keyarray[0] = '\0';
  *keyarray[1] = '\0';
  *keyarray[2] = '\0';
  *contentarray[0] = '\0';
  *contentarray[1] = '\0';
  *contentarray[2] = '\0';

  /* Regular adjective */
  strcat(keyarray[0], baseword);
  strcat(contentarray[0], rootword);
  strcat(contentarray[0], "\tA");
  
  /* Comparative and superlative adjectives */
  if (*endofstring == 'e') 
    {
      strcat(keyarray[1], baseword);
      strcat(keyarray[1], "r");
      strcat(keyarray[2], baseword);
      strcat(keyarray[2], "st");
    }
  else if ((*endofstring == 'y') && (!strchr("aeiou", *(endofstring -1))))
    {
      strncat(keyarray[1], baseword, strlen(baseword) - 1);
      strcat(keyarray[1], "ier");
      strncat(keyarray[2], baseword, strlen(baseword) - 1);
      strcat(keyarray[2], "iest");
    }
  else
    {
      strcat(keyarray[1], baseword);
      strcat(keyarray[1], "er");
      strcat(keyarray[2], baseword);
      strcat(keyarray[2], "est");
    }
  strcat(contentarray[1], rootword);
  strcat(contentarray[1], "\tA COMP");
  strcat(contentarray[2], rootword);
  strcat(contentarray[2], "\tA SUPER");

  return(3);
}

/*  This procedure creates the 4 standard inflected forms of a Proper Noun - 3sg,  */
/*  3sg GEN, 3pl, 3pl GEN.  The baseword is the word that is inflected, which is   */
/*  normally the same as the root word, but not always.  This allows for       */
/*  alternate spellings, as well as hyphenatation, to have the same rootword   */
/*  while inflecting the surface form correctly.  Special provisions are made  */
/*  for the following classes of words:                                    */
/*    1) end in 's'                                                            */
int create_propn_inflections(baseword, rootword, keyarray, contentarray)
     char *baseword;
     char *rootword;
     char keyarray[MAX_STD_INFL][DATABUFFER];
     char contentarray[MAX_STD_INFL][DATABUFFER];
     
{
  char *endofstring;
  
  endofstring = baseword+strlen(baseword) - 1;

  /* Intialize the strings, because strcpy doesn't put '\0' on a string */
  *keyarray[0] = '\0';
  *keyarray[1] = '\0';
  *keyarray[2] = '\0';
  *keyarray[3] = '\0';
  *contentarray[0] = '\0';
  *contentarray[1] = '\0';
  *contentarray[2] = '\0';
  *contentarray[3] = '\0';

  /*  3rd singular */
  strcat(keyarray[0], baseword);
  strcat(contentarray[0], rootword);
  strcat(contentarray[0], "\tPropN 3sg");
  
  /* 3rd singular Genitive */
  strcat(keyarray[1], baseword);
  strcat(keyarray[1], "'s");
  strcat(contentarray[1], rootword);
  strcat(contentarray[1], "\tPropN 3sg GEN");
  
  /* 3rd plural and 3rd plural Genitive */
  if (*endofstring == 's')
    {                            /* ends in 's' */
      strcat(keyarray[2], baseword);
      strcat(keyarray[2], "es");
      strcat(keyarray[3], baseword);
      strcat(keyarray[3], "es'");
    }
  else
    {
      strcat(keyarray[2], baseword);
      strcat(keyarray[2], "s");
      strcat(keyarray[3], baseword);
      strcat(keyarray[3], "s'");
    }
  strcat(contentarray[2], rootword);
  strcat(contentarray[2], "\tPropN 3pl");
  strcat(contentarray[3], rootword);
  strcat(contentarray[3], "\tPropN 3pl GEN");

  return(4);
}

/*  This procedure creates the 5 standard inflected forms of a Verb - Infinitive,  */
/*  Progressive, 3sg Present, Past Weak, and Past Particle Weak.  The baseword */
/*  is the word that is inflected, which is normally the same as the root      */
/*  word, but not always.  This allows for alternate spellings, as well as     */
/*  hyphenatation, to have the same rootword while inflecting the surface form */
/*  correctly.  Special provisions are made for the following classes of words:*/
/*    1) end in 's'                                                            */
/*    2) end in 'e'                                                            */
/*    3) end in 'y', preceded by a consonant                                   */
int create_verb_inflections(baseword, rootword, keyarray, contentarray)
     char *baseword;
     char *rootword;
     char keyarray[MAX_STD_INFL][DATABUFFER];
     char contentarray[MAX_STD_INFL][DATABUFFER];
{
  char *endofstring;
  
  endofstring = baseword+strlen(baseword) - 1;

  /* Intialize the strings, because strcpy doesn't put '\0' on a string */
  *keyarray[0] = '\0';
  *keyarray[1] = '\0';
  *keyarray[2] = '\0';
  *keyarray[3] = '\0';
  *keyarray[4] = '\0';
  *contentarray[0] = '\0';
  *contentarray[1] = '\0';
  *contentarray[2] = '\0';
  *contentarray[3] = '\0';
  *contentarray[4] = '\0';

  /* Infinitive form */
  strcat(keyarray[0], baseword);
  strcat(contentarray[0], rootword);
  strcat(contentarray[0], "\tV INF");
  
  /* Progressive, 3sg Present, Past Tense, and Past Participle */
  if (*endofstring == 'e')
    {
      strncat(keyarray[1], baseword, strlen(baseword) - 1);
      strcat(keyarray[1], "es");
      strncat(keyarray[2], baseword, strlen(baseword) - 1);
      strcat(keyarray[2], "ing");
      strncat(keyarray[3], baseword, strlen(baseword) - 1);
      strcat(keyarray[3], "ed");
      strncat(keyarray[4], baseword, strlen(baseword) - 1);
      strcat(keyarray[4], "ed");
    }
  else if (*endofstring == 's')
    {
      strcat(keyarray[1], baseword);
      strcat(keyarray[1], "es");
      strcat(keyarray[2], baseword);
      strcat(keyarray[2], "ing");
      strcat(keyarray[3], baseword);
      strcat(keyarray[3], "ed");
      strcat(keyarray[4], baseword);
      strcat(keyarray[4], "ed");
    }
  else if ((*endofstring == 'y') && (!strchr("aeiou", *(endofstring -1))))
    {
      strncat(keyarray[1], baseword, strlen(baseword) - 1);
      strcat(keyarray[1], "ies");
      strncat(keyarray[2], baseword, strlen(baseword) - 1);
      strcat(keyarray[2], "ying");
      strncat(keyarray[3], baseword, strlen(baseword) - 1);
      strcat(keyarray[3], "ied");
      strncat(keyarray[4], baseword, strlen(baseword) - 1);
      strcat(keyarray[4], "ied");
    }
  else
    {
      strcat(keyarray[1], baseword);
      strcat(keyarray[1], "s");
      strcat(keyarray[2], baseword);
      strcat(keyarray[2], "ing");
      strcat(keyarray[3], baseword);
      strcat(keyarray[3], "ed");
      strcat(keyarray[4], baseword);
      strcat(keyarray[4], "ed");
    }
  
  strcat(contentarray[1], rootword);
  strcat(contentarray[1], "\tV 3sg PRES");
  strcat(contentarray[2], rootword);
  strcat(contentarray[2], "\tV PROG");
  strcat(contentarray[3], rootword);
  strcat(contentarray[3], "\tV PAST WK");
  strcat(contentarray[4], rootword);
  strcat(contentarray[4], "\tV PPART WK");

  return(5);
}
