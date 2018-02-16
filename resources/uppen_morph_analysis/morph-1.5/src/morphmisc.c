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

#include <stdio.h>
#ifdef BSD
#include <strings.h>
#else
#include <string.h>
#endif
#include <sys/file.h>
#include <errno.h>

#include "morph.h"

/* Checks possible multiple entry strings to be added against possible multiple */
/* entry strings already in the database.  Returns 0 if there is a */
/* non-duplicating string, and 1 if there is no non-duplicating string.  If */
/* there is a non-duplicating string, it is placed into nodup_string. */

int 
morph_duptest(
	      char *dbstring,
	      char *newstring,
	      char *nodup_string
	      )
{
  char one_entry[MAXCHARS];
  char *restofnew, *endhash;
  int dup_flag;
  
  *nodup_string = '\0';
  dup_flag = 1;
  if (*dbstring != '\0') 
    {
      restofnew = newstring;
      while ((endhash = strchr(restofnew+2, SEP_CHAR)) != NULL) 
	{
	  *one_entry = '\0';
	  strncat(one_entry, restofnew, endhash - restofnew);
	  if (!simple_duptest(dbstring, one_entry)) 
	    {
	      if (*nodup_string != '\0')
		strcat(nodup_string, SEP_STRING);
	      strcat(nodup_string, one_entry);
	      dup_flag = 0;
	    }
	  restofnew = endhash + 1;
	}
      if (!simple_duptest(dbstring, restofnew)) 
	{
	  if (*nodup_string != '\0')
	    strcat(nodup_string, SEP_STRING);
	  strcat(nodup_string, restofnew);
	  dup_flag = 0;
	}
    }
  else 
    {
      strcat(nodup_string, newstring);
      dup_flag = 0;
    }
  return(dup_flag);
}


/* Checks for duplicate strings of inflectional information in the database */
/* against one entry */
/* Return 1 if there is a duplicate and 0 if there is not a duplicate. */
int 
simple_duptest(char *dbstring, char *newstring)
{
  char *start, *end;

  if (*dbstring != '\0') 
    {
      start = dbstring;
      while ((end = strchr(start+2, SEP_CHAR)) != NULL)
	/* start+2 to skip over the first two bytes that encode the */
	/* inflectional array and number of letters in common.  These could */
	/* incidentally look like the SEP_CHAR */
	if (!strncmp(newstring, start, end-start))
	  return(1);
	else
	  start = end+1;
      if (!strcmp(newstring, start)) 
	return(1);
      else 
	return(0);
    }
  else 
    return(0);
}


/*  This procedure takes a string and places the first word at the end of the */
/*  string.  It is intended to `flip' a content string from "word infl_string" */
/*  (more user-oriented) to "infl_string word", as the function encode expects it. */
/*  encode expects that format due to the way lex2morph builds strings.  It was */
/*  decided that it was easier to flip the strings than to go back and redo */
/*  the way that lex2morph builds strings. */

void
flip(char *content_string)
{
  char *end;
  char temp_string[DATABUFFER];
  
  
  end = strchr(content_string, '\t');
  strcpy(temp_string, end+1);
  strcat(temp_string, "\t");
  strncat(temp_string, content_string, end-content_string);
  strcpy(content_string, temp_string);
}


/*  This procedure takes a textfile and a stats record and writes the */
/*  statistical information to that record. */

void
print_stats(FILE *fp, stats_t stat)
{
  fprintf(fp,"\n\nStatistical Information: \n\n");
  fprintf(fp,"Number of keys: %d\n", stat.word_count);
  fprintf(fp,"Number of lexicon words: %d\n", stat.entry_count);
  fprintf(fp,"Average chars in keys: %f\n", ((float)stat.key_size)/((float)stat.word_count));
  fprintf(fp,"Average chars in entries: %f\n\n\n", ((float)stat.content_size)/((float)stat.word_count));
}



/* This procedure takes a content string and performs encoding on the entire */
 /* string */
int 
encode_hashedstring(
		    char *decodedstring,
		    char *encodedstring,
		    char *keystring
		    )
{
  char no_hash[MAXCHARS];
  char encdata[MAXCHARS];
  char *starthash, *endhash;
  int hashsize, success;


  /* Encode decodedstring by encoding each set of inflectional */
  /* information between hashmarks and adding them to content.data.
  /* Don't forget to skip over the encoding for the length and inflectional */
  /* info when searching for the hash mark. */
  *encodedstring = '\0';
  starthash = decodedstring;
  success = 1;
  while ((endhash = strchr(starthash+2, SEP_CHAR)) != NULL)
    {
      *no_hash =  '\0';
      *encdata = '\0';
      strncat(no_hash, starthash, endhash-starthash);
      /* switch position of word and inflectional */
      /* info before calling encode - encode expects infl data first*/
      flip(no_hash);    
      if (encode(keystring, no_hash, encdata, &hashsize))
	{
	  strcat(encodedstring, encdata);
	  strcat(encodedstring, SEP_STRING);
	}
      else 
	success = 0;
      starthash = endhash+1;
    } /* while */
		  
  /* switch position of word and inflectional */
  /* info before calling encode - encode expects infl data first*/
  flip(starthash); 
  if (encode(keystring, starthash, encdata, &hashsize)) 
    strcat(encodedstring, encdata);
  else
    success = 0;
  return(success);
}


int 
remove_tab(char *tabbed_string, char *untabbed_string)
{
  char *tab_place;
  
  *untabbed_string = '\0';
  tab_place = strchr(tabbed_string, '\t');
  strncat(untabbed_string, tabbed_string, tab_place - tabbed_string);
  strcat(untabbed_string, "   ");
  strcat(untabbed_string, tab_place+1);
  return(1);
}

int 
replace_tab(char *untabbed_string, char *tabbed_string)
/*  Returns 0 if not successful;
            1 if successful       */
{
  char *tab_place;
  char *current_place;
  
  *tabbed_string = '\0';
  current_place = untabbed_string;

  while (*tabbed_string == '\0')
    {
      tab_place = strchr(current_place, ' ');
      if (tab_place == NULL)
	return(0);
      if ((*(tab_place+1) == ' ') && (*(tab_place + 2) == ' '))
	{
	  strncat(tabbed_string, untabbed_string, tab_place - untabbed_string);
	  strcat(tabbed_string, "\t");
	  strcat(tabbed_string, tab_place + 3);
	}
      else
	current_place = tab_place + 1;
    }
  return(1);
}


int *
morph_uniq_numstring(char *uniq_string)
/* Returns a uniq number string based on the process id (it will be the
process id reversed) */
{
  int process_num, i;
  char *tempstring;
  

  i = 0;
  *uniq_string = '\0';
  process_num = getpid();

  do 
    {
      uniq_string[i++] = process_num % 10 + '0';
    } while ((process_num /= 10) > 0);
  uniq_string[i] = '\0';
  return (1);
}
