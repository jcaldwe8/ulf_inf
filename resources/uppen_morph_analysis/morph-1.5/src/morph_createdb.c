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

/*     
 *  This file takes a '.flat' file and converts it into a database file.
 *  This data is expected to be in the following form:
 *
 *  keyword \t\trootword\tinflectional_forms
 *         ^ ** Note that there is a space after keyword **
 *
 *  Allowable inflectional forms can be found in morph.h
 *  This program automatically deletes duplicates, and sorts the infl info.
 */
#include <stdio.h>
#ifdef BSD
#include <strings.h>
#else
#include <string.h>
#endif
#include <sys/file.h>
#include <errno.h>
#include <db.h>
#include <fcntl.h>
#include "morph.h"


main(argc, argv)
     int argc;
     char **argv;
{
  DBT key, content, old;
  DB *dbp;
  HASHINFO table;
  FILE *text_fp;
  char line_buffer[MAXCHARS];
  char hash_buffer[MAXCHARS];
  char no_hash[MAXCHARS];
  char nodupdata[MAXCHARS];
  char encdata[MAXCHARS];
  char keydata[MAXCHARS];
  char contentdata[MAXCHARS];
  char *starthash, *endhash;
  int hashsize, dupflag;
  
    
   /* Check arguments */
  if (argc != 2)
    {
      fprintf(stderr,"Usage: %s <morphology text file>\n\n", argv[0]);
      exit(1);
    }
  
  /* Open the morphology text file for reading */
  if ((text_fp = fopen( argv[1], "r")) == NULL) 
    {
      fprintf(stderr, "Error opening morphology text file: %s\n", argv[1]);
      exit(1);
    }
  

  /* Create a new database file with these parameters*/
  table.bsize = BUCKET_SIZE;
  table.ffactor = FILL_FACTOR;
  table.nelem = NUM_ENTRIES;
  table.cachesize = CACHE_SIZE;
  /* table.lorder = LITTLE_ENDIAN; */
  table.hash = NULL;     /* 96/08 xchang added here, the new linc will not assign this by default */
  table.lorder = BIG_ENDIAN;  

  if (!get_files( argv[1], &dbp, table))
    exit(1);

  while (fgets(line_buffer, MAXCHARS, text_fp) != NULL)
    {
      /* Strip off the newline at the end */
      hashsize = strlen(line_buffer);
      line_buffer[hashsize-1] = '\0';
      
	/*skip commented or blank lines */
      if ((*line_buffer != ';') && (*line_buffer != '\0'))
	{

	  /* Break line_buffer into key and content */
	  *keydata = '\0';
	  strncat(keydata, line_buffer, strchr(line_buffer, ' ')- (int) line_buffer);
	  key.size = strlen(keydata) + 1;
	  strcpy(hash_buffer, strchr((strchr(line_buffer,'\t')+1), '\t')+1);
	  key.data = keydata;


	  /* Encode content (hash_buffer) by encoding each set of inflectional */
	  /* information between hashmarks and adding them to content.data. */
	  /* Don't forget to skip over the 1st two characters that contain the */
	  /* inflectional array info and # of characters in common.  */

	  *contentdata = '\0';
	  starthash = hash_buffer;
	  while ((endhash = strchr(starthash+2, SEP_CHAR)) != NULL)
	  {
	    *no_hash =  '\0';
	    *encdata = '\0';
	    strncat(no_hash, starthash, endhash-starthash);
	    /* switch position of word and inflectional */
	    /* info before calling encode */
	    flip(no_hash);    
	    if (encode(key.data, no_hash, encdata, &hashsize))
	      {
		if (simple_duptest(contentdata, encdata) == 0) 
		  {
		    if (*contentdata != '\0')
		      strcat(contentdata, SEP_STRING);
		    strcat(contentdata, encdata);
		  }
	      }
	    starthash = endhash+1;
	  } /* while */
	  
	  /* switch position of word and inflectional */
	  /* info before calling encode */
	  flip(starthash); 
	  if (encode(key.data, starthash, encdata, &hashsize)) 
	    {
	      if (simple_duptest(contentdata, encdata) == 0)  
		{
		  if (*contentdata != '\0')
		    strcat(contentdata, SEP_STRING);
		  strcat(contentdata, encdata);
		}

	      content.data = contentdata;
	      content.size = strlen(contentdata) + 1;

	      /* Insert key and content into database */
	      dupflag = 0;
	      /* if (!(dbp->get)(dbp, &key, &old, NULL))  */
	      if (!(dbp->get)(dbp, &key, &old, 0)) 
		{
		  /* merge with already existing info; check for duplicates */
		  if ((dupflag = morph_duptest(old.data, contentdata, nodupdata)) == 0)
		    {
		      strcpy(hash_buffer, old.data);
		      strcat(hash_buffer, SEP_STRING);
		      strcat(hash_buffer, nodupdata);
		      content.data = hash_buffer;
		      content.size = old.size + strlen(nodupdata) + 3;
		    }
		}
	      if (dupflag == 0)
		if ((dbp->put)(dbp, &key, &content, R_PUT) < 0)
		    {
		      fprintf(stderr, "Storage error! key: %s\tdata:%s\n", key.data, content.data);
		      return(1);
		    } /* if*/
	    } /* if encode */
    } /* if */
    } /* while */


  /* Close files */
  close(text_fp);
  (dbp->close)(dbp);
  exit(0);
} /* main */




/* **********************************************************************
 * name: get_files
 * args: string, database pointer
 * returns: 0 on failure, 1 on success
 * summary: Alters suffix of STRING argument to ".db", then unlinks a file
 *          with that name.  Attempts to open up that file as a db database
 *          file, pointing DBP (argument 2) to it.
 */
int
get_files(dbfile, dbp, info)
     char *dbfile;
     DB **dbp;
     HASHINFO info;
{
  char *suffix;
  

  if ((suffix = strrchr( dbfile, '.' )) != NULL)
    strcpy(suffix+1, "db\0");
  else
    strcat(dbfile, ".db");
  unlink(dbfile);
  
  if (!(*dbp = hash_open( dbfile, O_CREAT | O_RDWR, 0740, &info ))) 
    {
      fprintf( stderr, "cannot create: %s\n", dbfile );
      return(0);
    }
  return(1);
}



