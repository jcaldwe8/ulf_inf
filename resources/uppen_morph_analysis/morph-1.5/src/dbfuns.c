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

#include <sys/types.h>
#include <stdio.h>
#include <sys/file.h>
#include <db.h>
#include <fcntl.h>
#include "morph.h"

extern int decode();

/* Procedure to open a DB file */
DB *
C_open_db(char *file, DB *dbp)
{
  if (!(dbp = hash_open( file, O_RDONLY, 0400, NULL))) {
    fprintf(stderr, "cannot open: %s\n", file );
    dbp = NULL;
  } 
  return( dbp );
}

/* Procedure to close a DB file */
int 
C_close_db(DB *dbp)
{
  return((dbp->close)(dbp));
}

/* Given a key word and the DB fileptr, get an encoded string
   from the database, decode it, and return in buffer */
int 
C_db_get_decoded(
		 char *keystring,
		 DB *dbp,
		 char *buffer
		 )
{
  int status;
  DBT key, data;

/* Code to check, nullity of the database - srini */
  if (dbp == NULL) {
    fprintf(stderr,"Database Absent-Failing to access information for %s\n",keystring);
    return(-1);
  }
  key.data = keystring;
  key.size = strlen(key.data)+1;

  /* if get is successful, then decode the entry. */
  if ((status = (dbp->get)( dbp, &key, &data, 0 )) == 0) {
    decode(keystring, data.data, data.size, buffer);
  }
  return( status );
}

/* Given a key word and the DB fileptr, get an UNencoded string
   from the database, return in buffer */
int 
C_db_get(
	 char *keystring,
	 DB *dbp,
	 char *buffer
	 )
{
  int status;
  DBT key, data;

  key.data = keystring;
  key.size = strlen(key.data)+1;
  
  /* if get is successful, then byte-copy the entry into buffer */
  if ((status = (dbp->get)( dbp, &key, &data, 0 )) == 0) {
    bcopy ( data.data, buffer, data.size );
    buffer[data.size] = 0;
  }
  return( status );
}

/* This procedure inserts an encoded string into the database.  It takes
   a DB file pointer, the key word, and the UNencoded entries.  It encodes
   the entries, and them inserts them into the database, deleting duplicate
   entries as it goes.  encode_hashedstring and morph_duptest are both in morphmisc.c */
int 
C_db_put_encoded(
		 DB *dbp,
		 char *keystring,
		 char *contentstring
		 )
/* Returns -2 if encoding error,
           -1 if other error,
	    0 if success */
{
  DBT key, content, old;
  char encdata[MAXCHARS];
  char contentdata[MAXCHARS];
  char nodupdata[MAXCHARS];
  char *starthash, *endhash;
  int hashsize, dupflag;

  *nodupdata = '\0';
  key.data = keystring;
  key.size = strlen(keystring) + 1;
  
  /* encode contentstring, returning the encoded string as contentdata.
     contentstring may have multiple entries separated by hashmarks */
  if (encode_hashedstring(contentstring, contentdata, key.data))
    {
      content.size = strlen(contentdata) + 1;
      content.data = contentdata;

      /* Insert key and content into database */
      dupflag = 0;
      /* Check if the key is already in the database */
      if (!(dbp->get)(dbp, &key, &old, NULL)) 
	  /* check if there are duplicates between the old and new entries */
	  if ((dupflag = morph_duptest(old.data, contentdata, nodupdata)) == 0)
	    {
	      content.data = nodupdata;
	      content.size = strlen(content.data) + 1;
	      if (*(old.data) != '\0')
		{
		  /* combine the old and new data */
		  strcpy(encdata, old.data);
		  strcat(encdata, SEP_STRING);
		  /* strcat(encdata, content.data); */
                  strcat(encdata, nodupdata);   /*xchang changed here, to make it clear */
		  content.data = encdata;
		  /* content.size += old.size + 2; */
                  content.size = old.size + strlen(nodupdata) + 3; 
		}
	    }
      /* place the new/combined data into the database */
      if (dupflag == 0)
	return ((dbp->put)(dbp, &key, &content, R_PUT));
      else return(0);  /* info already in database, so 'successful' */
    } /* if encode */
  else return (-2);
}

/* This procedure replaces the entries in the database with new information.
   It is used when modifying an entry. */
int 
C_db_replace_encoded(
		     DB *dbp,
		     char *keystring,
		     char *contentstring
		     )
/* Returns -2 if encoding error,
           -1 if other error,
	   0  if success */
{
  DBT key, content;
  char encodedstring[DATABUFFER];
  
  key.data = keystring;
  key.size = strlen(key.data) + 1;
  /* encode the contentstring */
  if (encode_hashedstring(contentstring, encodedstring, key.data))
    {
      /* replace the old with the new */
      content.data = encodedstring;
      content.size = strlen(encodedstring) +1;
      return((dbp->put)(dbp, &key, &content, R_PUT));
    }
  else 
    return (-2);
}


/* This procedure deletes the key and associated entries */
int 
C_db_del_encoded(DB *dbp, char *keystring)
/* Returns -1 on error; 0 on success; 1 if key was not in the file */
{
  DBT key;
  
  key.data = keystring;
  key.size = strlen(key.data)+1;
  return ((dbp->del)(dbp, &key, 0));
}


