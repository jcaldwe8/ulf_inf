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
 *   This program prompts for a key, and prints either the entries associated
 *     with that key, or the words *** NOT FOUND ***.  It also prints out the
 *     time that it took to retrieve the key.  This program provides a good
 *     shell for writing any custom utility programs that might be needed.
 */


#include <stdio.h>
#ifdef BSD
#include <strings.h>
#else
#include <string.h>
#endif
#include <sys/file.h>
#include <fcntl.h>
#include <db.h>
#include <sys/timeb.h>
#include "morph.h"

int
main(int argc, char **argv)
{
  char	input_string[DATABUFFER];
  char	decoded_string[DATABUFFER];
  DBT entry, key;
  DB	*dbp;
  int ok, keys, time, diff;
  struct timeb start, end;

  /* Check arguments */
  if (argc != 2)
    {
      fprintf(stderr,"Usage: %s <database file>\n\n", argv[0]);
      exit(1);
    }

  /* Open DB file */
  if (!(dbp = hash_open( argv[1], O_RDONLY, 0440, NULL ))) 
    {
      fprintf( stderr, "cannot create: %s\n", argv[1] );
      exit(1);
    }
  time = 0;
  keys = 0;
  
  printf("key: ");
  while ((ok = scanf("%s", input_string)) != EOF)
    {
      ftime(&start);
      
      key.data = input_string;
      key.size = strlen(input_string) + 1;
      decoded_string[0] = '\0';
      /* Retrieve key from database */
      if ((dbp->get)(dbp, &key, &entry, NULL)) 
	{
	  printf("Key: %s \tEntry: *** NOT FOUND ***", input_string);
	}
      else
	{
	  /* decode the encoded entries */
	  decode(input_string, entry.data, entry.size, decoded_string);
	  printf ( "Key: %s \tEntry: %s",  input_string, decoded_string);
	}
      ftime(&end);
      
      diff = timediff(end,start);
      printf ( " \tRetrieval time:%u msec.\n", diff );
      keys++;
      time += diff;
      
      printf("key: ");
    }
  printf("\n\nAverage retrieval time: %f msec.\n\n", (float)time/(float)keys );
  (dbp->close)(dbp);

  return(0);
}

int
timediff(struct timeb x1, struct timeb x2)
{
  if (x1.time == x2.time)
    return(x1.millitm - x2.millitm);
  else
    return(x1.millitm - x2.millitm + 1000);
}
