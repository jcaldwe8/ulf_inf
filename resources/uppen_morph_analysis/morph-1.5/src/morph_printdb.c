/*
 * Copyright 1992,1993 The University of Pennsylvania
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
 *  This program dumps an ENCODED DB file, such as the morphological lexicon
 *  to stdout.  It takes a db filename as its only parameter.  The copyright
 *  notice is printed at the top of the .flat file.  It is distinguishable by
 *  the 3 semi-colons preceding each line.  The executable is called from
 *  the database maintenance program.
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
#include "morph.h"

void print_copyright();

int 
main (int argc, char **argv)
{
  DBT content, key;
  DB *dbp;
  char decoded_string[DATABUFFER];

  if (argc != 2) {
    fprintf(stderr, "Usage: %s <database file>\n", argv[0]);
    exit(1);
  }

  /* Try to open the database file */
  if (!(dbp = hash_open( argv[1], O_RDONLY, 0440, NULL ))) 
    {
      fprintf(stderr, "cannot open: %s\n",argv[1] );
      exit(1);
    }

  /* Get the first key in the database.  If there isn't one, then
     the file is empty.  Otherwise, print it out, and get the next key,
     continuing until there are no more keys.  */
  if ((dbp->seq)(dbp, &key, &content, R_FIRST))
    {
      fprintf (stderr, "database is empty");
      exit(1);
    }

  /* Print out the copyright notice. */
  print_copyright();
  do {
    if (!decode(key.data, content.data, content.size, decoded_string))
      fprintf(stderr, "ERROR: can not decode %s\n", content.data);
    else
      printf("%s \t\t%s\n", key.data, decoded_string);
  } while (!(dbp->seq)(dbp, &key, &content, R_NEXT));
  
  (dbp->close)(dbp);

  exit(0);
}

void
print_copyright()
{
  printf(";;;VERSION NUMBER: %s\n", VERSION_NUMBER);
  printf(";;;\n");
  printf(";;;\n");
  printf(";;;            Copyright (C) 1992, 1993 University of Pennsylvania\n");
  printf(";;;\n");                                                           
  printf(";;;   Permission to use, copy, modify, and distribute this software and\n");
  printf(";;;   its documentation for any purpose and without fee is hereby\n");
  printf(";;;   granted, provided that the above copyright notice appear in all\n");
  printf(";;;   copies and that both that copyright notice and this permission\n");
  printf(";;;   notice appear in supporting documentation, and that the name of\n");
  printf(";;;   U. of Pennsylvania not be used in advertising or publicity pertaining\n"); 
  printf(";;;   to distribution of the software without specific, written prior\n");
  printf(";;;   permission.  U. of Pennsylvania makes no representations about\n");
  printf(";;;   the suitability of this software for any purpose.  It is\n");
  printf(";;;   provided 'as is' without express or implied warranty.\n");
  printf(";;;                         (11/15/93)\n");
  printf(";;;\n"); 
  printf(";;;\n");                                                            
}
