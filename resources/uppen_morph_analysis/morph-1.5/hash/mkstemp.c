/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "%W% (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/file.h>
#include <sys/fcntl.h>

mkstemp(as)
	char *as;
{
	register char *s;
	register unsigned int pid;
	register int fd, i;

	pid = getpid();
	s = as;
	while (*s++)
		/* void */;
	s--;
	while (*--s == 'X') {
		*s = (pid % 10) + '0';
		pid /= 10;
	}
	s++;
	i = 'a';
	while ((fd = open(as, O_CREAT|O_EXCL|O_RDWR, 0600)) == -1) {
		if (i == 'z')
			return(-1);
		*s = i++;
	}
	return(fd);
}
