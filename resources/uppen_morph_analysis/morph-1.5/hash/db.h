/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <cdefs.h>

/* REMOVE THIS WHEN THIS GETS PUT ON A SYSTEM THAT HAS EFTYPE defined */
#define	EFTYPE	EINVAL

/* Magic Numbers */
#define	HASHMAGIC	0x061561
#define HASHVERSION	1
#define BTREEMAGIC	0x053162
#define BTREEVERSION	2
#define RECNOMAGIC	0x072658
#define RECNOVERSIOn	1

/* flags for DB.put() call */
#define	R_APPEND	1		/* RECNO */
#define	R_DUP		2		/* BTREE */
#define	R_INSERT	3		/* RECNO */
#define	R_NOOVERWRITE	4		/* BTREE, HASH, RECNO */

/* The following was missing and added by Yves Schabes */
#define R_PUT           5               /*  BTREE, HASH, RECNO */

/* flags for DB.seq() call */
#define	R_CURSOR	1		/* BTREE, RECNO */
#define	R_FIRST		2		/* BTREE, HASH, RECNO */
#define	R_LAST		3		/* BTREE, RECNO */
#define	R_NEXT		4		/* BTREE, HASH, RECNO */
#define	R_PREV		5		/* BTREE, RECNO */

/*
    Swap between big/little endian
*/

#define BLSWAP(a) { \
    long _tmp = a; \
    ((char *)&(a))[0] = ((char *)&_tmp)[3]; \
    ((char *)&(a))[1] = ((char *)&_tmp)[2]; \
    ((char *)&(a))[2] = ((char *)&_tmp)[1]; \
    ((char *)&(a))[3] = ((char *)&_tmp)[0]; \
}

#define BLSWAP_COPY(a,b) { \
    ((char *)&(b))[0] = ((char *)&(a))[3]; \
    ((char *)&(b))[1] = ((char *)&(a))[2]; \
    ((char *)&(b))[2] = ((char *)&(a))[1]; \
    ((char *)&(b))[3] = ((char *)&(a))[0]; \
}
#define BSSWAP(a) { \
    u_short _tmp = (a); \
    ((char *)&(a))[0] = ((char *)&_tmp)[1]; \
    ((char *)&(a))[1] = ((char *)&_tmp)[0]; \
}
#define BSSWAP_COPY(a,b) { \
    ((char *)&(b))[0] = ((char *)&(a))[1]; \
    ((char *)&(b))[1] = ((char *)&(a))[0]; \
}

/* key/data structure -- a data-base thang */
typedef struct {
	char *data;
	int size;
} DBT;

/* access method description structure */
typedef struct _db {
      void *internal;         /* access method private; really void * */
#define		DB_BTREE	1
#define		DB_HASH		2
#define		DB_RECNO	3
      int type;
__BEGIN_DECLS
      int (*close) __P((const struct _db *));
      int (*del) __P((const struct _db *, const DBT *, unsigned int));
      int (*get) __P((const struct _db *, DBT *, DBT *, unsigned int));
      int (*put) __P((const struct _db *, const DBT *, const DBT *,
              unsigned int));
      int (*seq) __P((const struct _db *, DBT *, DBT *, unsigned int));
      int (*sync) __P((const struct _db *));
__END_DECLS
} DB;

/* structure used to pass parameters to the hashing routines */
typedef struct {
	int bsize;		/* bucket size */
	int ffactor;		/* fill factor */
	int nelem;		/* number of elements */
	int cachesize;		/* bytes to cache */
	int (*hash)();		/* hash function */
	int lorder;		/* byte order */
} HASHINFO;

/* structure used to pass parameters to the btree routines */
typedef struct {
	u_long flags;
	int cachesize;		/* bytes to cache */
	int psize;		/* page size */
	int (*compare);		/* compare function */
	int lorder;		/* byte order */
} BTREEINFO;

/* structure used to pass parameters to the record routines */
typedef struct {
#define	R_FIXEDLEN	0x01	/* fixed-length records */
	u_long flags;
	int cachesize;		/* bytes to cache */
	size_t reclen;		/* record length (fixed-length records) */
	u_char bval;		/* delimiting byte (variable-length records */
} RECNOINFO;

/* key structure for the record routines */
typedef struct {
	u_long number;
	u_long offset;
	u_long length;
#define	R_LENGTH	0x01	/* length is valid */
#define	R_NUMBER	0x02	/* record number is valid */
#define	R_OFFSET	0x04	/* offset is valid */
	u_char valid;
} RECNOKEY;

#if __STDC__ || c_plusplus
DB *btree_open(const char *file, int flags, int mode, const BTREEINFO *private);
DB *hash_open(const char *file, int flags, int mode, const HASHINFO *private);
DB *recno_open(const char *file, int flags, int mode, const RECNOINFO *private);
#else
DB *btree_open();
DB *hash_open();
DB *recno_open();
#endif
