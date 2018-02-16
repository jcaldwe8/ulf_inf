############################################################################
# choose your system by the CFLAGS definition
#
#	System V:
#CFLAGS= -g -DUNIX -DUSG
#
#	HP:
#CFLAGS = -O -DUNIX
#
#	BSD or SunOS:
CFLAGS=-O -DUNIX -DBSD -g
#
#	ULTRIX:
#CFLAGS=-O -DUNIX -DBSD -DULTRIX
#
#################
CC = cc
############################################################################
PWD=/mnt/linc/xtag2/pub/morph-1.5
LIBDIR = lib
BINDIR = bin
SRCDIR = src
DATADIR = data

################       BEGIN  INSTALLATION      ##############################
#
# Athena widgets include
#    Set XAWPATH to pathname to your system's Athena widgets include directory.
#XAWPATH = /usr/openwin/share/include
XAWPATH = /usr/include/X11/Xaw
#

# location for libXaw.so.5
SYSLIBDIRS = /usr/openwin/lib:/usr/lib

#
# DB package directory
#     If you already have the DB package of disk-based database routines
#     from the University of California, Berkeley installed on your
#     system, then set HASHDIR to your local system directory where it
#     can be found.  To use the version supplied, do nothing.
HASHDIR = $(PWD)/hash

# X application defaults file location
#     If the X application defaults files will remain in the local
#     directory, do nothing.  If they will be installed in the system's
#     app-defaults directory, set XAPPLDIR to the proper pathname
#     (normally, /usr/lib/X11/app-defaults) and uncomment the `cp'
#     command to copy the entire ./src/XAPPL subdirectory.
XAPPLDIR = $(PWD)/$(SRCDIR)/XAPPL

INCLUDES = -I$(HASHDIR) -I$(XAWPATH) 

OTHERLIBS = -lm 
XtLIBS =  -lXaw -lXmu -lXt -lXext -lX11
MORPHLIB = -L$(LIBDIR) -lmorph -lhash

all: Makefile libraries cprogs xmdbm 

perms: 
	chgrp -R xtag . > /dev/null
	chmod -R g+w . > /dev/null

###############       END INSTALLATION         #############################


# Customize files for xmdbm.
#     Default file and path names for the program to print out the encoded
#     database, and for the messages file to allow internationalization
XMDBMDEFS = -DPRINTCOMMAND='"$(PWD)/bin/morph_printdb"' -DMSGPATH='"$(XAPPLDIR)"'

cprogs: morph_createdb morph_printdb morph_timedretrieve

xmdbm: $(BINDIR)/xmdbm
morph_createdb: $(BINDIR)/morph_createdb
morph_printdb: $(BINDIR)/morph_printdb
morph_timedretrieve: $(BINDIR)/morph_timedretrieve
morph_english.db: $(DATADIR)/morph_english.db
libraries: $(LIBDIR)/libmorph.a $(LIBDIR)/libhash.a


#  Library Files for morph
MORPHOBJECTS = $(LIBDIR)/coding.o $(LIBDIR)/dbfuns.o $(LIBDIR)/morphmisc.o \
		$(LIBDIR)/mySimpleText.o $(LIBDIR)/inflection.o

$(LIBDIR)/libmorph.a: $(MORPHOBJECTS)
	ar r $(LIBDIR)/libmorph.a $(MORPHOBJECTS)
	ranlib $(LIBDIR)/libmorph.a

$(LIBDIR)/coding.o: $(SRCDIR)/coding.c $(SRCDIR)/morph.h
	$(CC) $(CFLAGS) $(INCLUDES) -c -o $(LIBDIR)/coding.o $(SRCDIR)/coding.c
$(LIBDIR)/dbfuns.o: $(SRCDIR)/dbfuns.c $(SRCDIR)/morph.h
	$(CC) $(CFLAGS) $(INCLUDES) -c -o $(LIBDIR)/dbfuns.o $(SRCDIR)/dbfuns.c
$(LIBDIR)/morphmisc.o: $(SRCDIR)/morphmisc.c $(SRCDIR)/morph.h
	$(CC) $(CFLAGS) $(INCLUDES) -c -o $(LIBDIR)/morphmisc.o $(SRCDIR)/morphmisc.c
$(LIBDIR)/inflection.o: $(SRCDIR)/inflection.c $(SRCDIR)/morph.h
	$(CC) $(CFLAGS) $(INCLUDES) -c -o $(LIBDIR)/inflection.o $(SRCDIR)/inflection.c
$(LIBDIR)/mySimpleText.o: $(SRCDIR)/mySimpleText.c $(SRCDIR)/mySimpleText.h $(SRCDIR)/mySimpleTexP.h $(SRCDIR)/morph.h
	$(CC) $(CFLAGS) $(INCLUDES) -c -o $(LIBDIR)/mySimpleText.o $(SRCDIR)/mySimpleText.c

#  Library files for hash

HASHOBJECTS = $(LIBDIR)/dynahash.o $(LIBDIR)/log2.o $(LIBDIR)/page.o \
		$(LIBDIR)/bigkey.o $(LIBDIR)/buf.o $(LIBDIR)/hfunc.o \
		$(LIBDIR)/mkstemp.o $(LIBDIR)/bcopy.o

$(LIBDIR)/libhash.a: $(HASHOBJECTS)
	ar r $(LIBDIR)/libhash.a $(HASHOBJECTS)
	ranlib $(LIBDIR)/libhash.a

$(LIBDIR)/dynahash.o: $(HASHDIR)/dynahash.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -w -o $(LIBDIR)/dynahash.o $(HASHDIR)/dynahash.c
$(LIBDIR)/log2.o: $(HASHDIR)/log2.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -w -o $(LIBDIR)/log2.o $(HASHDIR)/log2.c
$(LIBDIR)/page.o: $(HASHDIR)/page.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -w -o $(LIBDIR)/page.o $(HASHDIR)/page.c
$(LIBDIR)/bigkey.o: $(HASHDIR)/bigkey.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -w -o $(LIBDIR)/bigkey.o $(HASHDIR)/bigkey.c
$(LIBDIR)/buf.o: $(HASHDIR)/buf.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -w -o $(LIBDIR)/buf.o $(HASHDIR)/buf.c
$(LIBDIR)/hfunc.o: $(HASHDIR)/hfunc.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -w -o $(LIBDIR)/hfunc.o $(HASHDIR)/hfunc.c
$(LIBDIR)/mkstemp.o: $(HASHDIR)/mkstemp.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -w -o $(LIBDIR)/mkstemp.o $(HASHDIR)/mkstemp.c
$(LIBDIR)/bcopy.o: $(HASHDIR)/bcopy.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -w -o $(LIBDIR)/bcopy.o $(HASHDIR)/bcopy.c


# Binary files
$(BINDIR)/morph_printdb: $(SRCDIR)/morph_printdb.c libraries
	$(CC) $(CFLAGS) $(INCLUDES) -o $(BINDIR)/morph_printdb $(SRCDIR)/morph_printdb.c \
		$(MORPHLIB) $(OTHERLIBS)
$(BINDIR)/morph_createdb: $(SRCDIR)/morph_createdb.c libraries
	$(CC) $(CFLAGS) $(INCLUDES) -w -o $(BINDIR)/morph_createdb $(SRCDIR)/morph_createdb.c \
		$(MORPHLIB) $(OTHERLIBS)
$(BINDIR)/xmdbm: $(SRCDIR)/xmdbm-base.c $(SRCDIR)/xmdbm-base.h libraries
	/bin/rm -f $(BINDIR)/xmdbm
	echo '#! /bin/sh' >> $(BINDIR)/xmdbm
	echo >> $(BINDIR)/xmdbm
	echo '# Script to call xmdbm-base with proper environment variables' >>$(BINDIR)/xmdbm
	echo >> $(BINDIR)/xmdbm
	echo 'XAPPLRESDIR=$(XAPPLDIR); export XAPPLRESDIR' >> $(BINDIR)/xmdbm
	echo 'LD_LIBRARY_PATH=$(SYSLIBDIRS); export LD_LIBRARY_PATH' >> $(BINDIR)/xmdbm
	echo exec $(PWD)/$(BINDIR)/xmdbm-base '$$*' >> $(BINDIR)/xmdbm
	chmod a+rx $(BINDIR)/xmdbm
	$(CC) $(CFLAGS) $(INCLUDES) $(XMDBMDEFS) -o $(BINDIR)/xmdbm-base $(SRCDIR)/xmdbm-base.c \
		$(MORPHLIB) $(XtLIBS) $(OTHERLIBS)
	chmod a+rx $(BINDIR)/xmdbm

$(BINDIR)/morph_timedretrieve: $(SRCDIR)/morph_timedretrieve.c libraries
	$(CC) $(CFLAGS) $(INCLUDES) -o $(BINDIR)/morph_timedretrieve $(SRCDIR)/morph_timedretrieve.c \
		 $(OTHERLIBS) $(MORPHLIB)

# Data Files
$(DATADIR)/morph_english.db: $(DATADIR)/morph_english.flat
	$(BINDIR)/morph_createdb $(DATADIR)/morph_english.flat


#Clean-up
clean: 
	/bin/rm -f $(LIBDIR)/*.o
	/bin/rm -f $(LIBDIR)/*.a
	/bin/rm -f $(BINDIR)/*

spotless: clean
	/bin/rm -f $(DATADIR)/morph_english.db

#clean:
#	/bin/rm -f $(LIBDIR)/*
#	/bin/rm -f $(DATADIR)/morph_english.db*
#	/bin/rm -f $(BINDIR)/*

#xmdbmdefaults: $(SRCDIR)/XAPPL/XMDBM $(SRCDIR)/XAPPL/XMDBMmsgs
#	cp -r $(SRCDIR)/XAPPL/* $(XAPPLDIR)
#
