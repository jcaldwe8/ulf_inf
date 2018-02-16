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
#include <string.h>
#include <time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <db.h>
#include <sys/file.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h> 
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Cardinals.h>

#include "mySimpleText.h"
#include "morph.h"
#include "xmdbm-base.h"

Widget topwindow;          /* Window that the wm knows about */
Widget morphwindow;        /* My window that controls where everything is */
Widget dbdialogshell;      /* Popup shell for getting DB file name */
Widget logdialogshell;     /* Popup shell for getting log file name */
Widget dbfiledialog;       /* Dialog widget for getting DB file name */
Widget logfiledialog;      /* Dialog widget for getting log file name */
Widget errormsgshell;      /* Popup shell for error messages */
Widget errormsgdialog;     /* Dialog widget for error messages */

/* Top level button widgets */
Widget filebutton, lookupbutton, modifybutton, deletebutton, addbutton;
/* button widgets for inside the various modes. dummy button is for spacing  */
Widget donebutton, clearbutton, dummybutton;
Widget inputspace;           /* the workspace for all modes to work in */
Widget modelabel;          /* indicate current mode */
Widget keydialog;          /* SimpleText widget to get the key word */
Widget rootdialog;         /* SimpleText widget to getor show the root word */
Widget historytext;        /* AsciiText widget to display the session history */
Widget verifyshell;        /* Popup shell for verifying a command */
Widget verifydialog;       /* Dialog widget for verifying a command */
Widget delentrychoiceslabel;  /* Label widget to make the workspace pretty. */
Widget delentrychoices;       /* List widget to hold the entries of a key word */
Widget addkeyentrieslabel;    /* Label widget to make the workspace pretty. */
Widget addkeyentries;         /* List widget to hold the entries of a key word */
Widget modifychoiceslabel;    /* Label widget to make the workspace pretty. */
Widget modifychoices;         /* List widget to hold the entries of a key word */
Widget speechparts;        /* List widget to hold the parts of speech */
Widget subspeechparts;     /* List widget to hold the inflectional info */ 
Widget subspeechshell;     /* Popup shell for the inflectional info */

HASHINFO table;        /* Necessary when creating a new DB database */
XtAppContext app_con;  /* X Window application context */
DB *dbfileptr = NULL, *tempdbfileptr = NULL;       /* DB file ptrs */
FILE *logfileptr = NULL, *templogfileptr = NULL;   /* log file ptrs */
char *dbnameptr;                  /* ptr to the name of the database file*/
char *lognameptr;                 /* ptr to the name of the log file */
char *tempdbnameptr;                  /* ptr to the name of the temp database file*/
char *templognameptr;                 /* ptr to the name of the temp log file */
char *lastkeyptr;          /* previous key to make sure it hasn't changed. */
char *listarray[MAX_ENTRIES+1];  /* array to hold entries of a key for display */
				 /* one extra to hold modifications being made */
int listindex = -1;              /* number of entries currently in listarray */
int stateflag = NOFLAG;    /* what 'mode' are we currently in? (lookup, add, etc */
int changedflag = FALSE;   /* has the database been modified? */
int makeflat = FALSE;      /* changes have been saved, so .flat file need updating */
int nolog = 0, readonly = 0;  /* flags set by command line parameters */

struct stat dbfileinfo;    /* info on modification times for the db file */

#ifdef X11R4
#define _XtAddCallback(lookupbutton,XtNcallback,lookup_pressed) \
  XtAddCallback(lookupbutton,XtNcallback,lookup_pressed)
#else
#define _XtAddCallback(lookupbutton,XtNcallback,lookup_pressed) \
  XtAddCallback(lookupbutton,XtNcallback,lookup_pressed,NULL)
#endif

main(int argc, char **argv)
{
    Widget filemenu, deletemenu;          /* the two menus */
    Widget mselection;   /* Used when setting up the menu selections */
    XrmDatabase appmsgs = NULL;  /* Used in setting up the internationalization */
    char dbname[DATABUFFER];       /* obvious */
    char logname[DATABUFFER];      /* obvious */
    char tempdbname[DATABUFFER];   /* temporary db file */
    char templogname[DATABUFFER];  /* temporary log file */
    char lastkey[DATABUFFER];      /* obvious */
    char tempstring[DATABUFFER];   /* generic temp string */
    int i;

/* Some initializations that cc doesn't let us do when declaring */
    *dbname = '\0';
    *logname = '\0';
    *lastkey = '\0';
    *tempstring = '\0';
/* Some other initializations for the temp files */
    *tempdbname = '\0';
    *templogname = '\0';
    morph_uniq_numstring(tempstring);
    strcat(tempdbname, TEMPDBNAME);
    strcat(tempdbname, tempstring);
    strcat(templogname, TEMPLOGNAME);
    strcat(templogname, tempstring);
    tempdbnameptr = tempdbname;
    templognameptr = templogname;
    *tempstring = '\0';

/* Initialize resources info */
    topwindow = XtAppInitialize(&app_con, "XMDBM", NULL, ZERO,
			  &argc, argv, fallback_resources, NULL, ZERO);

/* Load all text messages to be used in the program.  Used for */
/* internationalization */
    if (getenv("LANG") != NULL)
      sprintf(tempstring, "%s/%s/XMDBMmsgs", MSGPATH, getenv("LANG"));
    else
      sprintf(tempstring, "%s/XMDBMmsgs", MSGPATH);
    appmsgs = XrmGetFileDatabase(tempstring);
    if (appmsgs == NULL) 
      {
	XtDestroyApplicationContext(app_con);
	fprintf(stderr, "XMDBM ERROR: messages database not found in %s\n",
		tempstring);
	exit(0);
      }
    XrmMergeDatabases(appmsgs, XtAppGetErrorDatabase(app_con));
    XtAppAddActions(app_con, actionTable, XtNumber(actionTable));


/* Initialize some variables */
    dbnameptr = dbname;
    lognameptr = logname;
    lastkeyptr = lastkey;
    table.bsize = BUCKET_SIZE; table.ffactor = FILL_FACTOR;
    table.nelem = NUM_ENTRIES; table.cachesize = CACHE_SIZE;
    reset_tempfiles(table);
    for (i = 0; i <= MAX_ENTRIES; i++)
      {
      if ((*(listarray+i) = (char *)malloc(DATABUFFER)) == NULL)
	  {
	    fprintf(stderr, "Error allocating array space\n");
	    exit(1);
	  }
      **(listarray+i) = '\0';
    }

    if (argc != 1)		
	syntax(app_con, argc, argv, dbname, logname);
    

/* Create initial window layout */
    morphwindow = XtVaCreateManagedWidget("maintenance", formWidgetClass, topwindow, NULL);
    filebutton = XtVaCreateManagedWidget("filebutton", menuButtonWidgetClass,
				       morphwindow, NULL);    
    lookupbutton = XtVaCreateManagedWidget("lookupbutton", commandWidgetClass,
				       morphwindow, XtNfromHoriz, filebutton, NULL);
    _XtAddCallback(lookupbutton, XtNcallback, lookup_pressed);
    modifybutton = XtVaCreateManagedWidget("modifybutton", commandWidgetClass,
				       morphwindow, XtNfromHoriz, lookupbutton, NULL);
    _XtAddCallback(modifybutton, XtNcallback, modify_pressed);
    addbutton = XtVaCreateManagedWidget("addbutton", commandWidgetClass,
				       morphwindow, XtNfromHoriz, modifybutton, NULL);
    _XtAddCallback(addbutton, XtNcallback, add_pressed);
    deletebutton = XtVaCreateManagedWidget("deletebutton", menuButtonWidgetClass,
				       morphwindow, XtNfromHoriz, addbutton, NULL);
    dummybutton = XtVaCreateManagedWidget("dummybutton", commandWidgetClass,
				       morphwindow, XtNfromHoriz, deletebutton);
    clearbutton = XtVaCreateManagedWidget("clearbutton", commandWidgetClass,
				       morphwindow, XtNfromHoriz, dummybutton, NULL);
    _XtAddCallback(clearbutton, XtNcallback, clear_pressed);
    donebutton = XtVaCreateManagedWidget("donebutton", commandWidgetClass,
				       morphwindow, XtNfromHoriz, clearbutton, NULL);
    _XtAddCallback(donebutton, XtNcallback, done_pressed);

    modelabel = XtVaCreateManagedWidget("modelabel", labelWidgetClass,
				      morphwindow, XtNfromVert, filebutton, NULL);
    
    inputspace = XtVaCreateManagedWidget("blank", formWidgetClass, morphwindow,
				       XtNfromVert, modelabel, NULL);
    historytext = XtVaCreateManagedWidget("historytext", asciiTextWidgetClass,
					morphwindow, XtNfromVert, inputspace, NULL);
    XawTextDisplayCaret(historytext, False);


/* Create other transient windows */
    filemenu = XtVaCreatePopupShell("menu", simpleMenuWidgetClass, filebutton, NULL);
    deletemenu = XtVaCreatePopupShell("menu", simpleMenuWidgetClass, deletebutton,
				  NULL);

    dbdialogshell = XtVaCreatePopupShell("dbdialogshell", transientShellWidgetClass,
				   filebutton, NULL);
    dbfiledialog = XtVaCreateManagedWidget("dbfiledialog", dialogWidgetClass,
					   dbdialogshell, XtNvalue, dbname);
    logdialogshell = XtVaCreatePopupShell("logdialogshell", transientShellWidgetClass,
				   filebutton, NULL);
    logfiledialog = XtVaCreateManagedWidget("logfiledialog", dialogWidgetClass,
					   logdialogshell, XtNvalue, logname, NULL);

    errormsgshell = XtVaCreatePopupShell("errormsgshell", transientShellWidgetClass,
				   inputspace, NULL);
    errormsgdialog = XtVaCreateManagedWidget("errormsgdialog", dialogWidgetClass,
					     errormsgshell, NULL);
    XawDialogAddButton(errormsgdialog, "errormsgok", errormsg_popdown, NULL);

    verifyshell = XtVaCreatePopupShell("verifyshell", transientShellWidgetClass,
				     inputspace, NULL);
    verifydialog = XtVaCreateManagedWidget("verifydialog", dialogWidgetClass,
					     verifyshell, NULL);
    XawDialogAddButton(verifydialog, "verifyok", ok_pressed, NULL);
    XawDialogAddButton(verifydialog, "verifycancel", cancel_pressed, NULL);

/* Create inputspace windows */
    keydialog = XtVaCreateManagedWidget("keydialog", xiSimpleTextWidgetClass,
				      inputspace, NULL);
    rootdialog = XtVaCreateManagedWidget("rootdialog", xiSimpleTextWidgetClass,
				       inputspace, XtNfromVert, keydialog, NULL);

    delentrychoiceslabel = XtVaCreateManagedWidget("delentrychoiceslabel",
						 labelWidgetClass, inputspace,
						 XtNfromVert, keydialog, NULL);

    delentrychoices = XtVaCreateManagedWidget("delentrychoices", listWidgetClass,
					    inputspace, XtNfromVert, keydialog,
					    XtNfromHoriz, delentrychoiceslabel,
					    XtNlist, listarray, NULL);
    _XtAddCallback(delentrychoices, XtNcallback, delentry_verify);

    addkeyentrieslabel = XtVaCreateManagedWidget("addkeyentrieslabel",
						 labelWidgetClass, inputspace,
						 XtNfromVert, rootdialog, NULL);
    addkeyentries = XtVaCreateManagedWidget("addkeyentries", listWidgetClass,
					    inputspace, XtNfromVert,
					  rootdialog, XtNfromHoriz,
					  addkeyentrieslabel, XtNlist,
					  listarray, NULL);
    /* _XtAddCallback(addkeyentries, XtNcallback, XawListUnhighlight); */
    _XtAddCallback(addkeyentries, XtNcallback, XawListUnhighlight);

    modifychoiceslabel = XtVaCreateManagedWidget("modifychoiceslabel",
						 labelWidgetClass, inputspace,
						 XtNfromVert, rootdialog, NULL);
    modifychoices = XtVaCreateManagedWidget("modifychoices", listWidgetClass, inputspace, 
					  XtNfromVert, rootdialog, 
					  XtNfromHoriz,modifychoiceslabel, 
					  XtNlist, listarray, NULL);
    _XtAddCallback(modifychoices, XtNcallback, modify_chosen);
    
    speechparts = XtVaCreateManagedWidget("speechparts", listWidgetClass,
					inputspace, XtNfromHoriz, rootdialog,
					XtNlist, speechparts_list, NULL);
    _XtAddCallback(speechparts, XtNcallback, putup_subparts);
    subspeechshell = XtVaCreatePopupShell("subspeechshell", transientShellWidgetClass,
				   speechparts, NULL);
    subspeechparts = XtVaCreateManagedWidget("subspeechparts", listWidgetClass,
					subspeechshell, NULL);
    _XtAddCallback(subspeechparts, XtNcallback, verify_inflinfo);

/* Set up pull-down menus */ 
  /* File menu */   
    mselection = XtVaCreateManagedWidget(get_appmsg("appfilemenu", "choosedatabase"), 
				       smeBSBObjectClass, filemenu, NULL);
    _XtAddCallback(mselection, XtNcallback, FileMenuSelect);
    mselection = XtVaCreateManagedWidget(get_appmsg("appfilemenu", "chooselogfile"), 
				       smeBSBObjectClass, filemenu, NULL);
    _XtAddCallback(mselection, XtNcallback, FileMenuSelect);
    mselection = XtVaCreateManagedWidget(get_appmsg("appfilemenu", "save"), 
				       smeBSBObjectClass, filemenu, NULL);
    _XtAddCallback(mselection, XtNcallback, FileMenuSelect);
    mselection = XtVaCreateManagedWidget(get_appmsg("appfilemenu", "quit"), 
				       smeBSBObjectClass, filemenu, NULL);
    _XtAddCallback(mselection, XtNcallback, FileMenuSelect);

  /* Delete menu */
    mselection = XtVaCreateManagedWidget(get_appmsg("appdeletemenu", "deletekey"), 
				       smeBSBObjectClass, deletemenu, NULL);
    _XtAddCallback(mselection, XtNcallback, DeleteMenuSelect);
    mselection = XtVaCreateManagedWidget(get_appmsg("appdeletemenu", "deleteentry"), 
				       smeBSBObjectClass, deletemenu, NULL);
    _XtAddCallback(mselection, XtNcallback, DeleteMenuSelect);

    /* Set up title of program */
    sprintf(tempstring, "%s%s%s%s", get_appmsg("appname", "title"), " (", VERSION_NUMBER, ")");
    XtVaSetValues(topwindow, XtNtitle, tempstring, NULL);

    if (readonly)
      {
	/* disable the buttons that allow changes to the database */
	XtSetSensitive(modifybutton, False);
	XtSetSensitive(addbutton, False);
	XtSetSensitive(deletebutton, False);
      }

    XtRealizeWidget(topwindow);
    XtAppMainLoop(app_con);
}

/* ARGSUSED */
static void
FileMenuSelect(
	       Widget w,
	       XtPointer junk,
	       XtPointer user_data
	       )
/*  This procedure distributes control according to the option
    chosen under the FILE menu.  All menu names are not hard-coded
    to promote internationalization.  Users can choose database
    and log file names from this menu, as well as save the database and
    quit the program.  Error checking is done when opening new database and
    log files, and a new log file is opened automatically when a new
    database is opened.  A check is made when quitting to verify that
    any changes have been saved.  If not, an error message is put up, and
    the user may go back and save the changes.  */
{
  char tempstring[DATABUFFER];
  
  *tempstring = '\0';
  if (streq(XtName(w), get_appmsg("appfilemenu", "choosedatabase"))) 
    {
      stateflag = DBFILE;
      dialog_popup(dbdialogshell, dbnameptr);
    }
  else if (streq(XtName(w), get_appmsg("appfilemenu", "chooselogfile")))
    if (nolog)
      errormsg_popup(get_appmsg("apperror", "nologflag"));
    else
      {
	/* won't take the change directly.  Have to null it out, and then
	   put in the next string.  Pain in the *** */
	XtVaSetValues(logfiledialog, XtNvalue, "", NULL);
	XtVaSetValues(logfiledialog, XtNvalue, lognameptr, NULL);
	dialog_popup(logdialogshell, lognameptr);
      }
  else if (streq(XtName(w), get_appmsg("appfilemenu", "save")))
    save_database();
  else if (streq(XtName(w), get_appmsg("appfilemenu", "quit"))) 
    {
      stateflag = QUIT;
      if (changedflag)
	{
	  strcat(tempstring, get_appmsg("appverify", "losechanges"));
	  XtVaSetValues(verifydialog, XtNlabel, tempstring, XtNwidth,
		    strlen(tempstring) * FONTWIDTH, NULL);
	  confirm_popup();
	}
      else 
	quit();
    }
  else
    fprintf(stderr, "Problem with internalization file? Item %s chosen\n", XtName(w));
}

/* ARGSUSED */
static void
DeleteMenuSelect(
		 Widget w,
		 XtPointer junk,
		 XtPointer given_data
		 )
/*  Call the appropriate procedure depending on the two option 
    chosen under the delete menu.  Option names are not hard-coded to
    aid in internationalization. */
{
  if (dbfileptr == NULL)
      errormsg_popup(get_appmsg("apperror", "dbnotspecified"));
  else if (streq(XtName(w), get_appmsg("appdeletemenu", "deletekey")))
    deletekey_pressed();
  else if (streq(XtName(w), get_appmsg("appdeletemenu", "deleteentry")))
    deleteentry_pressed();
}


void 
lookup_pressed()
/* The lookup mode was chosen, so check to verify that a database
   has been opened, and then initalize the appropriate structures and
   map the keydialog and the done and clear buttons. Put the appropriate mode
   label up as well.  The label is not hard-coded to aid in
   internationalization. */
{
  if (dbfileptr == NULL)
    errormsg_popup(get_appmsg("apperror", "dbnotspecified"));
  else 
    {
      stateflag = LOOKUP;
      set_commandsensitivity(False);
      XtVaSetValues(modelabel, XtNlabel, get_appmsg("appmode", "lookup"), NULL);
      XtMapWidget(modelabel);
      XtMapWidget(keydialog);
      XtMapWidget(clearbutton);
      XtMapWidget(donebutton);
      XtSetKeyboardFocus(inputspace, keydialog);
    }
}

void 
modify_pressed()
/* The modify mode was chosen, so check to verify that a database
   has been opened, and then initalize the appropriate structures and
   map the keydialog and the done and clear buttons. Put the appropriate mode
   label up as well.  The label is not hard-coded to aid in
   internationalization. */
{
  
  if (dbfileptr == NULL)
      errormsg_popup(get_appmsg("apperror", "dbnotspecified"));
  else 
    {
      stateflag = MODIFY;
      XtVaSetValues(modelabel, XtNlabel, get_appmsg("appmode", "modify"), NULL);
      XtMapWidget(modelabel);
      set_commandsensitivity(False);
      XtMapWidget(keydialog);
      XtMapWidget(clearbutton);
      XtMapWidget(donebutton);
      XtSetKeyboardFocus(inputspace, keydialog);
    }
}

void 
add_pressed()
/* The add mode was chosen, so check to verify that a database
   has been opened, and then initalize the appropriate structures and
   map the keydialog and the done and clear buttons. Put the appropriate mode
   label up as well.  The label is not hard-coded to aid in
   internationalization. */
{
   if (dbfileptr == NULL)
      errormsg_popup(get_appmsg("apperror", "dbnotspecified"));
  else  
    {
      stateflag = ADD;
      XtVaSetValues(modelabel, XtNlabel, get_appmsg("appmode", "add"), NULL);
      XtMapWidget(modelabel);
      init_listarray();
      set_commandsensitivity(False);
      XtMapWidget(keydialog);
      XtMapWidget(clearbutton);
      XtMapWidget(donebutton);
      XtSetKeyboardFocus(inputspace, keydialog);
      cleartext(rootdialog);
    }
 }

void 
deletekey_pressed()
/* The delete key mode was chosen, so check to verify that a database
   has been opened, and then initalize the appropriate structures and
   map the keydialog and the done and clear buttons. Put the appropriate mode
   label up as well.  The label is not hard-coded to aid in
   internationalization. */
{
    if (dbfileptr == NULL)
      errormsg_popup(get_appmsg("apperror", "dbnotspecified"));
  else 
    {
      stateflag = DELETEKEY;
      XtVaSetValues(modelabel, XtNlabel, get_appmsg("appmode", "deletekey"), NULL);
      XtMapWidget(modelabel);
      set_commandsensitivity(False);
      XtMapWidget(keydialog);
      XtMapWidget(clearbutton);
      XtMapWidget(donebutton);
      XtSetKeyboardFocus(inputspace, keydialog);
    }
  }

void 
deleteentry_pressed()
/* The delete entry mode was chosen, so check to verify that a database
   has been opened, and then initalize the appropriate structures and
   map the keydialog and the done and clear buttons. Put the appropriate mode
   label up as well.  The label is not hard-coded to aid in
   internationalization. */
{
  if (dbfileptr == NULL)
      errormsg_popup(get_appmsg("apperror", "dbnotspecified"));
  else 
    {
      stateflag = DELETEENTRY;
      XtVaSetValues(modelabel, XtNlabel, get_appmsg("appmode", "deleteentry"), NULL);
      XtMapWidget(modelabel);
      init_listarray();
      set_commandsensitivity(False);
      XtMapWidget(keydialog);
      XtMapWidget(donebutton);
      XtMapWidget(clearbutton);
      XtSetKeyboardFocus(inputspace, keydialog);
    }
}

void 
clear_pressed()
/*  The clear button cleans off the workspace to leave just the keydialog
    remaining. */
{
  switch (stateflag)
    {
    case ADD:
      XtPopdown(subspeechshell);
      XtUnmapWidget(rootdialog);
      XtUnmapWidget(speechparts);
      XtUnmapWidget(addkeyentrieslabel);
      XtUnmapWidget(addkeyentries);
      break;
    case DELETEENTRY:
      XtUnmapWidget(delentrychoiceslabel);
      XtUnmapWidget(delentrychoices);

      break;
    case DELETEKEY:
    case LOOKUP:
      break;
    case MODIFY:
      XtPopdown(subspeechshell);
      XtUnmapWidget(rootdialog);
      XtUnmapWidget(speechparts);
      XtUnmapWidget(modifychoiceslabel);
      XtUnmapWidget(modifychoices);
      break;
    }      
  cleartext(keydialog);
  cleartext(rootdialog);
  init_listarray();
  XawListChange(delentrychoices, listarray, MAX_ENTRIES, 0, False);
  XawListChange(addkeyentries, listarray, MAX_ENTRIES, 0, False);
  XawListUnhighlight(speechparts);
  XawListUnhighlight(subspeechparts);
  XtSetKeyboardFocus(inputspace, keydialog);
}

void 
done_pressed()
/*  To leave a mode, the user pressing the DONE button.  This procedure calls
    the appropriate procedure for each mode to clean up the workspace. */
{
  switch(stateflag)
    {
    case MODIFY:      
      modify_cancelled();
      break;
    case ADD:  
      addkey_cancelled();
      break;;
    case DELETEENTRY:
      delentry_cancelled();
      break;
    case LOOKUP:
      lookupkey_cancelled();
      break;
    case DELETEKEY:
      delkey_cancelled(); 
      break;
    default: break;
    }
}

void 
ok_pressed()
/*  Here, the user has verified an operation specific to the mode that they
    are currently in.  This procedure is a switch to call the correct 'confirmed'
    procedure depending on their current mode. */
{
  switch (stateflag)
    {
    case ADD:
      createentry_confirmed();
      break;
    case MODIFY:
      modify_replace();
      break;
    case DELETEKEY:
      delkey_confirmed();
      break;
    case DELETEENTRY:
      delentry_confirmed();
      break;
    case CREATE:
      confirm_popdown();
      create_newfiles();
      break;
    case DBFILE:
      confirm_popdown();
      switch_dbfiles();
      break;
    case SAVECHANGES:
      confirm_popdown();
      save_changes();
      break;
    case QUIT:
      confirm_popdown();
      quit();
      break;
    }
}  

void 
cancel_pressed()
/*  Here, the user is cancelling an operation specific to the mode that they
    are currently in.  This procedure is a switch to call the correct cancel
    procedure depending on their current mode. */
{
  switch (stateflag)
    {
    case ADD:
      createentry_cancelled();
      break;
    case MODIFY:
      createentry_cancelled();
      break;
    case DELETEKEY:
      confirm_popdown();
      break;
    case DELETEENTRY:
      confirm_popdown();
      break;
    case CREATE:
      stateflag = NOFLAG;
      confirm_popdown();
      XtPopdown(dbdialogshell);
      break;
    case DBFILE:
      stateflag = NOFLAG;
      confirm_popdown();
      XtPopdown(dbdialogshell);
      break;
    case SAVECHANGES:
      stateflag = NOFLAG;
      confirm_popdown();
      break;
    case QUIT:
      stateflag = NOFLAG;
      confirm_popdown();
      break;
    }
}  


static 
void have_key()
/* Once a mode is chosen (lookup, add, etc), the first thing the user must do
   is give a key to be operated on.  This procedure is called once the user
   has done so.  It calls the code to set up the workspace for each of the
   different modes.  */
{
  char *keystring;
  
  keystring = stripspaces(XiSimpleTextGetString(keydialog));
  strcpy(lastkeyptr, keystring);
  if (*(keystring) == '\0')
    done_pressed();
  else
    {
      switch (stateflag)
	{
	case MODIFY: 
	  cleartext(rootdialog);
	  entries_into_listarray(modifychoices);
	  XtMapWidget(rootdialog);
	  XtMapWidget(modifychoiceslabel);
	  XtMapWidget(modifychoices);
	  XtMapWidget(speechparts);
	  XtSetKeyboardFocus(inputspace, NULL);
	  choose_entry();
	  break;
	case ADD:      
	  entries_into_listarray(addkeyentries);
	  XtMapWidget(rootdialog);
	  XtMapWidget(speechparts);
	  XtMapWidget(addkeyentrieslabel);
	  XtMapWidget(addkeyentries);
	  XtSetKeyboardFocus(inputspace, NULL);
	  break;
	case DELETEENTRY:
	  XtMapWidget(delentrychoiceslabel);
	  XtMapWidget(delentrychoices);
	  XtSetKeyboardFocus(inputspace, NULL);
	  choose_entry();
	  break;
	case LOOKUP:
	  lookup_key();
	  break;
	case DELETEKEY:
	  verify_delkey();
	  break;
	default: break;
	}
    }
}

static 
void have_root()
/* A dummy procedure to keep the rootdialog from 1) scrolling and 2)beeping */
{
}


void 
syntax(
       XtAppContext app_con,
       int numparams,
       char **params,
       char *dbname,
       char *logname
       )
/* This procedure processes the calling parameters that aren't processed by
   appinitialize.  See printusage_and_exit for proper parameters. */
{
  Arg arg;
  char filename[DATABUFFER];
  int success = 1, index = 1, tempindex;
  
  *filename = '\0';
  while ((index < numparams) && (success))
    {
      if streq(params[index], "-db")   /* only valid if file already exists */
	{
	  index++;
	  /* check if readonly */
	  tempindex = index+1;
	  while (tempindex < numparams)
	    if (streq(params[tempindex++], "-readonly"))
		readonly = 1;
	  if (index < numparams) 
	    {
	      if (!(success = open_dbfile(params[index])))
		fprintf( stderr, "cannot open %s\n", params[index]);
	    }
	  else 
	    printusage_and_exit(params[0]);  /* no filename specified */
	}
      else if (streq(params[index], "-create"))  /* to create a new file */
	{
	  index++;
	  if (index < numparams) 
	    {
	      success = open_dbfile(params[index]);
	      if (success) 
		{
		  fprintf( stderr, "cannot create %s: file already exists\n",
			  params[index]);
		  close_dbfile();
		  success = 0;
		}
	      else if ((!success) && (errno == ENOENT))
		{
		  success = create_dbfile(params[index], table);
		  if (!success)
		    fprintf( stderr, "cannot create %s\n", params[index]);
		}
	    }
	  else 
	    printusage_and_exit(params[0]);  /* no filename specified */
	}
      else if (streq(params[index], "-log"))   /* only if want a different log */
	{	                               /* name than given by default */
	  index++;
	  if (index < numparams)
	    success = open_logfile(params[index]);
	  else 
	    printusage_and_exit(params[0]);  /* no filename specified */
	}
      else if (streq(params[index], "-nolog"))  /* no log file to be created */
	{
	  if (logfileptr != NULL)
	    close_logfile();
	  nolog = 1;
	}
      else if (streq(params[index], "-readonly"))  /* very useful for protecting */
	{                                          /* against unwanted changes while */
	  readonly = 1;                            /* still allowing look-ups. */
	}
      else
	{
	  printusage_and_exit(params[0]);
	} 
      index++;
    }
  if ((dbfileptr != NULL) && (logfileptr == NULL)  && (!nolog))
    {
      strcat(filename, dbname);
      strcat(filename, ".log");
      success = open_logfile(filename);
    } 
 
  if (!success)
    {
      /* couldn't set up files for some reason */
      XtDestroyApplicationContext(app_con);
      exit(0);
    } 
}

void 
printusage_and_exit(char *params)
/*  This procedure is called when invalid parameter are given to the program.
    It prints out the valid syntax and exits.  */
{
  XtDestroyApplicationContext(app_con);
  fprintf(stderr, "Options: -db <DBfile>        Name of existing DB database file to use\n");
  fprintf(stderr, "         -create <DBfile>    Name of DB database file to be created\n");
  fprintf(stderr, "         -log <logfile>      To be used in place of <DBfile>.log\n");
  fprintf(stderr, "         -readonly           No modifications allowed.\n");
  fprintf(stderr, "         -nolog              Changes will not be written to the log file\n");
  exit(0);
}

void 
lookup_key()
/*  Called when the user hits return after typing in a key.  Retrieves the
    info from the database, printing out the info in the history widget, or
    an error message if the key is not found. The info is NOT placed into
    the log file.  */
{
  char decoded_string[DATABUFFER];
  char formatted_string[DATABUFFER];
  DBT keyitem, content;

  *decoded_string = '\0';
  *formatted_string = '\0';

  keyitem.data = stripspaces(XiSimpleTextGetString(keydialog));
  keyitem.size = strlen(keyitem.data) + 1;
  strcpy(lastkeyptr, keyitem.data);
  /*      Retrieve keyitem from database  */
  if (getinfo_fromdb(&keyitem, &content)) 
    {
      sprintf(formatted_string, get_appmsg("apphistory", "keynotfound"),
	      keyitem.data);
      strcat(formatted_string, "\n\n");
      update_historystring(formatted_string);
/*      errormsg_popup(formatted_string); */
    }
  else
    {
      /* Decode the info and print ot out in the history widget */
      decode(keyitem.data, content.data, content.size, decoded_string);
      sprintf(formatted_string, get_appmsg("apphistory", "lookup"), keyitem.data);
      format_decodedstring(formatted_string, decoded_string);
      strcat(formatted_string, "\n\n");
      update_historystring(formatted_string);
    }
}


void 
lookupkey_cancelled()
/*  Leaving lookup mode.  Reset the working space. */
{
  set_commandsensitivity(True);
  XtUnmapWidget(modelabel);
  XtUnmapWidget(keydialog);
  XtUnmapWidget(modelabel);
  XtUnmapWidget(clearbutton);
  XtUnmapWidget(donebutton);
  XtSetKeyboardFocus(inputspace, NULL);
  stateflag = NOFLAG;
}


void 
verify_delkey(
	      Widget w,
	      XEvent *event,
	      String *params,
	      int num_params
	      )
/* Verify that that is the key that the user wants deleted. */
{
  char newlabel[DATABUFFER];

  *newlabel = '\0';
  strcat(newlabel, get_appmsg("appverify", "deletekey"));
  XtVaSetValues(verifydialog, XtNlabel, newlabel, XtNwidth, strlen(newlabel)*FONTWIDTH,
		NULL);
  confirm_popup();
}


void 
delkey_confirmed()
/* popdown the verify dialog and call the code to delete the key. */
{
  char *textstring;
  
  confirm_popdown();
  textstring = stripspaces(XiSimpleTextGetString(keydialog));
  delete_the_key(dbfileptr, textstring);
}

void 
delkey_cancelled()
/*  Leaving delete key mode.  Reset the working space. */
{
  confirm_popdown();
  set_commandsensitivity(True);
  XtUnmapWidget(modelabel);
  XtUnmapWidget(keydialog);
  XtUnmapWidget(clearbutton);
  XtUnmapWidget(donebutton);
  XtSetKeyboardFocus(inputspace, NULL);
  stateflag = NOFLAG;
}


int 
delete_the_key(DB *dbfileptr, char *textstring)
/*  Called from delete entry or delete key to delete a key from the database.
    Since all changes are made to a temporary database, there must be special
    way to mark deleted keys, since simply deleting them from the temp database
    would just allow them to be brought in from the permanent one the next
    time the key was accessed.  This is done by 'zero-ing out' the content
    of the key.  Note that this has ramifications for the encoding scheme,
    since the first character in the encoded string is the index into the
    encoded array.  Therefore the first element of the encoded array should
    not be given an inflectional meaning.  This procedure also takes care of 
    generating the appropriate error messages and updating the history and 
    log files. */
{
  char historystring[DATABUFFER];
  int success;
  
  *historystring = '\0';
  success = bringinto_tempdb(textstring);
  if (!success)
    success = db_zeroout_content(tempdbfileptr, textstring);
  switch (success)
    {
    case -1:
      errormsg_popup(get_appmsg("apperror", "keynotdeleted"));
      break;
    case 0: 
      changedflag = TRUE;
      sprintf(historystring, get_appmsg("apphistory", "deletekey"), textstring);
      update_historystring(historystring);
      update_logfile(historystring);
      break;
    case 1:
      errormsg_popup(get_appmsg("apperror", "keynotfoundwo"));
      break;
    }
  return(success);
}



void 
choose_entry()
/* For both modify and delete entry, the user must choose an entry to act upon.
   This procedure calls the code to have the entries put into listarray,
   and prints an error message if the key is not found.  */
   
{
  char decoded_string[DATABUFFER];
  char *keystring;
  int result;

  *decoded_string = '\0';
  keystring = stripspaces(XiSimpleTextGetString(keydialog));
  switch (stateflag)
    {
    case DELETEENTRY:
      result = entries_into_listarray(delentrychoices);
      break;
    case MODIFY:
      result = entries_into_listarray(modifychoices);
      if ((!result) && (listindex == 0)) 
	{
	  XawListHighlight(modifychoices, listindex);
	  modify_chosen();
	}
      break;
    }
  if (result)
    {
      sprintf(decoded_string, get_appmsg("apperror", "keynotfoundw"), keystring);
      errormsg_popup(decoded_string);
    }
}

void 
delentry_verify()
/*  Once the user chooses an entry, verify that that was indeed the entry
    that she wanted to delete.  If there is only 1 entry for a key, warn
    the user that entire key will be deleted. */
{
  char newlabel[DATABUFFER];
  int nstrings;
  
  *newlabel = '\0';
  XtVaGetValues(delentrychoices, XtNnumberStrings, &nstrings, NULL);
  if (nstrings == 1)
    {
      strcpy(newlabel, get_appmsg("appverify", "deletewarning"));
      XtVaSetValues(verifydialog, XtNlabel, newlabel, XtNwidth,
		    strlen(newlabel) * FONTWIDTH, NULL);
    }
  else
    {
      strcpy(newlabel, get_appmsg("appverify", "deleteentry"));
      XtVaSetValues(verifydialog, XtNlabel, newlabel, XtNwidth,
		    strlen(newlabel) * FONTWIDTH, NULL);
    }
  confirm_popup();
}

void 
delentry_confirmed()
/* Once the user has confirmed that that is the entry that she wanted
   to delete, this procedure checks to see if it is the only entry for that
   key. If it is, then the whole key is deleted, otherwise the entries are
   put into listarray, an UNencoded string minus the entry to be deleted is
   created, the list is updated, and the code called to delete the entry */
{
  XawListReturnStruct *chosenitem;
  char decoded_string[DATABUFFER];
  char tabbed_string[DATABUFFER];
  char *keystring;
  int nstrings, flag;
  
  *decoded_string = '\0';
  *tabbed_string = '\0';
  confirm_popdown();
  XtVaGetValues(delentrychoices, XtNnumberStrings, &nstrings, NULL);
  chosenitem = XawListShowCurrent(delentrychoices);
  keystring = stripspaces(XiSimpleTextGetString(keydialog));
  if (nstrings == 1)  /* key has only 1 entry */
    {
      flag = delete_the_key(dbfileptr, keystring);
      if (!flag) 
	{
	  init_listarray();
	  XawListChange(delentrychoices, listarray, MAX_ENTRIES, 0, False);
	}
      else XawListUnhighlight(delentrychoices);
    }      
  else
    {
  /*    entries_into_listarray(delentrychoices);
      XawListChange(delentrychoices, listarray, listindex, 0, False);*/
      array_to_decoded(listarray, decoded_string, nstrings, chosenitem->list_index);
      replace_tab(chosenitem->string, tabbed_string);
      delete_the_entry(dbfileptr, tabbed_string, keystring, decoded_string);
      entries_into_listarray(delentrychoices);
    }
}

void 
delentry_cancelled()
/*  Leaving delete entry mode.  Reset the working space. */
{
  confirm_popdown();
  set_commandsensitivity(True);
  XtUnmapWidget(modelabel);
  XtUnmapWidget(keydialog);
  XtUnmapWidget(donebutton);
  XtUnmapWidget(clearbutton);
  XtUnmapWidget(delentrychoiceslabel);
  XtUnmapWidget(delentrychoices);
  XtSetKeyboardFocus(inputspace, NULL);
  stateflag = NOFLAG;
}


void 
delete_the_entry(
		 DB *dbfileptr,
		 char *delstring,
		 char *keystring,
		 char *contentstring
		 )
/* When an entry is deleted, a new string is created without the entry and
   sent here.  This procedure replaces the old content with the new one
   without that entry.  This procedure takes care of error messages, as well
   as updating the history widget and log file. */
{
  char historystring[DATABUFFER];
  int problem;
  
  *historystring = '\0';
  problem = bringinto_tempdb(keystring);
  if (!problem)
    problem = C_db_replace_encoded(tempdbfileptr, keystring, contentstring);
  switch (problem)
    {
    case -2:
      errormsg_popup(get_appmsg("apperror", "entrynotencoded"));
      break;
    case -1:
      errormsg_popup(get_appmsg("apperror", "entrynotdeleted"));
      break;
    case 0: 
      changedflag = TRUE;
      sprintf(historystring, get_appmsg("apphistory", "deleteentry"),
	      delstring, keystring);
      update_historystring(historystring);
      update_logfile(historystring);
      break;
    }
}

void 
addkey_insert()
/*  Does some error checking to ensure that the key has not
    been changed, places the entry to be added (which was stored 
    'out-of-bounds' in listarray into decoded string and send it off 
    to be added.  If the entry was successfully added, then
    update the info in the workspace to show the new entry.  */
{
  char decoded_string[DATABUFFER];
  char *keystring;
  int result = 0;
    
  *decoded_string = '\0';
  keystring = stripspaces(XiSimpleTextGetString(keydialog));
  if (!streq(keystring, lastkeyptr))
    errormsg_popup(get_appmsg("apperror", "keychanged"));
  else
    {
      replace_tab(listarray[listindex+1], decoded_string); 
      /* +1 because newentry isn't 'officially' on list yet */
      result = add_the_key(keystring, decoded_string);
      if (!result) 
	{
	  listindex++;
	  XawListChange(addkeyentries, listarray, listindex+1, 0, True);
	}
    }
}


void 
addkey_cancelled()
/*  Leaving add mode.  Reset the working space. */
{
  set_commandsensitivity(True);
  XtUnmapWidget(modelabel);
  XtPopdown(subspeechshell);
  XtUnmapWidget(keydialog);
  XtUnmapWidget(rootdialog);
  XtUnmapWidget(speechparts);
  XtUnmapWidget(addkeyentrieslabel);
  XtUnmapWidget(addkeyentries);
  XtUnmapWidget(clearbutton);
  XtUnmapWidget(donebutton);
  XawListUnhighlight(speechparts);
  XawListUnhighlight(subspeechparts);
  XtSetKeyboardFocus(inputspace, NULL);
  stateflag = NOFLAG;
}


int 
add_the_key(char *keystring, char *decodedstring)
/*  Called from add to add a new entry to a key, an also from 
    generate_automatically.  Works the same whether
    the word existed before or not.  Duplicate entries are taken take of
    at a lower level.  This procedure takes care of generating the
    appropriate error messages and updating the history and log files. */
{
  char historystring[DATABUFFER];
  int problem;
  
  *historystring = '\0';
  sprintf(historystring, get_appmsg("apphistory", "addentry"), keystring, decodedstring);
/* do this first because put_encoded flips the entry and the infl data (decodedstring) */

  bringinto_tempdb(keystring);
  problem = C_db_put_encoded(tempdbfileptr, keystring, decodedstring);
  switch (problem)
    {
    case -2:
      errormsg_popup(get_appmsg("apperror","cantencodeentry"));
      break;
    case -1:
      errormsg_popup(get_appmsg("apperror", "entrynotadded"));
      break;
    case 0: 
      changedflag = TRUE;
      /* see above sprintf for message */
      update_historystring(historystring);
      update_logfile(historystring);
      XtPopdown(subspeechshell);
      XtUnmapWidget(rootdialog);              /*  Force user hit <RET> when */
      XtUnmapWidget(addkeyentrieslabel);      /*  giving a new key to add */
      XtUnmapWidget(addkeyentries);
      XtUnmapWidget(speechparts);
      XawListUnhighlight(speechparts);
      break;
    }
  return(problem);
}

void 
modify_chosen()
/* An entry has been chosen to modify.  Find the root and place it into
   the rootdialog.  This is done to help the user.  She may subsequently decide
   to modify the root as well.  */
{
  char *tempptr;
  XawListReturnStruct *chosenitem;
  char tempstring[DATABUFFER];
  char tabbed_string[DATABUFFER];
  
  *tempstring = '\0';
  *tabbed_string = '\0';
  chosenitem = XawListShowCurrent(modifychoices);
  replace_tab(chosenitem->string, tabbed_string);
  tempptr = strchr(tabbed_string, '\t');
  strncat(tempstring, tabbed_string, tempptr  - tabbed_string);
  XiSimpleTextSetString(rootdialog, tempstring);
}

void 
modify_replace()
/*  Does some error checking to ensure that the key has not
    been changed, then creates the new content for the key,
    including the entry that's been modified.   The new entry was
    stored 'out-of-bounds' in listarray while verifying with the user,
    now move it to its proper place in the array, overwriting the old entry.
    Call the code to change the info in the database, and update the entry
    info in the workspace if the change was successfully accomplished.  */
{
  XawListReturnStruct *chosenitem;
  char decodedstring[DATABUFFER], backup[DATABUFFER];
  char *keystring;
  int result;

  *decodedstring = '\0';
  *backup = '\0';
  confirm_popdown();
  XtPopdown(subspeechshell);
  chosenitem = XawListShowCurrent(modifychoices);
  keystring = stripspaces(XiSimpleTextGetString(keydialog));
  if (!streq(keystring, lastkeyptr))
    errormsg_popup(get_appmsg("apperror","keychanged"));
  else if (chosenitem->list_index == XAW_LIST_NONE)
    errormsg_popup(get_appmsg("apperror", "entrynotchosen"));
  else
    {
      /* move the new entry into its proper place.  Put the old version
         into a safe place in case the replace fails.  (This is done before
         the replace so that existing procedures to convert the list array
	 into a string can be used as is. */
      strcpy(backup, listarray[chosenitem->list_index]);
      strcpy(listarray[chosenitem->list_index], listarray[listindex+1]);
      strcpy(listarray[listindex+1], backup);
      array_to_decoded(listarray, decodedstring, listindex+1, 100);
      result = replace_the_key(keystring, decodedstring);
      switch (result)
	{
	case 0:  /* replace successful.  Update workspace */
	  XawListChange(modifychoices, listarray, listindex+1, 0, True);
	  break;
	default: /* replace unsuccessul.  Move the old entry back into the list */
	  strcpy(listarray[chosenitem->list_index], backup);
	  break;
	}
    }
  XawListUnhighlight(speechparts);
}


void 
modify_cancelled()
/*  Leaving modify mode.  Reset the working space. */
{
  set_commandsensitivity(True);
  XtUnmapWidget(modelabel);
  XtPopdown(subspeechshell);
  XtUnmapWidget(keydialog);
  XtUnmapWidget(rootdialog);
  XtUnmapWidget(speechparts);
  XtUnmapWidget(modifychoiceslabel);
  XtUnmapWidget(modifychoices);
  XtUnmapWidget(clearbutton);
  XtUnmapWidget(donebutton);
  XawListUnhighlight(speechparts);
  XawListUnhighlight(subspeechparts);
  XtSetKeyboardFocus(inputspace, NULL);
  stateflag = NOFLAG;
}

int 
replace_the_key(char *keystring, char *decodedstring)
/*  Called from modify to replace an old key/content pair with a
    new one.  This procedure takes care of generating the
    appropriate error messages and updating the history and log files. */
    
{
  char historystring[DATABUFFER];
  int problem;

  *historystring = '\0';
  /*  The change will be made to the temporary database, so bring the key
      over if it hasn't already been done. */
  problem = bringinto_tempdb(keystring);
  if (!problem)
    /* Replace the old entry with the new one given */
    problem = C_db_replace_encoded(tempdbfileptr, keystring, decodedstring);
  switch (problem)
    {
    case -2:
      errormsg_popup(get_appmsg("apperror", "cantencodeentry"));
      break;
    case -1:
      errormsg_popup(get_appmsg("apperror", "entrynotmodified"));
      break;
    case 0: 
      /*  Successfully replaced the entry.  Update the history widget and log */
      /*  file.  Set changedflag to TRUE to indicate that changes have been made. */
      changedflag = TRUE;
      sprintf(historystring, get_appmsg("apphistory", "modify"),
	      listarray[listindex+1],
	      listarray[XawListShowCurrent(modifychoices)->list_index], 
	      keystring);
      update_historystring(historystring);
      update_logfile(historystring);
      break;
    }
  return(problem);
}

void 
putup_subparts()
/*  Put up the subparts window with the appropriate subparts depending
    on the part of speech chosen.  Some parts of speech don't have options
    available. */
{
  XawListReturnStruct *chosen_speechpart;
  Position x,y;
  Boolean up;
  

  chosen_speechpart = XawListShowCurrent(speechparts);
  /* These don't have options available */
  if (streq(chosen_speechpart->string, "Preposition") ||
      streq(chosen_speechpart->string, "Verb Particle") ||
      streq(chosen_speechpart->string, "Complementizer") ||
      streq(chosen_speechpart->string, "Interjection") ||
      streq(chosen_speechpart->string, "Punctuation") ||
      streq(chosen_speechpart->string, "Genitive") ||
      streq(chosen_speechpart->string, "Conjunction")) 
    {
      XtPopdown(subspeechshell);
      verify_inflinfo();
    }
  else
    {
      if streq(chosen_speechpart->string, "Noun")
	XawListChange(subspeechparts, noun_speechparts, 0, 0, True);
      else if streq(chosen_speechpart->string, "Proper Noun")
	XawListChange(subspeechparts, propnoun_speechparts, 0,0,True);
      else if streq(chosen_speechpart->string, "Verb")
	XawListChange(subspeechparts, verb_speechparts, 0,0,True);
      else if streq(chosen_speechpart->string, "N/V Contraction")
	XawListChange(subspeechparts, nvcontraction_speechparts, 0,0, True);
      else if streq(chosen_speechpart->string, "V/V Contraction")
	XawListChange(subspeechparts, vvcontraction_speechparts, 0,0, True);
      else if streq(chosen_speechpart->string, "Pronoun")
	XawListChange(subspeechparts, pronoun_speechparts, 0,0,True);
      else if streq(chosen_speechpart->string, "Adjective")
	XawListChange(subspeechparts, adj_speechparts, 0,0,True);
      else if streq(chosen_speechpart->string, "Adverb")
	XawListChange(subspeechparts, adv_speechparts, 0,0,True);
      else if streq(chosen_speechpart->string, "Determiner")
	XawListChange(subspeechparts, det_speechparts, 0,0,True);

      XtPopdown(subspeechshell);
      XtVaGetValues(topwindow, XtNx, &x, XtNy, &y, NULL);
      XtVaSetValues(subspeechshell, XtNx, x+SPEECH_XOFFSET, XtNy, y+SPEECH_YOFFSET, NULL);
      XtPopup(subspeechshell, XtGrabNone);
    }
  
}

void 
verify_inflinfo()
/*  This procedure is called after the part of speech and other infl info has
    been chosen.  It checks to make sure that there is a root, and that the user is
    not trying to add past the maximum number of entries.  It then creates the
    new entry and asks the user to confirm that this is the entry that they
    wanted.  This procedure is used in modify and add.  */
{
  char newentry[DATABUFFER];
  char newlabel[DATABUFFER];
  char untabbed_entry[DATABUFFER];
  XawListReturnStruct *chosen_speechpart, *chosen_subspeechpart;
  char *keystring, *rootstring;
  
  *newentry = '\0';
  *newlabel = '\0';
  *untabbed_entry = '\0';
  keystring = stripspaces(XiSimpleTextGetString(keydialog));
  rootstring = stripspaces(XiSimpleTextGetString(rootdialog));
  if (*rootstring == '\0')    /* check to verify there is a root word provided */
    {
      errormsg_popup(get_appmsg("apperror", "rootmissing"));
      XtPopdown(subspeechshell);
      XawListUnhighlight(speechparts);
    }
  else if ((stateflag == ADD) && (listindex+1 >= MAX_ENTRIES))
  /* make sure not trying to add past the maximum number of entries */
    {
      errormsg_popup(get_appmsg("apperror", "tryexceedmaxentries"));
      XtPopdown(subspeechshell);
      XawListUnhighlight(speechparts);
    }
  else 
    {
      /* Create the new entry */
      sprintf(newlabel, get_appmsg("appverify", "newentry"));
      strcat(newentry, rootstring);
      chosen_speechpart = XawListShowCurrent(speechparts);
      chosen_subspeechpart = XawListShowCurrent(subspeechparts);

      /* Check for automatic generation */
      if (!generate_automatically(keystring, rootstring, chosen_speechpart->string,
				  chosen_subspeechpart->string))
	{
	  if (streq(chosen_speechpart->string, "Noun")) 
	      strcat(newentry, "\tN");
	  else if (streq(chosen_speechpart->string, "Proper Noun"))
	    strcat(newentry, "\tPropN");
	  else if (streq(chosen_speechpart->string, "Verb"))
	    strcat(newentry, "\tV");
	  else if (streq(chosen_speechpart->string, "N/V Contraction"))
	    strcat(newentry, "\tNVC");
	  else if (streq(chosen_speechpart->string, "V/V Contraction"))
	    strcat(newentry, "\tVVC");
	  else if (streq(chosen_speechpart->string, "Adjective"))
	    strcat(newentry, "\tA");
	  else if (streq(chosen_speechpart->string, "Adverb"))
	    strcat(newentry, "\tAdv");
	  else if (streq(chosen_speechpart->string, "Pronoun"))
	    strcat(newentry, "\tPron");
	  else if (streq(chosen_speechpart->string, "Determiner"))
	    strcat(newentry, "\tDet");
	  else if (streq(chosen_speechpart->string, "Verb Particle"))
	    {
	      chosen_subspeechpart = NULL;
	      strcat(newentry, "\tPart");
	    }
	  else if (streq(chosen_speechpart->string, "Preposition"))
	    {
	      chosen_subspeechpart = NULL;
	      strcat(newentry, "\tPrep");
	    }
	  else if (streq(chosen_speechpart->string, "Genitive"))
	    {
	      chosen_subspeechpart = NULL;
	      strcat(newentry, "\tG");
	    }
	  else if (streq(chosen_speechpart->string, "Punctuation"))
	    {
	      chosen_subspeechpart = NULL;
	      strcat(newentry, "\tPunct");
	    }
	  else if (streq(chosen_speechpart->string, "Complementizer"))
	    {
	      chosen_subspeechpart = NULL;
	      strcat(newentry, "\tComp");
	    }
	  else if (streq(chosen_speechpart->string, "Interjection"))
	    {
	      chosen_subspeechpart = NULL;
	      strcat(newentry, "\tI");
	    }
	  else if (streq(chosen_speechpart->string, "Conjunction"))
	    {
	      chosen_subspeechpart = NULL;
	      strcat(newentry, "\tConj");
	    }
	  
	  if (chosen_subspeechpart != NULL)
	    if (!streq(chosen_subspeechpart->string, "NONE"))
	      {
		strcat(newentry, " ");
		strcat(newentry, chosen_subspeechpart->string);
	      }
	  
	  /* ask user to verify new entry */
	  remove_tab(newentry, untabbed_entry);
	  strcat(newlabel, untabbed_entry);
	  strcpy(listarray[listindex+1], untabbed_entry);
	  XtVaSetValues(verifydialog, XtNlabel, newlabel, XtNwidth,
			strlen(newlabel)*FONTWIDTH, NULL);
	  confirm_popup();
	}
    }
}

void 
createentry_confirmed()
/*  Called when user chooses OK option after choosing the inflectional
    info in add only.  Modify calls modify_replace */
{
  confirm_popdown();
  XtPopdown(subspeechshell);
  XawListUnhighlight(speechparts);
  addkey_insert();
}

void 
createentry_cancelled()
/*  Called when user chooses CANCEL option after choosing the inflectional
    info in add and modify */
{
  confirm_popdown();
  XawListUnhighlight(speechparts);
  XtPopdown(subspeechshell);
}

void 
update_historystring(char *newstr)
/* Update the historytext widget to reflect whatever new has just happened. */
{
  XawTextPosition insertpoint;
  XawTextBlock textblock;

  textblock.firstPos = 0;
  textblock.length = strlen(newstr);
  textblock.ptr = newstr;
  textblock.format = FMT8BIT;
  XtVaSetValues(historytext, XtNeditType, XawtextAppend, NULL); 
  insertpoint = XawTextGetInsertionPoint(historytext);
  XawTextReplace(historytext, insertpoint, insertpoint, textblock);
  XawTextSetInsertionPoint(historytext, insertpoint+ strlen(newstr)+1);
  XtVaSetValues(historytext, XtNeditType, XawtextRead, NULL);
}

int 
generate_automatically(
		       char *baseword,
		       char *rootword,
		       char *speechstring,
		       char *subspeechstring
		       )
/* Returns 1 if inflections are generated automatically;
           0 otherwise.  */
{
  int num_inflections, index;
  char keyarray[MAX_STD_INFL][DATABUFFER];
  char contentarray[MAX_STD_INFL][DATABUFFER];
  
  if (streq(subspeechstring, "*STD INFLECTIONS*"))
    {
      if (streq(speechstring, "Noun"))
	{
	  num_inflections = create_noun_inflections(baseword, rootword, 
						    keyarray, contentarray);
	}
      else if (streq(speechstring, "Proper Noun"))
	{
	  num_inflections = create_propn_inflections(baseword, rootword, 
						     keyarray, contentarray);
	}
      else if (streq(speechstring, "Verb"))
	{
	  num_inflections = create_verb_inflections(baseword, rootword, 
						    keyarray, contentarray);
	}
      else if (streq(speechstring, "Adjective"))
	{
	  num_inflections = create_adj_inflections(baseword, rootword, 
						   keyarray, contentarray);
	}
      else return(0);
      
      for (index = 0; index < num_inflections; index++)
	  add_the_key(keyarray[index], contentarray[index]);
      return(1);
    }
  else return(0);
}


void 
format_decodedstring(char *formattedstr, char *decodedstr)
/*  Format the decoded string that it will look nice when displayed in
    the history area */
{
  char *hashptr, *newstart;

  newstart = decodedstr;
  /* Don't forget to skip over the 1st two characters that contain the */
  /* inflectional array info and # of characters in common.  */
  while  ((hashptr = strchr(newstart+2, SEP_CHAR)) != NULL) 
    {
      strncat(formattedstr, newstart, hashptr-newstart);
      strcat(formattedstr, "\n         ");
      newstart = hashptr + 1;
    }
  strcat(formattedstr, newstart);
  strcat(formattedstr, "\n");
}

int 
entries_into_listarray(Widget listwidget)
/*  This procedure gets the key from the keydialog widget, looks up the
    associated entries in the database, and then calls the procedures to
    decode the entries and place them into the listarray.  If there are
    no entries, then it zeros out the listarray. */
/* Returns -1 if error,
            0 if success,
            1 if key not found. */
{
  char decoded_string[DATABUFFER];
  DBT keyitem, content;
  int result = -1;

  *decoded_string = '\0';
  /* Get key from keydialog */
  keyitem.data = stripspaces(XiSimpleTextGetString(keydialog));
  keyitem.size = strlen(keyitem.data) + 1;
  /*      Retrieve keyitem from database  */
  result = getinfo_fromdb(&keyitem, &content);
  switch (result)
    {
    case 0:  /* Item found, decode and place into listarray */
      decode(keyitem.data, content.data, content.size, decoded_string);
      decoded_to_array(listarray, &listindex, decoded_string);
      XawListChange(listwidget, listarray, listindex+1, 0, TRUE);
      break;
    case 1:  /* Item not found. Zero out listarray */
      init_listarray();
      XawListChange(listwidget, listarray, MAX_ENTRIES, 0, TRUE);
      break;
    }
  return(result);
}


void 
decoded_to_array(
		 char **larray,
		 int *lindex,
		 char *dstring
		 )
/* Convert an UNencoded string into a list of entries. */
{
  char tabbed_string[DATABUFFER];
  int numchars;
  char *hashptr, *restofstring;

  *tabbed_string = '\0';
  if (*dstring == '\0')
    *lindex = -1;
  else
    {
      *lindex = 0;
      restofstring = dstring;

      /* Separate into entries according to the hash marks */
      /* Don't forget to skip over the 1st two characters that contain the */
      /* inflectional array info and # of characters in common.  */
      while  (((hashptr = strchr(restofstring+2, SEP_CHAR)) != NULL) &&
	      (*lindex < MAX_ENTRIES))
	{
	  *tabbed_string = '\0';
	  numchars = hashptr-restofstring;
	  strncat(tabbed_string, restofstring, numchars);
	  remove_tab(tabbed_string, *(larray+*lindex));
	  (*lindex)++;
	  restofstring = hashptr+1;
	}
      if (*lindex < MAX_ENTRIES)
	remove_tab(restofstring, *(larray+*lindex));
      else
	errormsg_popup(get_appmsg("apperror", "exceedsmaxentries"));
    }
}


void 
array_to_decoded(
		 char **larray,
		 char *decoded_string,
		 int nstrings,
		 int delindex
		 )
/* Create an UNencoded character string containing all the of entries for a key
found in larray, returning it in decoded_string.  Delindex gives the (optional)
index of an array element to be deleted. */
{
  char tabbed_string[DATABUFFER];
  int i;
  
  *decoded_string = '\0';
  *tabbed_string = '\0';
  if (nstrings == 1) 
    replace_tab(*larray, decoded_string);
  else
    {
      for (i = 0; i < nstrings; i++)    /* Go through array, adding to string */
	if (i != delindex)              /* Don't add in the one to be deleted */
	  {
	    replace_tab(*(larray+i), tabbed_string);
	    strcat(decoded_string, tabbed_string);
	    strcat(decoded_string, SEP_STRING);
	  }
      i = strlen(decoded_string);
      decoded_string[i-1] = '\0';   /* Get rid of last hashmark */
    }
}

int 
bringinto_tempdb(char *keystring)
/* This procedure brings a key/content pair into the temporary DB if
it isn't already in there.  A special check is needed to see if a key
has been marked for deletion.  */
{
  /* Returns -1 if not successful,
              0 if successful  
	      1 if not found  */
  DBT key, content;

  key.data = keystring;
  key.size = strlen(key.data)+1;
  if ((tempdbfileptr->get)(tempdbfileptr, &key, &content, NULL))
    if ((dbfileptr->get)(dbfileptr, &key, &content, NULL))
      return (1);           /* Not in either database */
    else 
      /* in the orginal, copy into the temp */
      return ((tempdbfileptr->put)(tempdbfileptr, &key, &content, R_PUT));
  else
    if (*(content.data) == '\0')
      /* found in temp, but content field is NULL, so item has been 'deleted' */
      return(1);
    else
      /* is already in the temp database */
      return(0);
}

int 
getinfo_fromdb(DBT *key, DBT *content)
/* Check the temporary database, then the permanent one for
   information on a keyword.  For deleted keys, the entry is
   still in the temp DB, but the content is NULL. */
{
  /* Returns 0 if successful,
             1 if not found */

  if ((tempdbfileptr->get)(tempdbfileptr, key, content, NULL))
    return((dbfileptr->get)(dbfileptr, key, content, NULL));
  else 
    if (*(content->data) == '\0')  /* item has been 'deleted' */
      return(1);
    else return (0);
}


void 
init_listarray()
/*  There is one globally available array to hold the entries from a key.  It */
/*  is used for delete entry, add, and modify.  */
{
  int index;
  
  for (index = 0; index < MAX_ENTRIES; index++)
    **(listarray+index) = '\0';
  listindex = -1;
}


static void 
get_newdbname(
	      Widget w,
	      XEvent *event,
	      String *params,
	      int num_params
	      )
/*  This procedure is called when a new filename is typed into the dbdialog */
 /*  widget popped up in response to 'Choose Database' under the File menu.  It */
 /*  checks to see if any changes have been made and not saved.  If so, it pops */
 /*  up a verify shell to see if the user wishes to discard those changes.  If */
 /*  yes, then switch_dbfiles is called.  Otherwise, everything will pop back */
 /*  down so that the user can save the file.  */
{
  char tempstring[DATABUFFER];

  *tempstring = '\0';
  if (changedflag)
    {
      strcat(tempstring, get_appmsg("appverify", "losechanges"));
      XtVaSetValues(verifydialog, XtNlabel, tempstring, XtNwidth, strlen(tempstring)*FONTWIDTH, NULL);
      confirm_popup();
    }
  else
    switch_dbfiles();
}

void 
switch_dbfiles()
/*  This procedure is called from get_newdbname when there are no changes to be */
/*  saved, or is the person has decided to discard those changes.  If changes */
/*  to the database had been made and successfully saved, then the .flat must be */
/*  updated to keep the files consistent.  spinoff_flat() takes care of this.  If */
/*  the filename is empty string, nothing changes.  The old file pointer remains, */
/*  as does the header for xmdbm.  Otherwise, the procedure tries to open the */
/*  file.  If the file can't be opened because it doesn't exist, then a verify */
/*  shell is set up, and create_newfiles() is called from that as necessary. */
/*  If the file does exist and can be opened, then the appropriately named log */
/*  file is opened as well.  If the user wishes to use a non-default log file, */
/*  then s/he must open it AFTER choosing the db file name.  That will open */
/*  the default log name.  */
{
  char errtext[DATABUFFER];
  char logfile[DATABUFFER];

  *errtext = '\0';
  *logfile = '\0';
  strcpy(dbnameptr, XawDialogGetValueString(dbfiledialog));
  spinoff_flat();
  if (*dbnameptr != '\0')
    if (!open_dbfile(dbnameptr))
      {
	if (errno == ENOENT)
	  {
	    stateflag = CREATE;
	    strcat(errtext, get_appmsg("appverify", "createnewdb"));
	    XtVaSetValues(verifydialog, XtNlabel, errtext, XtNwidth, strlen(errtext)*FONTWIDTH, NULL);
	    confirm_popup();
	  }
	else
	  {
	    XtPopdown(dbdialogshell);
	    sprintf(errtext, get_appmsg("apperror", "dbcantopen"), dbnameptr);
	    errormsg_popup(errtext);
	  }
      }
    else
      {
	XtPopdown(dbdialogshell);
	if (!nolog)
	  {
	    strcat(logfile, dbnameptr);
	    strcat(logfile, ".log");
	    if (!open_logfile(logfile))
	      {
		sprintf(errtext, get_appmsg("apperror", "logcantopen"), logfile);
		errormsg_popup(errtext);
	      }
	  }
	stateflag = NOFLAG;
      }
  else XtPopdown(dbdialogshell);
}

void 
create_newfiles()
/*  This procedure is the calling procedure for creating a new database file */
/*  and its corresponding log file.  It is called when the ok button is */
/*  pressed when the user is asked if s/he wants to create a new file since */
/*  the filename asked for is not found.  */
{
  char errtext[DATABUFFER];
  char logfile[DATABUFFER];

  *errtext = '\0';
  *logfile = '\0';
  XtPopdown(dbdialogshell);
  dbnameptr = XawDialogGetValueString(dbfiledialog);
  if (!create_dbfile(dbnameptr, table))
    {
      sprintf(errtext, get_appmsg("apperror", "dbcantcreate"), dbnameptr);
      errormsg_popup(errtext);
    }
  else
    {
      if (!nolog)
	{
	  strcat(logfile, dbnameptr);
	  strcat(logfile, ".log");
	  if (!open_logfile(logfile))
	    {
	      sprintf(errtext, get_appmsg("apperror", "logcantopen"), logfile);
	      errormsg_popup(errtext);
	    }
	}
      stateflag = NOFLAG;
    }
}

int 
open_dbfile(char *dbfilename)
/*  This procedure opens a previously existing DB database file.  create_dbfile() */
/*  is used to create a new file.  If the file is successfully opened, the */
/*  header for xmdbm will show dbfilename.  If it cannot be opened, it will */
/*  show no filename.  If the filename is blank, it will not affect the */
/*  current dbfileptr, nor the name shown on the header. */
{
  char tempstring[DATABUFFER];
  
  *tempstring = '\0';
  strcpy(dbnameptr, dbfilename);
  if (*dbfilename != '\0')
    {
      if (dbfileptr != NULL)
	close_dbfile();
      if (readonly)
	dbfileptr = hash_open(dbfilename, O_RDONLY, 0440, NULL);
      else
	dbfileptr = hash_open(dbfilename, O_RDWR, 0440, NULL);
      if (dbfileptr == NULL) 
	{
	  sprintf(tempstring, "%s%s%s%s", get_appmsg("appname", "title"), " (", VERSION_NUMBER, ")");
	  XtVaSetValues(topwindow, XtNtitle, tempstring, NULL);
	  return(0);
	}
      else
	{
	  sprintf(tempstring, "%s%s%s%s%s", get_appmsg("appname", "title"), " (", VERSION_NUMBER,
		  "): ", dbfilename);
	  XtVaSetValues(topwindow, XtNtitle, tempstring, NULL);
	  stat(dbfilename, &dbfileinfo);
	  return(1);
	}
    }
  return(1);
  }

int 
create_dbfile(char *dbfilename, HASHINFO table)
/*  This procedure creates a new DB file called dbfilename.   If it is */
/*  successfully opened, then the header on the xmdbm window is changed */
/*  to reflect the new database name.  The HASHINFO table must be declared at */
 /*  the top level (not necessarily globally) and *passed* into the procedure */
 /*  that called hash_open.  Otherwise hash_open will fail.  It is not known */
 /*  why this occurs.  */
{
  char tempstring[DATABUFFER];
  
  *tempstring = '\0';
  strcpy(dbnameptr, dbfilename);
  if (*dbfilename != '\0')
    if (!(dbfileptr = hash_open(dbfilename, O_CREAT|O_RDWR, 0660, &table ))) 
      {
	fprintf( stderr, "cannot create %s\n", dbfilename);
	return(0);
      }
    else 
      {
	sprintf(tempstring, "%s%s%s%s%s", get_appmsg("appname", "title"), " (", VERSION_NUMBER,
		  "): ", dbfilename);
/*	strcat(tempstring, "XMDB Maintenance: ");
	strcat(tempstring, dbfilename); */
	XtVaSetValues(topwindow, XtNtitle, tempstring, NULL);
	stat(dbfilename, &dbfileinfo);
	return(1);
      }
  else return(1);
}

int 
close_dbfile()
/*  Whenever the database file is closed, the temporary files need to be reset, */
/*  since any changes made will have been made to the old (now closed) */
/*  database.  A check is made before this point to warn the user of any */
/*  unsaved changes. */
{
  if (dbfileptr != NULL)
    (dbfileptr->close)(dbfileptr);
  dbfileptr = NULL;
  reset_tempfiles(table);
}

static void 
get_newlogname(
	       Widget w, 
	       XEvent *event, 
	       String *params, 
	       int num_params
	       )
/*  This procedure is called when the <RET> is pressed in logfiledialog.  It is */
/*  accessed from the 'Choose Log file' option in the File menu.  A simple */
/*  check to see if no change was made to the name is made.  The procedure */
 /*  should work fine with or without this.  */
{
  char errtext[DATABUFFER];
  
  *errtext = '\0';
  XtPopdown(logdialogshell);
  strcat(errtext, lognameptr);
  lognameptr = XawDialogGetValueString(logfiledialog);
  if (!streq(lognameptr, errtext))
    if (!open_logfile(lognameptr))
      {
	sprintf(errtext, get_appmsg("apperror", "logcantopen"), lognameptr);
	errormsg_popup(errtext);
      }
}

int 
open_logfile(char *logfilename)
/*  open log file for appending.  File does not actually get written to until */
/*  save_database() is called.  */
{
  
  if (logfileptr != NULL)
    close_logfile();
  if (*logfilename != '\0') 
    {
      strcpy(lognameptr, logfilename);
      if (!(logfileptr = fopen(logfilename, "a"))) 
	{
	  fprintf( stderr, "cannot open %s\n", logfilename);
	  return(0);
	}
      return(1);
    }
  else return(1);
}

int 
close_logfile()
/* close the real log file, and set the ptr to NULL.  The temp log file is */
/* closed in close_tempfiles(). */
{
  *lognameptr = '\0';
  if (logfileptr != NULL)
    fclose(logfileptr);
  logfileptr = NULL;
}

void 
update_logfile(char *datastring)
/* writes datastring to the temporary log file.  This is updated in the */
/* real log file when necessary in the save_database procedure */
{
  if (templogfileptr != NULL)
    if (!fputs(datastring, templogfileptr))
      errormsg_popup(get_appmsg("apperror", "logcantwrite"));
}

int 
save_database()
/*  This procedure puts a wrapper around the save_changes procedure and checks */
/*  to verify that changes have been made, and that no one else has modified */
/*  the database since the user first loaded xmdbm.  It is the user's option */
/*  whether to save the changes anyway at the point.  (When saving the */
/*  database, only local changes are saved.  That is, it does not rewrite the */
/*  entire database from any cache or store in memory.  Thus, two people *should* */
/*  be able to modify the database at one time, with no ill side affects, as */
/*  long as both are working on different words.  It is, in general however, */
/*  not a good practice, and should not be relied upon.  */
{
  struct stat newdbfileinfo;
  char verifytext[DATABUFFER];
  int returnvalue = 0;

  *verifytext = '\0';
  if (readonly)
    errormsg_popup(get_appmsg("apperror", "readonly"));
  if (changedflag) 
    {
      stateflag = SAVECHANGES;
      stat(dbnameptr, &newdbfileinfo);
      if (newdbfileinfo.st_mtime != dbfileinfo.st_mtime) 
	{
	  strcat(verifytext, get_appmsg("appverify", "alreadymodified"));
	  XtVaSetValues(verifydialog, XtNlabel, verifytext, XtNwidth, strlen(verifytext)*FONTWIDTH, NULL);
	  confirm_popup();
	}
      else 
	return(save_changes());
    }
  else
    errormsg_popup(get_appmsg("apperror", "nochanges"));
}

int 
save_changes()
 /* if the changed flag is set (it is set whenever a change is successfully    */
 /*  made), then the procedure simply goes through the temporary database      */
 /*  sequentially, replacing the entry in the real database with that made in  */
 /*  the temporary one.  A special case is when an entry has been deleted.  In */
 /*  that case, the content.data will be the empty string, and a call to       */
 /*  C_db_del_encoded must be made to delete the key in the real database.  If */
 /*  a problem is encountered while replacing keys in the real database, the   */
 /*  loop quits out and returns the error from the db put routine.             */
 /*  After successfully saving the database, a line by line copy of the log is */
 /*  made.  Since the log is not as critical, problems there do not affect     */
 /*  whether the save is considered successful or not.  If a save is           */
 /*  successful, we reset the temporary files, and then close and immediately  */
 /*  reopen the database files.  This is necessary because of the buffering    */
 /*  that db does to changes in the database.  Otherwise, if the program       */
 /*  terminate abnormally after a save was successfult, one would not be       */
 /*  assured that the changes had actually made it into the real database.     */
{
  DBT key, content;
  char logline[DATABUFFER];
  int problem = 0;
  
  *logline = '\0';
  (tempdbfileptr->seq)(tempdbfileptr, &key, &content, R_FIRST);
  
  do {
    if (*(content.data) == '\0') { /* item has been 'deleted' */
      problem = C_db_del_encoded(dbfileptr, key.data);
      if (problem == 1) /* item wasn't in database, so can't delete it. */
	problem = 0;  /* Happens if person adds and then deletes same key */
    }
    else
      problem = (dbfileptr->put)(dbfileptr, &key, &content, R_PUT);
  } while ((!(tempdbfileptr->seq)(tempdbfileptr, &key, &content, R_NEXT)) &&
	   (!problem));
  
  if (!problem) 
    {
      if (!nolog)
	{
	  fclose(templogfileptr);
	  templogfileptr = NULL;
	  templogfileptr = fopen(templognameptr, "r");
	  if ((templogfileptr != NULL) && (logfileptr != NULL))
	    while (fgets(logline, DATABUFFER, templogfileptr) != NULL)
	      fputs(logline, logfileptr);
	}
      makeflat = TRUE;
      changedflag = FALSE;
      stateflag = NOFLAG;
      strcpy(logline, get_appmsg("apphistory", "dbsaved"));
      update_historystring(logline);
      reset_tempfiles(table);
      close_dbfile();
      open_dbfile(dbnameptr);
    }
  return(problem);
}

int db_zeroout_content(DB *dbp, char *keystring)
/* Returns -1 if error,
	   0  if success */
/*  This procedure creates a database entry with a nothing in the content */
 /*  portion.  This is necessary to distinguish in the temporary database */
 /*  between a key that has been deleted, and one which is simply not in the */
 /*  temproary database (in which case it woud be pulled in from the permanent */
 /*  database) */
{
  DBT key, content;
  
  *(content.data) = '\0';
  content.size = 1;
  key.data = keystring;
  key.size = strlen(key.data) + 1;
  return((dbp->put)(dbp, &key, &content, R_PUT));
}

void reset_tempfiles(HASHINFO table)
/*  This procedure effectively deletes any existing temporary database or log */
 /*  file, and creates a new, empty one.  If the program was invoked with the */
 /*  -nolog flag, then the log file is not opened.  */
{
  time_t tp;
  char loginfo[DATABUFFER];
  
  *loginfo = '\0';
  
  if (*templognameptr == '\0') 
    fprintf(stderr, "cannot create a uniq tempfile for the database\n");
  /* open temp db file */
  unlink(tempdbnameptr);
  if (!(tempdbfileptr = hash_open(tempdbnameptr, O_CREAT|O_RDWR, 0660, NULL)))
      {
	fprintf( stderr, "cannot open temporary file %s\n", tempdbnameptr);
	exit(1);
      }

/* Open temp log file */
  if (!nolog)
    if (!(templogfileptr = fopen(templognameptr, "w"))) 
      {
	fprintf( stderr, "cannot open temporary file %s\n", templognameptr);
	exit(1);
      }
    else 
      {
	tp = time(0);
	sprintf(loginfo, get_appmsg("applog", "userinfo"), getenv("USER"), ctime(&tp));
	update_logfile(loginfo);
      }
}

void 
close_tempfiles()
/* Closing the temp files is necessary to retain any changes made to them. */
/* This is necessary because db buffers output to the file. This means that */
/* if the program ends abmormally, it will probably not be possible to retrieve any */
/* changes made.

/* Change on 8/10/93:  Program now deleted the temp file when exiting.  It will */
/* no longer be possible to retrieve changes made if one forgets to save them. */
/* This is due to a change that adds the process name to the temp name to give */
/* a uniq name for each temp file - necessary when more than one person is */
/* running xmdbm on the same machine.  */

{
  
  if (templogfileptr != NULL)
    fclose(templogfileptr);
  if (tempdbfileptr != NULL)
      (tempdbfileptr->close)(tempdbfileptr);

  unlink(tempdbnameptr);
  unlink(templognameptr);
  
  templogfileptr = NULL;
  tempdbfileptr = NULL;
}

void 
spinoff_flat()
 /*  Create a background process that creates a .flat file.  The name of the */
 /*  .flat file is .flat appended to the database name.  The global variable */
 /*  makeflat is set true upon the successful save of a database.  This */
 /*  procedure is invoked whenever a change of database is being made, or when 
 /*  quitting the program.  It is not invoked whenever a save is done, because */
 /*  the operation takes approx. 1.5 min for a 10M file.  */
{
  char commandstring[DATABUFFER];

  *commandstring = '\0';
  if (makeflat)
    {
      fprintf(stderr, "Spawning background process: creating .flat file...\n");
      strcat(commandstring, PRINTCOMMAND);
      strcat(commandstring, " ");
      strcat(commandstring, dbnameptr);
      strcat(commandstring, " > ");
      strcat(commandstring, dbnameptr);
      strcat(commandstring, ".flat&");
      system(commandstring);
      makeflat = FALSE;
    }
}

void 
quit()
/* Close everything up and leave */
{
  spinoff_flat();    /* create a .flat file if necessary */
  close_dbfile();
  close_logfile();
  close_tempfiles();
  XtDestroyApplicationContext(XtWidgetToApplicationContext(topwindow));
  exit(0);
}

void 
dialog_popup(Widget dialogshell, char *nameptr)
/*  Used to pop up both the dbdialog shell and the logdialog shell.  The */
/*  nameptr is passed in only to set the width of the shell.  The value of the dialog has
/*  already been set.  */
{
  Position x,y;
  Dimension width;
  
  width = MAX((int)strlen(nameptr)*FONTWIDTH, DIALOG_MINSIZE);
  XtVaGetValues(topwindow, XtNx, &x, XtNy, &y, NULL);
  XtVaSetValues(dialogshell, XtNx, x+DIALOG_XOFFSET, XtNy, y+DIALOG_YOFFSET,
		XtNwidth, width, NULL);
  XtPopup(dialogshell, XtGrabNonexclusive);
}

static void 
dialog_popdown(
	       Widget w,
	       XEvent *event,
	       String *params,
	       int num_params
	       )
/* Popdown the shell that asks for the db file name */
{
  XtPopdown(dbdialogshell);
}

void 
errormsg_popup(char *errormsgtext)
/* Determine and set the x,y coordinates for the error message, as well as the */
/* width needed to display the given text.  Popup the shell.  */
{
  Position x,y;
  Dimension width;
  
  width = MAX((int)strlen(errormsgtext)*FONTWIDTH, ERRORMSG_MINSIZE);
  XtVaGetValues(topwindow, XtNx, &x, XtNy, &y, NULL);
  XtVaSetValues(errormsgshell, XtNx, x+ERRORMSG_XOFFSET, XtNy, y+ERRORMSG_YOFFSET,
		XtNwidth, width, NULL);
  XtVaSetValues(errormsgdialog, XtNlabel, errormsgtext, NULL);
  XtPopup(errormsgshell, XtGrabNonexclusive);
}

static void 
errormsg_popdown(
		 Widget w,
		 XEvent *event,
		 String *params,
		 int num_params
		 )
/* Popdown the errormsg shell */
{
  XtPopdown(errormsgshell);
}

void 
confirm_popup()
/*  Determine and set the x,y corrdinates to pop up the verify shell and then */
/*  do so */
{
  Position x,y;
  
  XtVaGetValues(topwindow, XtNx, &x, XtNy, &y, NULL);
  XtVaSetValues(verifyshell, XtNx, x+CONFIRM_XOFFSET, XtNy, y+CONFIRM_YOFFSET, NULL);
  XtPopup(verifyshell, XtGrabNonexclusive);
}

static void 
confirm_popdown()
/* Popdown the verify shell */
{
  XtPopdown(verifyshell);
}

void 
set_commandsensitivity(char *newvalue)
/*  Grey out or restore the top command buttons.  Newvalue takes the character */
/*  strings 'True' or 'False' */
{
  XtSetSensitive(filebutton, newvalue);
  XtSetSensitive(lookupbutton, newvalue);
  if (!readonly)
    {
      XtSetSensitive(modifybutton, newvalue);
      XtSetSensitive(addbutton, newvalue);
      XtSetSensitive(deletebutton, newvalue);
    }
}

void 
cleartext(XiSimpleTextWidget tw)
/* Clear the text from a SimpleTextWidget widget */
{
  XiSimpleTextSetString(tw, "\0");
}

char *
get_appmsg(char *msgname, char *msgtype)
{
  static char msgbuffer[DATABUFFER];
  char defaultmsg[DATABUFFER];
  
  *msgbuffer = '\0';
  *defaultmsg = '\0';
  sprintf(defaultmsg, "Installation Error: cannot find msg %s.%s\n", msgname, msgtype);
  XtAppGetErrorDatabaseText(app_con, msgname, msgtype, "AppText", defaultmsg,
			    msgbuffer, DATABUFFER, NULL);
  return(msgbuffer);
}



static void 
beep(
     Widget w,
     XEvent *event,
     String *params,
     int num_params
     )
{
  XBell(XtDisplay(w), 50);
}

char *
stripspaces(char *instring)
/* Strip tailing spaces from a given string */
{
  int current;

  current = strlen(instring) -1;
  while ((current >=0) && (*(instring+current) == ' '))
    {
      *(instring+current) = '\0';
      --current;
    }
  return(instring);
}


/* CHANGES LOG 

Changes in version posted 9/14/92:
  - Return chooses a default action in pop-up windows.  The default action is
      highlighted.
  - In modify, if there is only 1 entry, that is chosen automatically.
  - xmdbm now checks the modification data for the database, and warns the user
      when they try to save if the database has beren changed since they started
      using it.
  - The log file is created with g+rw permission.
  - NVC (Noun-Verb Contractions), and Part (Verb Particles) were added as new
      parts of speech.
  - Nouns and PropN were changed to explicitly 3sg or 3pl
  - Bug that allowed the old subparts window to remain showing when the new
      part-of-speech choosen didn't have any subparts.

Changes in version installed 10/20/92:
  - Made sure that *all* strings are properly initialized.  This is in response
    to the seg fault that we get in delentry on the HP.
  - For adding a key - changed the order of the string printed in the history
      so that the inflected form was first - and therefore more noticeable.
  - Started to add an fopen_with_perms so that the log file permissions were
      correct (writeable for group).  Turns out that the docu for umask is wrong,
      and that a umask of 000 will give the permissions that we want.

Changed in version installed 12/2/92:
  - Added 'V TO' as an inflection in coding.h and also in xmdbm-base.h  This
      necessitated the moving of 5 blank spaces from after the pronouns to after
      the verbs to accomodate the new inflection.


Changed in version installed 2/9/93:
  - 2 calls to reset_templates did not send the variable 'table'.  This caused 
      it to crash on HP workstations.
  - Changed the inflectional tags for NVC to reflect the current verb/aux
      system.  The number of NVC tags was changed from 10 to 15, which shifted the
      Nouns down by 5.  The old inflections were: {NVC INF, NVC 1sg PRES, NVC
      2sg PRES, NVC 3sg PRES, NVC PRES pl, NVC 1sg PAST STR, NVC 2sg PAST STR, 
      NVC 3sg PAST STR, NVC PAST STR, NVC PAST STR pl}.  The new inflections
      are: {NVC 1sg PRES, NVC 1pl PRES, NVC 2nd PRES, NVC 3sg PRES, NVC 3pl PRES,
      NVC 3sg PRES fem, NVC 3sg PRES masc, NVC 3sg PRES neut, NVC 3sg PRES wh,
      NVC 3pl PRES wh, NVC 1sg INF, NVC 1pl INF, NVC 2nd INF, NVC 3pl INF, 
      NVC 3sg PAST STR neut}

  - Added a version number in morph.h, which was incorporated into the title of
      xmdbm.  The version number will show up in the title at all times.  The
      version number will also be printed out at the top of every flat file,
      but no attempt will be made to automatically verify that the version
      numbers are the same.  This decision was made to simplify the process of
      changing over from one version of xmdbm to another.  The version number
      is *not* encoded in the binary.

Changed in working version:
   - Changed 3 inflectional information tags in coding.h and in xmdbm-base.h:
       Pron wh became Pron 3sg wh
       Pron acc wh became Pron 3sg acc wh
       Pron nom wh became Pron 3sg nom wh

   - Added a NONE option to possible Det inflectional tags

   - The nvcontraction list was called verbparticle for some reason.  Changed
   the name, and updated the list to reflect the actual tags now used in
   morph.h for the N/V Contractions.  The list had not been updated in the
   xmdbm-base.h file when the change was made 2/9/1993.

   (7/23/93)
   - Added a 'Pron GEN' option to handle genitive's of non-reflexive pronouns,
   such as 'everyone'.

   - Added a 'Det ref3sg' for 'its', since the other ref3sg options were marked
   for either 'reffem' or 'refmasc'.

   - Modified the decode function in coding.h to test for the hashmark *after*
   the first 2 characters.  The first two characters contain the codes for the
   inflectional array and the number of characters the root and inflected words
   have in common.  Either could be character 35('#') without indicating that
   there is more than entry for that inflected form.  This did not show up
   earlier, because the infl tag in array slot 35 was a NULL.  With the shift 
   in numbers due to adding 5 spaces in the Determiner part of the array, the 
   new tag in slot 35 was 'N 3sg'.

   (8/18/93)
   - Changed to temp file code so that it uses the process number to assign a
   unique file name for the temporary files.  This eliminates
   conflict problems with the temporary files when 2 or more people
   are using the program on the same machine.

   (8/23/93)
   - Fixed some problems created in the 8/18/93 version.  Moved the creation of
   the temporary file names to the beginning of the program, and made them
   global variables.  Also fixed some problems from 7/23/93 with the N 3sg
   thing.  I think that I got all the searches for the SEP_CHAR skipping over
   the first 2 characters of the encoded string.

   (3/6/95) 
   - Added in code so that Adverbs can be wh (`how' is the only one).
   Previously they had no inflectional tags.  

   Tue Jun  1 23:34:43 1999
   - changed from K&R to ANSI
*/
