                    /****** SimpleText.c ****/ 
/***               simple text entry box implementation                 ***/
/* Code copied from Appendix U of "Object-Oriented Programming with the 
 * X Window System Toolkits" by Jerry D. Smith.  That copyright follows this message.
 * Code was modifed to solve problems with the translation table, and with
 * the overlapping of the label and text widgets whenever the widget size
 * was modified. 
 * 9/14/92  by Dania Egedi:  Initialized modified to shift keyboard focus from the top
 *          widget to the AsciiString Widget
*/
/*************************************************************************
 * Copyright (c) 1990 Iris Computing Laboratories.
 *
 *  This software is provided for demostration purposes only.  As
 *  freely-distributed, modifiable source code, this software carries
 *  absolutely no warranty.
 *************************************************************************/


#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#ifdef X11R3
#include <X11/AsciiText.h>
#include <X11/Form.h>
#include <X11/Label.h>
#else
#include <X11/Xaw/XawInit.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#endif

#include "mySimpleTexP.h"

static XtResource resources[] = 
{
  {XiNlabel, XiCLabel, XtRString, sizeof(char *), 
      XtOffset(XiSimpleTextWidget, simple_text.label),
      XtRString, TEXT_DEFAULT_LABEL },
  {XiNtext, XiCText, XtRString, sizeof(char *),
     XtOffset(XiSimpleTextWidget, simple_text.default_text),
      XtRString, TEXT_DEFAULT_TEXT },
  {XiNtextWidth, XiCTextWidth, XtRDimension, sizeof(Dimension),
     XtOffset(XiSimpleTextWidget, simple_text.text_width),
      XtRImmediate, (caddr_t)TEXT_DEFAULT_TEXT_WIDTH },
  {XiNfont, XiCFont, XtRFontStruct, sizeof(XFontStruct *),
     XtOffset(XiSimpleTextWidget, simple_text.font),
      XtRString, "XtDefaultFont" },
};

    
/* Class Methods: */
static void Initialize();
static void ConstraintInitialize();
static Boolean SetValues();
static void Destroy();


/* Action functions */
static void Beep();


/* Define storage for the class here: */

XiSimpleTextClassRec XisimpleTextClassRec = 
{
  { /* core_class variables */
    (WidgetClass) &formClassRec,  /* ancestor */
    "SimpleText",                 /* class name */
    sizeof(XiSimpleTextRec),      /* widget size */
#ifdef X11R3
    NULL,                         /* class initialize */
#else
    XawInitializeWidgetSet,       /* class initialize */
#endif
    NULL,                         /* class part init */
    FALSE,                        /* class inited */
    Initialize,                   /* initialize */
    NULL,                         /* initialize hook */
    XtInheritRealize,             /* realize */
    NULL,                         /* actions */
    0,                            /* number of actions */
    resources,                    /* resources */
    XtNumber(resources),          /* number of resources */
    NULLQUARK,                    /* xrm class */
    TRUE,                         /* compress motions */
    TRUE,                         /* compress exposures */
    TRUE,                         /* compress enter/leave */
    FALSE,                        /* visibility interest */
    Destroy,                      /* destroy */
    XtInheritResize,              /* resize */
    XtInheritExpose,              /* expose */
    SetValues,                    /* set values */
    NULL,                         /* set values hook */
    XtInheritSetValuesAlmost,     /* set values almost */
    NULL,                         /* get values hook */
    NULL,                         /* accept focus */
    XtVersion,                    /* version */
    NULL,                         /* callback private */
    NULL,                         /* translation table */
    XtInheritQueryGeometry,      /* query geometry */
    XtInheritDisplayAccelerator,  /* display accelerator */
    NULL,                         /* extension */
  },
  { /* composite_class variables */
    XtInheritGeometryManager,     /* geometry manager */
    XtInheritChangeManaged,       /* change managed */
    XtInheritInsertChild,         /* insert child */
    XtInheritDeleteChild,          /* delate child */
    NULL,                         /* extension */
  },
  { /* constraint_class fields */
    NULL,                         /* subresources */
    0,                            /* number of subresources */
    sizeof(XiSimpleTextConstraintsRec), /* record size */
    ConstraintInitialize,         /* initialize */
    NULL,                         /* destroy */
    NULL,                         /* set values */
    NULL,                         /* extension */
  },
  { /* form_class fields */
#ifdef X11R3
    0,
#else
    XtInheritLayout,
#endif
  },
  { /* simple_text_class variables */
    0,
  },
};  /* XisimpleTextClassRec */

WidgetClass xiSimpleTextWidgetClass = (WidgetClass) &XisimpleTextClassRec;
     

/* XiSimpleTextWidget methods: */

/* Initialize() creates the label and text widget for the text
entry box; it has a zero-width border.  Other variables are
initialized as well. (/
/*ARGSUSED*/
static void Initialize(request, new)
     XiSimpleTextWidget request;
     XiSimpleTextWidget new;
{
  static char text_translations[] =
    "#override\n\
    Ctrl<Key>J:  beep()\n\
    Ctrl<Key>M:  beep()\n\
    Ctrl<Key>O:  beep()\n\
    <Key>Linefeed: beep()\n";
  static XtActionsRec text_actions[] = 
    {
      {"beep", (XtActionProc) Beep}
    };
  
  XtTranslations text_trans_table;
  
  Arg args[10];
  int i;
  
/* add/create actions/translations: */
  XtAddActions(text_actions, XtNumber(text_actions));
  text_trans_table = XtParseTranslationTable(text_translations);
  
  if (new->core.width == 0)
    new->core.width = TEXT_DEFAULT_WIDTH;
  if(new->core.height == 0)
    new->core.height = TEXT_DEFAULT_HEIGHT;

/* create the text entry label: */
  new->simple_text.labelW = NULL;
  if (*new->simple_text.label)
    {
      i = 0;
      XtSetArg(args[i], XtNlabel, (XtArgVal) new->simple_text.label);  i++;
      XtSetArg(args[i], XtNborderWidth, (XtArgVal) 0);  i++;
      XtSetArg(args[i], XtNfont, (XtArgVal) new->simple_text.font);  i++;
      new->simple_text.labelW = XtCreateManagedWidget("label", labelWidgetClass, new,
						      args, i);
    }

/* create the text entry area: */
  i = 0;
#ifdef X11R3
  new->simple_text.text = XtMalloc(TEXT_MAX_TEXT_LEN + 1);
  strncpy(new->simple_text.text, new->simple_text.default_text,
	  TEXT_MAX_TEXT_LEN);
  if (strlen(new->simple_text.default_text) >= TEXT_MAX_TEXT_LEN)
    new->simple_text.text[TEXT_MAX_TEXT_LEN] = '\0';
  XtSetArg(args[i], XtNeditType, (XtArgVal) XttextEdit); i++;
#else
  new->simple_text.text = new->simple_text.default_text;
  XtSetArg(args[i], XtNeditType, (XtArgVal) XawtextEdit); i++;
#endif  
  XtSetArg(args[i], XtNstring, (XtArgVal) new->simple_text.text);  i++;
  
  /* Philosophy?  This will override resource database requests *: */ 
  
/*XtSetArg(args[i], XtNborderWidth, (XtArgVal) 1); i++ */
  XtSetArg(args[i], XtNlength, (XtArgVal) TEXT_MAX_TEXT_LEN);  i++;
  XtSetArg(args[i], XtNwidth, (XtArgVal) new->simple_text.text_width);  i++;
  XtSetArg(args[i], XtNfont, (XtArgVal) new->simple_text.font); i++;
  new-> simple_text.textW = XtCreateManagedWidget("text",
#ifdef X11R3
						  asciiStringWidgetClass,
#else 
						  asciiTextWidgetClass,
#endif
						  new, args, i);
  XtOverrideTranslations(new->simple_text.textW, text_trans_table);
/* Added keyboard focus:  DME          9/9/92   */
  XtSetKeyboardFocus(new, new->simple_text.textW);
}  /* Initialize */




/*  ConstraintInitialize() sets up the from widget for the label, if it exists, */
/*  and the text entry field.  */
/*ARGSUSED*/
static void ConstraintInitialize(request, new)
     Widget request;
     Widget new;
{
  XiSimpleTextWidget stw = (XiSimpleTextWidget) new->core.parent;
  XiSimpleTextConstraints constraints = (XiSimpleTextConstraints)
    new->core.constraints;
  
  if (stw->simple_text.labelW == NULL) return;
#ifdef X11R3
  if (!XtIsSubclass(new, asciiStringWidgetClass)) return;
#else
  if (!XtIsSubclass(new, asciiTextWidgetClass)) return;
#endif  
  constraints->form.left = constraints->form.right = XtChainLeft;
  constraints->form.horiz_base = stw->simple_text.labelW;
} /* ConstraintInitialize */



/* SetValues() updates resources-related widget values */
/*ARGSUSED*/
static Boolean SetValues(current, Request, new)
     Widget current;
     Widget Request;
     Widget new;
{
  /* at present, nothing to update */
  return FALSE;
}  /* SetValues */



/* Destroy() frees dynamic data structures */
static void Destroy(stw)
     XiSimpleTextWidget stw;
{
  XtDestroyWidget(stw->simple_text.labelW);
  XtDestroyWidget(stw->simple_text.textW);
}  /* Destroy */


/* Public functions: */

char *XiSimpleTextGetString(stw)
     XiSimpleTextWidget stw;
{
#ifdef X11R3
  return stw->simple_text.text;
#else
  Arg args[1];
  char *text;
  
  XtSetArg(args[0], XtNstring, (XtArgVal) &text);
  XtGetValues(stw->simple_text.textW, args,1);
  return(text);
#endif
} /* XiSimpleTextGetString */

  
void XiSimpleTextSetString(stw, str)
     XiSimpleTextWidget stw;
     char *str;
{
  Arg args[1];
  int str_len = strlen(str);
  
#ifdef X11R3
  strncpy(stw->simple_text.text, str, TEXT_MAX_TEXT_LEN);
  if (str_len >= TEXT_MAX_TEXT_LEN) 
    {
      str_len = TEXT_MAX_TEXT_LEN;
      stw->simple_text.text[str_len] = '\0';
    }      
  XtTextSetInsertionPoint(stw->simple_text.textW, 0);
  XtTextSetLastPos(stw->simple_text.textW, strlen(str));
  XtTextDisplay(stw->simple_text.textW);
#else
  XtSetArg(args[0], XtNstring, (XtArgVal) str);
  XtSetValues(stw->simple_text.textW, args,1); 
#endif 
} /* XiSimpleTextSetString */


/*  Beep() is a general-purpose functions; one use is to beep the
user if they try to enter a character that results in a line
feed operation; see Initialize()  */
/*ARGSUSED*/
static void Beep(w, event)
     Widget w;
     XEvent *event;
{
  XBell(XtDisplay(w), 50);
}  /* Beep */

  
  
      
      

