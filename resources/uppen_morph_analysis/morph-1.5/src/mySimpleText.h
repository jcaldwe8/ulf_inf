/***** SimpleText.h *****/

/***** public declarations/definitions for SimpleText.c ****/

#ifndef _XiSimpleText_h
#define _XiSimpleText_h 1

/************************************************************
*  Copyright (c) 1990 Iris Computing Laboratories.
*
*  This software is provided for demonstration purposes only.  As
*  free-distributed, modifiable source code, this software carries 
*  absolutely no warranty.
*************************************************************/

/************************************************************
A SimpleText widget implements a one-line text entry widget, based
on the Athena text widget.  Unlike the Athena dialog widget, it does
not support buttons.

A convenience functions, XiSimpleTextGetString(), can be used to
retrieve the current text string.  Likewise, XiSimpleTextSetString()
can be used to replace the current text string.

Resources:

Name             Class           Data Type        Default    Modify?
----             ----            ---------        -------    -------
XiNlabel         XiCLabel        String           ""
XiNtext          XiCText         String           ""
XiNtextWidth     XiCTextWidth    int              200
XiNfont          XiCFont         XFontStruct *    XtDefaultFont

Public interfaces are described below.
***********************************************************************/

#ifdef X11R3
#include <X11/Form.h>
#else
#include <X11/Xaw/Form.h>
#endif

/* Resource definitions */

#define XiNlabel          "label"
#define XiNtext           "text"
#define XiNtextWidth      "textWidth"
#define XiNfont           "font"
#define XiCLabel          "Label"
#define XiCText           "Text"
#define XiCTextWidth      "TextWidth"
#define XiCFont           "Font"


typedef struct _XiSimpleTextClassRec *XiSimpleTextWidgetClass;
typedef struct _XiSimpleTextRec *XiSimpleTextWidget;

extern WidgetClass xiSimpleTextWidgetClass;


/* Public functions: */

extern char *XiSimpleTextGetString();
/* XiSimpleTextwidget simple_text_widget */

extern void XiSimpleTextSetString();
/* XiSimpleTextwidget simple_text_widget;
   char *string */

#endif /* XiSimpleText_h */
