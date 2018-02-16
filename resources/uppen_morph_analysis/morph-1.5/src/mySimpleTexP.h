/*****  SimpleTexP.h ****/
/*************************************************************************
 * Copyright (c) 1990 Iris Computing Laboratories.
 *
 *  This software is provided for demostration purposes only.  As
 *  freely-distributed, modifiable source code, this software carries
 *  absolutely no warranty.
 *************************************************************************/
/***** private declarations/definitions for SimpleText.c ****/

#ifndef _XiSimpleTextP_h
#define _XiSimpleTextP_h 1

#include "mySimpleText.h"
#ifdef X11R3
#include <X11/FormP.h>
#else
#include <X11/Xaw/FormP.h>
#endif


#define TEXT_DEFAULT_WIDTH      400
#define TEXT_DEFAULT_HEIGHT     175
#define TEXT_DEFAULT_TEXT_WIDTH 300
#define TEXT_DEFAULT_LABEL      ""
#define TEXT_DEFAULT_TEXT       ""
#define TEXT_MAX_TEXT_LEN       256


/* class definition */
typedef struct _XiSimpleTextClassPart {    
  int dummy;
} XiSimpleTextClassPart;


typedef struct _XiSimpleTextClassRec {
  CoreClassPart core_class;
  CompositeClassPart composite_class;
  ConstraintClassPart constraint_class;
  FormClassPart form_class;
  XiSimpleTextClassPart simple_text_class;
} XiSimpleTextClassRec;

extern XiSimpleTextClassRec xiSimpleTextClassRec;

/* instance definition */
typedef struct XiSimpleTextPart {
  char *label;
  char *text;
  char *default_text;
  Dimension text_width;
  XFontStruct *font;
  Widget labelW;
  Widget textW;
} XiSimpleTextPart;

typedef struct _XiSimpleTextRec {
  CorePart core;
  CompositePart composite;
  ConstraintPart constraint;
  FormPart form;
  XiSimpleTextPart simple_text;
} XiSimpleTextRec;

typedef struct {
  int dummy;
} XiSimpleTextConstraintsPart;

typedef struct _XiSimpleTextConstraintsRec {
  FormConstraintsPart form;
  XiSimpleTextConstraintsPart simple_text;
} XiSimpleTextConstraintsRec, *XiSimpleTextConstraints;


#endif /* XiSimpleTextP_h */





