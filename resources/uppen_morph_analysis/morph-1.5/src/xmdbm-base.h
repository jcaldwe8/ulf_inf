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


#define DIALOG_XOFFSET 50
#define DIALOG_YOFFSET 50
#define DIALOG_MINSIZE 150
#define ERRORMSG_XOFFSET 50
#define ERRORMSG_YOFFSET 50
#define ERRORMSG_MINSIZE 150
#define CONFIRM_XOFFSET 150
#define CONFIRM_YOFFSET -30
#define SPEECH_XOFFSET 450
#define SPEECH_YOFFSET 60

#define FONTWIDTH 8
#define TEMPDBNAME "/tmp/.xtagtemp.db"
#define TEMPLOGNAME "/tmp/.xtagtemp.log"

/* The following 2 #defines are given in the Makefile */
/*#define PRINTCOMMAND morph_printdb */
/*#define MSGPATH "XAPPL/ */

#define NOFLAG      0
#define MODIFY      1
#define LOOKUP      2
#define ADD         3
#define DELETEKEY   4
#define DELETEENTRY 5
#define DBFILE      6
#define SAVE        7
#define CREATE      8
#define SAVECHANGES 9
#define QUIT        10

#define streq(a, b)        ( strcmp((a), (b)) == 0 )
#define MAX(a,b)  (a>b?a:b)

static void FileMenuSelect(), DeleteMenuSelect();

static void dialog_popdown(), errormsg_popdown(), confirm_popdown();
static void get_newdbname(), get_newlogname();
static void beep(), have_key(), have_root();

int open_dbfile(), open_logfile(), create_dbfile();
int close_dbfile(), close_logfile();
void reset_tempfiles(), close_tempfiles();
void switch_dbfiles(), create_newfiles(), update_logfile();
void quit(), spinoff_flat(), syntax();

int save_database(), bringinto_tempdb(), getinfo_fromdb();

void lookup_pressed(), modify_pressed(), add_pressed();
void deletekey_pressed(),  deleteentry_pressed();
void clear_pressed(), done_pressed();
void ok_pressed(), cancel_pressed();

void lookup_key(), lookupkey_cancelled();
void verify_delkey(), choose_entry();
void format_decodedstring(), update_historystring(), dialog_popup(),
  errormsg_popup(), confirm_popup();

void delkey_confirmed(), delkey_cancelled();
void delentry_verify(), delentry_confirmed(), delentry_cancelled();
int  add_the_key();
void addkey_cancelled(), addkey_insert();
void createentry_confirmed(), createentry_cancelled();
void modify_chosen(), modify_replace(), build_modifyroot(), modify_cancelled();
void putup_subparts(), verify_inflinfo();

void init_listarray(), array_to_decoded(), decoded_to_array();
int entries_into_listarray();
void cleartext(),  set_commandsensitivity();
int delete_the_key(); 
void delete_the_entry();
void printusage_and_exit();
char *stripspaces(), *get_appmsg();

XtActionsRec actionTable[] = {
  {"dialog_popdown", dialog_popdown},
  {"errormsg_popdown", errormsg_popdown},
  {"ok_pressed", ok_pressed},
  {"get_newdbname", get_newdbname},
  {"get_newlogname", get_newlogname},
  {"have_key", have_key},
  {"have_root", have_root},
  {"beep", beep}
};


String fallback_resources[] = { 
  "*allowShellResize:  True",
  "*right:          chainLeft",
  "*left:           chainLeft",
  "*top:            chainTop",
  "*bottom:         chainTop",
  "*dummybutton.mappedWhenManaged: False",
  "*clearbutton.mappedWhenManaged: False",
  "*donebutton.mappedWhenManaged: False",

  "*modelabel.mappedWhenManaged:  False",
  "*modelabel*right:        Rubber",

  "*blank.right:          Rubber",

  "*historytext*right:        Rubber",
  "*historytext*bottom:         Rubber",

  "*errormsgdialog.label.resizeable: True",
  "*errormsgdialog*translations: #override \\n <Key>Return: errormsg_popdown()",

  "*keydialog*right:          chainLeft",
  "*keydialog*left:           chainLeft",
  "*keydialog.mappedWhenManaged:  False",
  "*keydialog*translations: #override \\n <Key>Return: have_key()",
  "*rootdialog.mappedWhenManaged:  False",
  "*rootdialog*right:          chainLeft",
  "*rootdialog*left:           chainLeft",
  "*rootdialog*translations: #override \\n <Key>Return: have_root()",

  "*verifydialog.label.resizeable: True",
  "*verifydialog*translations: #override \\n <Key>Return: ok_pressed()",

  "*delentrychoiceslabel.mappedWhenManaged:  False",
  "*delentrychoices.mappedWhenManaged:  False",

  "*addkeyentrieslabel.mappedWhenManaged:  False",
  "*addkeyentries.mappedWhenManaged:  False",

  "*modifychoiceslabel.mappedWhenManaged:  False",
  "*modifychoiceslabel.borderWidth:  0",
  "*modifychoices.mappedWhenManaged: False",

  "*speechparts.mappedWhenManaged: False",

  "*dbfiledialog*value.translations: #override \\n <Key>Return: get_newdbname(dbname)",

  "*logfiledialog*value.translations: #override \\n <Key>Return: get_newlogname(logname)",
  NULL,
};


static char *speechparts_list[] = 
{
  "Noun", "Proper Noun", "Verb", "Verb Particle", "Pronoun", 
  "N/V Contraction", "V/V Contraction", "Adjective", "Preposition","Punctuation", 
  "Complementizer", "Adverb", "Determiner", "Conjunction", "Interjection", "Genitive", NULL
  };

static char *noun_speechparts[] = 
{
  "*STD INFLECTIONS*", "3pl", "3pl GEN", "3pl masc", "3pl GEN masc", 
  "3sg", "3sg GEN", "3sg masc", "3sg GEN masc", "NONE", NULL
  };

static char *propnoun_speechparts[] = 
{
  "*STD INFLECTIONS*", "3sg", "3sg GEN", "3pl", "3pl GEN", NULL
};

static char *verb_speechparts[] = 
{
"*STD INFLECTIONS*", "1sg PAST STR", "1sg PRES", "2sg PAST STR", "2sg PRES", "3pl PRES",
"3sg PAST STR", "3sg PRES", "INF", "INF NEG", "NEG PRES",
"PAST", "PAST PPART", "PAST STR", "PAST WK", "PPART",
"PASSIVE PPART STR", "PPART STR", "PPART WK", "PPART STR to", "PRES", "INDAUX", 
"PROG", "PROG to", "PAST STR pl", "PRES pl", "NEG PRES", 
"NEG PRES pl", "2sg NEG PRES", "3sg NEG PRES", "NEG PAST STR","NEG PAST STR pl",
"1sg NEG PAST STR", "2sg NEG PAST STR", "3sg NEG PAST STR", "NEG PPART STR", 
"CONTR INF NEG", "CONTR NEG PRES", "CONTR NEG PRES pl", "2sg CONTR NEG PRES", 
"3sg CONTR NEG PRES", "CONTR NEG PAST STR", "CONTR NEG PPART STR",
"1sg CONTR NEG PAST STR", "2sg CONTR NEG PAST STR", "3sg CONTR NEG PAST STR", 
"CONTR NEG PAST STR pl", "TO", "IND", NULL
};

static char *nvcontraction_speechparts[] =
{
"1sg PRES", "1sg PAST", "1sg INF", "1pl PRES", "1pl PAST", "1pl INF", "2nd PRES", "2nd PAST", "2nd INF", "3sg PRES", "3sg PRES fem", "3sg PRES masc", "3sg PRES neut", "3sg PRES wh", "3sg PAST STR neut", "3sg PAST", "3sg PAST fem", "3sg PAST masc", "3sg PAST wh", "3pl PRES",  "3pl PRES wh", "3pl PAST", "3pl INF", NULL 
};

static char *vvcontraction_speechparts[] =
{
"PRES INF", "PAST INF", NULL
};

static char *pronoun_speechparts[] =
{
"1sg nom", "3pl", "3rd NEG nomacc", "3sg nomacc",
"3sg NEG nomacc", "3sg GEN nomacc", "GEN ref1sg", "GEN ref2nd", "GEN ref1pl",
"GEN ref2sg", "GEN ref3pl", "GEN ref3sg reffem", "GEN ref3sg refmasc", 
"GEN ref3sg", "3sg GEN NEG nomacc", "1pl acc", "1sg acc", "2sg acc", 
"3pl acc", "3sg acc fem", "3sg acc masc", "3sg neut nomacc", "1pl nom", 
"2sg nom", "3pl nom", "3sg fem nom", "3sg masc nom", "2nd nomacc", "2pl nomacc", 
"1pl refl", "1sg refl", "2pl refl", "2sg refl", "3pl refl", "3sg refl", "3sg fem refl", 
"3sg masc refl", "3sg neut refl", "3sg wh", "3sg acc wh", "3sg nom wh","NONE", 
NULL
};

static char *adj_speechparts[] =
{
"*STD INFLECTIONS*", "COMP", "SUPER", "NONE", NULL
};

static char *adv_speechparts[] = 
{
"wh", "NONE", NULL
};

static char *det_speechparts[] =
{
"wh", "GEN wh", "GEN ref1sg", "GEN ref2sg", "GEN ref2nd", "GEN ref1pl",
"GEN ref3pl", "GEN ref3sg reffem", "GEN ref3sg refmasc", "GEN ref3sg", "NONE", NULL
};

static char *no_speechparts[] = 
{ NULL };




