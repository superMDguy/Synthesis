/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#include "common.h"
#include "g.h"
#include "gcommon.h"
#include "mem.h"

int Gxfd;
Widget Groot;
Display *Gdisplay;
int Gpopdownflag;
int Gscreenn;
int Gdepth;
Glazyq_t Glazyq;

PIXpoint_t *Gppp;
int Gppn, Gppi;

char *Gbufp = NULL;
int Gbufn = 0, Gbufi = 0;

Gfont_t *Gfontp;
int Gfontn;

/* Xt[GS]etValues arguments */
Arg argp[MAXARGS];
int argn;

/* action and translation tables */
static XtActionsRec actiontable[] = {
    { "cwany", Gcwanyaction },
    { "lwany", Glwanyaction },
    { "tweol", Gtweolaction },
    { "qwpop", Gqwpopaction },
    { "wmdel", Gwmdelaction },
};
static char defcwanytrans[] = "\
    <BtnDown>: cwany()\n\
    <BtnUp>: cwany()\n\
    <KeyDown>: cwany()\n\
    <KeyUp>: cwany()";
static char deflwanytrans[] = "\
    <BtnDown>: lwany()\n\
    <BtnUp>: lwany()\n\
    <KeyDown>: lwany()\n\
    <KeyUp>: lwany()";
static char deftweoltrans[] = "<Key> Return: newline() tweol()";
static char defqwpoptrans[] = "<KeyDown> Return:\n<KeyUp> Return: qwpop()";
static char defwmdeltrans[] = "<Message>WM_PROTOCOLS: wmdel()\n";
XtTranslations Gtweoltable;
XtTranslations Gqwpoptable;
XtTranslations Glwanytable;
XtTranslations Gcwanytable;
XtTranslations Gwmdeltable;

Atom Gwmdelatom;

static XtAppContext appcontext;
static XFontStruct *deffont;

int Ginitgraphics (void) {
    argn = 0;
    if (!(Groot = XtAppInitialize (&appcontext, "LEFTY", NULL, 0,
            &argn, NULL, NULL, NULL, 0)))
        Gerr (POS, G_ERRINITFAILED);
    XtAppAddActions (appcontext, actiontable, XtNumber (actiontable));
    Gtweoltable = XtParseTranslationTable (deftweoltrans);
    Gqwpoptable = XtParseTranslationTable (defqwpoptrans);
    Glwanytable = XtParseTranslationTable (deflwanytrans);
    Gcwanytable = XtParseTranslationTable (defcwanytrans);
    Gwmdeltable = XtParseTranslationTable (defwmdeltrans);
    XtRegisterGrabAction (Glwanyaction, True,
            ButtonPressMask | ButtonReleaseMask, GrabModeAsync, GrabModeAsync);
    XtRegisterGrabAction (Gcwanyaction, True,
            ButtonPressMask | ButtonReleaseMask, GrabModeAsync, GrabModeAsync);
    Gdisplay = XtDisplay (Groot);
    Gscreenn = DefaultScreen (Gdisplay);
    Gdepth = DefaultDepth (Gdisplay, Gscreenn);
    deffont = XLoadQueryFont (Gdisplay, "fixed");
    Gxfd = ConnectionNumber (Gdisplay);
    Gwmdelatom = XInternAtom (Gdisplay, "WM_DELETE_WINDOW", False);
    Gpopdownflag = FALSE;
    Glazyq.flag = 0;
    Gbufp = Marrayalloc ((long) BUFINCR * BUFSIZE);
    Gbufn = BUFINCR;
    Gppp = Marrayalloc ((long) PPINCR * PPSIZE);
    Gppn = PPINCR;
    Gfontp = Marrayalloc ((long) FONTSIZE);
    Gfontn = 1;
    Gfontp[0].name = strdup ("default");
    if (!Gdefaultfont)
        Gfontp[0].font = deffont;
    else if (Gdefaultfont[0] != '\000')
        Gfontp[0].font = XLoadQueryFont (Gdisplay, Gdefaultfont);
    else
        Gfontp[0].font = NULL;
    return 0;
}

int Gtermgraphics (void) {
    int fi;

    for (fi = 0; fi < Gfontn; fi++)
        free (Gfontp[fi].name);
    Marrayfree (Gfontp), Gfontp = NULL, Gfontn = 0;
    Marrayfree (Gppp), Gppp = NULL, Gppn = 0;
    Marrayfree (Gbufp), Gbufp = NULL, Gbufn = 0;
    XtDestroyWidget (Groot);
    return 0;
}

void Gflushlazyq (void) {
    if (Glazyq.flag & LAZYMANAGE) {
        XtManageChildren (Glazyq.mws, Glazyq.mwn);
        Glazyq.flag &= ~LAZYMANAGE;
    }
    if (Glazyq.flag & LAZYREALIZE) {
        XtRealizeWidget (Glazyq.rw);
        if (Glazyq.flag & LAZYRHINTS)
            XSetWMNormalHints (Gdisplay, XtWindow (Glazyq.rw), &Glazyq.hints);
        XSetWMProtocols (Gdisplay, XtWindow (Glazyq.rw), &Gwmdelatom, 1);
        XtOverrideTranslations (Glazyq.rw, Gwmdeltable);
        Glazyq.flag &= ~LAZYRHINTS;
        Glazyq.flag &= ~LAZYREALIZE;
    }
}

void Glazyrealize (Widget w, int hintsflag, XSizeHints *hintsp) {
    if (Glazyq.flag & LAZYREALIZE) {
        XtRealizeWidget (Glazyq.rw);
        if (Glazyq.flag & LAZYRHINTS)
            XSetWMNormalHints (Gdisplay, XtWindow (Glazyq.rw), &Glazyq.hints);
        XSetWMProtocols (Gdisplay, XtWindow (Glazyq.rw), &Gwmdelatom, 1);
        XtOverrideTranslations (Glazyq.rw, Gwmdeltable);
    } else
        Glazyq.flag |= LAZYREALIZE;
    Glazyq.rw = w;
    if (hintsflag) {
        Glazyq.flag |= LAZYRHINTS;
        Glazyq.hints = *hintsp;
    } else
        Glazyq.flag &= ~LAZYRHINTS;
}

void Glazymanage (Widget w) {
    if (Glazyq.flag & LAZYMANAGE) {
        if (XtParent (Glazyq.mws[Glazyq.mwn - 1]) != XtParent (w) ||
                Glazyq.mwn >= LAZYQNUM) {
            XtManageChildren (Glazyq.mws, Glazyq.mwn);
            Glazyq.mwn = 0;
        }
    } else {
        Glazyq.flag |= LAZYMANAGE;
        Glazyq.mwn = 0;
    }
    Glazyq.mws[Glazyq.mwn++] = w;
}

int Gsync (void) {
    if (Glazyq.flag)
        Gflushlazyq ();
    XFlush (Gdisplay);
    return 0;
}

int Gresetbstate (int wi) {
    Gwidget_t *w;
    Gcw_t *cw;
    int bn;

    cw = Gwidgets[wi].u.c;
    bn = cw->bstate[0] + cw->bstate[1] + cw->bstate[2];
    cw->bstate[0] = cw->bstate[1] = cw->bstate[2] = 0;
    cw->buttonsdown -= bn;
    Gbuttonsdown -= bn;
    return 0;
}

int Gprocessevents (int waitflag, Geventmode_t mode) {
    int rtn;

    if (Glazyq.flag)
        Gflushlazyq ();
    rtn = 0;
    switch (waitflag) {
    case TRUE:
        XtAppProcessEvent (appcontext, XtIMAll);
        if (mode == G_ONEEVENT)
            return 1;
        rtn = 1;
        /* FALL THROUGH */
    case FALSE:
        while (XtAppPending (appcontext)) {
            XtAppProcessEvent (appcontext, XtIMAll);
            if (mode == G_ONEEVENT)
                return 1;
            rtn = 1;
        }
        break;
    }
    return rtn;
}
