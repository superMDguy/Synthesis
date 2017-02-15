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

#define WMU widget->u.m

static int menupoped;
static int menuselected;

static void mwcallback (Widget, XtPointer, XtPointer);

int GMcreatewidget (Gwidget_t *parent, Gwidget_t *widget,
        int attrn, Gwattr_t *attrp) {
    int ai;

    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRUSERDATA:
            widget->udata = attrp[ai].u.u;
            break;
        default:
            Gerr (POS, G_ERRBADATTRID, attrp[ai].id);
            return -1;
        }
    }
    if (!(widget->w = XtCreatePopupShell ("menu",
            simpleMenuWidgetClass, Groot, NULL, 0))) {
        Gerr (POS, G_ERRCANNOTCREATE);
        return -1;
    }
    XtAddCallback (widget->w, XtNpopdownCallback, mwcallback, (XtPointer) -1);
    WMU->count = 0;
    return 0;
}

int GMsetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
    int ai;

    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRUSERDATA:
            widget->udata = attrp[ai].u.u;
            break;
        default:
            Gerr (POS, G_ERRBADATTRID, attrp[ai].id);
            return -1;
        }
    }
    return 0;
}

int GMgetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
    int ai;

    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRUSERDATA:
            attrp[ai].u.u = widget->udata;
            break;
        default:
            Gerr (POS, G_ERRBADATTRID, attrp[ai].id);
            return -1;
        }
    }
    return 0;
}

int GMdestroywidget (Gwidget_t *widget) {
    XtDestroyWidget (widget->w);
    return 0;
}

int GMmenuaddentries (Gwidget_t *widget, int en, char **ep) {
    Widget mep;
    int ei;

    for (ei = 0; ei < en; ei++) {
        mep = XtCreateManagedWidget (ep[ei], smeBSBObjectClass,
                widget->w, NULL, 0);
        XtAddCallback (mep, XtNcallback, mwcallback, (XtPointer) WMU->count++);
    }
    return 0;
}

int GMmenudisplay (Gwidget_t *parent, Gwidget_t *widget) {
    Window rwin, cwin;
    int rx, ry, x, y;
    unsigned int mask;

    XQueryPointer (Gdisplay, XtWindow (parent->w),
            &rwin, &cwin, &rx, &ry, &x, &y, &mask);
    RESETARGS;
    ADD2ARGS (XtNx, rx);
    ADD2ARGS (XtNy, ry);
    XtSetValues (widget->w, argp, argn);
    menupoped = TRUE;
    menuselected = -1;
    XtPopupSpringLoaded (widget->w);
    while (menupoped)
        Gprocessevents (TRUE, G_ONEEVENT);
    Gpopdownflag = TRUE;
    return menuselected;
}

static void mwcallback (Widget w, XtPointer clientdata, XtPointer calldata) {
    if (((int) clientdata) > -1)
        menuselected = (int) clientdata;
    menupoped = FALSE;
}
