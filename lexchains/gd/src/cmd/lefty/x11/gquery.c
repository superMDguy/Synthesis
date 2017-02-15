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

#define WQU widget->u.q

static void qwcallback (Widget, XtPointer, XtPointer);
static void qbwcallback (Widget, XtPointer, XtPointer);

int GQcreatewidget (Gwidget_t *parent, Gwidget_t *widget,
        int attrn, Gwattr_t *attrp) {
    Widget w;
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
    if (!(widget->w = XtCreatePopupShell ("popup",
            transientShellWidgetClass, Groot, NULL, 0))) {
        Gerr (POS, G_ERRCANNOTCREATE);
        return -1;
    }
    XtAddCallback (widget->w, XtNpopdownCallback, qwcallback, NULL);
    RESETARGS;
    ADD2ARGS (XtNlabel, "prompt");
    ADD2ARGS (XtNvalue, "");
    if (!(WQU->w = XtCreateManagedWidget ("dialog",
            dialogWidgetClass, widget->w, argp, argn))) {
        Gerr (POS, G_ERRCANNOTCREATE);
        return -1;
    }
    XawDialogAddButton (WQU->w, "Cancel", qbwcallback, (XtPointer) 1);
    XawDialogAddButton (WQU->w, "OK", qbwcallback, (XtPointer) 2);
    if (!(w = XtNameToWidget (WQU->w, "value"))) {
        Gerr (POS, G_ERRCANNOTCREATE);
        return -1;
    }
    XtOverrideTranslations (w, Gqwpoptable);
    return 0;
}

int GQsetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
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

int GQgetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
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

int GQdestroywidget (Gwidget_t *widget) {
    XtDestroyWidget (widget->w);
    return 0;
}

int GQqueryask (Gwidget_t *widget, char *prompt,
        char *responsep, int responsen) {
    Window rwin, cwin;
    Dimension width, height;
    int rx, ry, x, y;
    unsigned int mask;
    char *s;

    RESETARGS;
    ADD2ARGS (XtNlabel, prompt);
    ADD2ARGS (XtNvalue, "");
    XtSetValues (WQU->w, argp, argn);
    XtRealizeWidget (widget->w);
    XQueryPointer (Gdisplay, XtWindow (widget->w),
            &rwin, &cwin, &rx, &ry, &x, &y, &mask);
    RESETARGS;
    ADD2ARGS (XtNwidth, &width);
    ADD2ARGS (XtNheight, &height);
    XtGetValues (widget->w, argp, argn);
    rx -= (width / 2), ry -= (height / 2);
    RESETARGS;
    ADD2ARGS (XtNx, rx);
    ADD2ARGS (XtNy, ry);
    XtSetValues (widget->w, argp, argn);
    WQU->state = 2;
    XtPopup (widget->w, XtGrabExclusive);
    s = NULL;
    while (WQU->state) {
        if (WQU->state == 1) {
            s = XawDialogGetValueString (WQU->w);
            strncpy (responsep, s, responsen);
            XtPopdown (widget->w);
        }
        Gprocessevents (TRUE, G_ONEEVENT);
    }
    if (!s)
        XtPopdown (widget->w);
    XtUnrealizeWidget (widget->w);
    Gpopdownflag = TRUE;
    if (!s)
        return -1;
    if (responsep[0] && responsep[strlen(responsep) - 1] == '\n')
        responsep[strlen(responsep) - 1] = 0;
    return 0;
}

void Gqwpopaction (Widget w, XEvent *evp, char **app, unsigned int *anp) {
    Gwidget_t *widget;

    widget = findwidget ((unsigned long) XtParent (XtParent (w)),
            G_QUERYWIDGET);
    WQU->state = 1;
}

static void qwcallback (Widget w, XtPointer clientdata, XtPointer calldata) {
    Gwidget_t *widget;

    widget = findwidget ((unsigned long) w, G_QUERYWIDGET);
    WQU->state = 0;
}

static void qbwcallback (Widget w, XtPointer clientdata, XtPointer calldata) {
    Gwidget_t *widget;

    widget = findwidget ((unsigned long) XtParent (XtParent (w)),
            G_QUERYWIDGET);
    if ((int) clientdata == 1) /* Cancel button */
        WQU->state = 0;
    else if ((int) clientdata == 2) /* OK Button */
        WQU->state = 1;
}
