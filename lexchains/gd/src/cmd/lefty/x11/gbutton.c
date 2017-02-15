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

#define WBU widget->u.b

static void bwcallback (Widget, XtPointer, XtPointer);

int GBcreatewidget (Gwidget_t *parent, Gwidget_t *widget,
        int attrn, Gwattr_t *attrp) {
    PIXsize_t ps;
    char *s;
    int ai;

    if (!parent) {
        Gerr (POS, G_ERRNOPARENTWIDGET);
        return -1;
    }
    WBU->func = NULL;
    ps.x = ps.y = MINBWSIZE;
    s = NULL;
    RESETARGS;
    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRSIZE:
            GETSIZE (attrp[ai].u.s, ps, MINBWSIZE);
            break;
        case G_ATTRBORDERWIDTH:
            ADD2ARGS (XtNborderWidth, attrp[ai].u.i);
            break;
        case G_ATTRTEXT:
            s = attrp[ai].u.t;
            ADD2ARGS (XtNlabel, s);
            break;
        case G_ATTRBUTTONCB:
            WBU->func = attrp[ai].u.func;
            break;
        case G_ATTRUSERDATA:
            widget->udata = attrp[ai].u.u;
            break;
        default:
            Gerr (POS, G_ERRBADATTRID, attrp[ai].id);
            return -1;
        }
    }
    if (!s || s[0] == '\000') {
        ADD2ARGS (XtNwidth, ps.x);
        ADD2ARGS (XtNheight, ps.y);
    } else {
        if (ps.x > MINBWSIZE)
            ADD2ARGS (XtNwidth, ps.x);
        if (ps.y > MINBWSIZE)
            ADD2ARGS (XtNheight, ps.y);
    }
    ADD2ARGS (XtNhighlightThickness, 0);
    ADD2ARGS (XtNinternalHeight, 0);
    ADD2ARGS (XtNinternalWidth, 0);
    ADD2ARGS (XtNjustify, XtJustifyLeft);
    if (!(widget->w = XtCreateWidget ("command", commandWidgetClass,
            parent->w, argp, argn))) {
        Gerr (POS, G_ERRCANNOTCREATE);
        return -1;
    }
    XtAddCallback (widget->w, XtNcallback, bwcallback,
            (XtPointer) widget->udata);
    Glazymanage (widget->w);
    return 0;
}

int GBsetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
    PIXsize_t ps;
    int ai;

    RESETARGS;
    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRSIZE:
            GETSIZE (attrp[ai].u.s, ps, MINBWSIZE);
            ADD2ARGS (XtNwidth, ps.x);
            ADD2ARGS (XtNheight, ps.y);
            break;
        case G_ATTRBORDERWIDTH:
            ADD2ARGS (XtNborderWidth, attrp[ai].u.i);
            break;
        case G_ATTRTEXT:
            ADD2ARGS (XtNlabel, attrp[ai].u.t);
            break;
        case G_ATTRBUTTONCB:
            WBU->func = attrp[ai].u.func;
            break;
        case G_ATTRUSERDATA:
            widget->udata = attrp[ai].u.u;
            break;
        default:
            Gerr (POS, G_ERRBADATTRID, attrp[ai].id);
            return -1;
        }
    }
    XtSetValues (widget->w, argp, argn);
    return 0;
}

int GBgetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
    Dimension width, height;
    int ai;

    for (ai = 0; ai < attrn; ai++) {
        RESETARGS;
        switch (attrp[ai].id) {
        case G_ATTRSIZE:
            ADD2ARGS (XtNwidth, &width);
            ADD2ARGS (XtNheight, &height);
            XtGetValues (widget->w, argp, argn);
            attrp[ai].u.s.x = width, attrp[ai].u.s.y = height;
            break;
        case G_ATTRBORDERWIDTH:
            ADD2ARGS (XtNborderWidth, &width);
            XtGetValues (widget->w, argp, argn);
            attrp[ai].u.i = width;
            break;
        case G_ATTRTEXT:
            ADD2ARGS (XtNlabel, &Gbufp[0]);
            XtGetValues (widget->w, argp, argn);
            attrp[ai].u.t = &Gbufp[0];
            break;
        case G_ATTRBUTTONCB:
            attrp[ai].u.func = WBU->func;
            break;
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

int GBdestroywidget (Gwidget_t *widget) {
   XtDestroyWidget (widget->w);
    return 0;
}

static void bwcallback (Widget w, XtPointer clientdata, XtPointer calldata) {
    Gwidget_t *widget;

    widget = findwidget ((unsigned long) w, G_BUTTONWIDGET);
    if (WBU->func)
        (*WBU->func) (widget - &Gwidgets[0], clientdata);
}
