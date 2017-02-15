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

#define WLU widget->u.l

int GLcreatewidget (Gwidget_t *parent, Gwidget_t *widget,
        int attrn, Gwattr_t *attrp) {
    PIXsize_t ps;
    int ai;

    if (!parent) {
        Gerr (POS, G_ERRNOPARENTWIDGET);
        return -1;
    }
    WLU->func = NULL;
    ps.x = ps.y = MINLWSIZE;
    RESETARGS;
    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRSIZE:
            GETSIZE (attrp[ai].u.s, ps, MINLWSIZE);
            break;
        case G_ATTRBORDERWIDTH:
            ADD2ARGS (XtNborderWidth, attrp[ai].u.i);
            break;
        case G_ATTRTEXT:
            ADD2ARGS (XtNlabel, attrp[ai].u.t);
            break;
        case G_ATTREVENTCB:
            WLU->func = attrp[ai].u.func;
            break;
        case G_ATTRUSERDATA:
            widget->udata = attrp[ai].u.u;
            break;
        default:
            Gerr (POS, G_ERRBADATTRID, attrp[ai].id);
            return -1;
        }
    }
    ADD2ARGS (XtNwidth, ps.x);
    ADD2ARGS (XtNheight, ps.y);
    ADD2ARGS (XtNhighlightThickness, 0);
    ADD2ARGS (XtNinternalHeight, 0);
    ADD2ARGS (XtNinternalWidth, 0);
    ADD2ARGS (XtNjustify, XtJustifyLeft);
    if (!(widget->w = XtCreateWidget ("label", labelWidgetClass,
            parent->w, argp, argn))) {
        Gerr (POS, G_ERRCANNOTCREATE);
        return -1;
    }
    XtOverrideTranslations (widget->w, Glwanytable);
    Glazymanage (widget->w);
    return 0;
}

int GLsetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
    PIXsize_t ps;
    int ai;

    RESETARGS;
    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRSIZE:
            GETSIZE (attrp[ai].u.s, ps, MINLWSIZE);
            ADD2ARGS (XtNwidth, ps.x);
            ADD2ARGS (XtNheight, ps.y);
            break;
        case G_ATTRBORDERWIDTH:
            ADD2ARGS (XtNborderWidth, attrp[ai].u.i);
            break;
        case G_ATTRTEXT:
            ADD2ARGS (XtNlabel, attrp[ai].u.t);
            break;
        case G_ATTREVENTCB:
            WLU->func = attrp[ai].u.func;
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

int GLgetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
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
        case G_ATTREVENTCB:
            attrp[ai].u.func = WLU->func;
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

int GLdestroywidget (Gwidget_t *widget) {
    XtDestroyWidget (widget->w);
    return 0;
}

void Glwanyaction (Widget w, XEvent *evp, char **app, unsigned int *anp) {
    Gwidget_t *widget;
    Gevent_t gev;
    int wi, xtype;
    char c;

    widget = findwidget ((unsigned long) w, G_LABELWIDGET);
    switch ((xtype = evp->type)) {
    case KeyPress:
    case KeyRelease:
        if (!XLookupString ((XKeyEvent *) evp, &c, 1, NULL, NULL))
            return;
        gev.type = G_KEYBD;
        gev.code = (xtype == KeyPress) ? G_DOWN : G_UP;
        gev.data = c;
        break;
    case ButtonPress:
    case ButtonRelease:
        gev.type = G_MOUSE;
        gev.code = (xtype == ButtonPress) ? G_DOWN : G_UP;
        gev.data = evp->xbutton.button - Button1;
        break;
    default:
        return;
    }
    wi = gev.wi = widget - &Gwidgets[0];
    if (WLU->func)
        (*WLU->func) (&gev);
    if (Gpopdownflag) {
        Gpopdownflag = FALSE;
        if (gev.type == G_MOUSE && gev.code == G_DOWN) {
            gev.code = G_UP;
            widget = &Gwidgets[wi];
            if (widget->inuse && WLU->func)
                (*WLU->func) (&gev);
        }
    }
}
