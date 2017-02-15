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

int GScreatewidget (Gwidget_t *parent, Gwidget_t *widget,
        int attrn, Gwattr_t *attrp) {
    PIXsize_t ps;
    Widget w;
    int ai;

    if (!parent) {
        Gerr (POS, G_ERRNOPARENTWIDGET);
        return -1;
    }
    ps.x = ps.y = MINSWSIZE;
    RESETARGS;
    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRSIZE:
            GETSIZE (attrp[ai].u.s, ps, MINSWSIZE);
            break;
        case G_ATTRBORDERWIDTH:
            ADD2ARGS (XtNborderWidth, attrp[ai].u.i);
            break;
        case G_ATTRCHILDCENTER:
            Gerr (POS, G_ERRCANNOTSETATTR1, "childcenter");
            return -1;
        case G_ATTRMODE:
            if (Strcmp ("forcebars", attrp[ai].u.t) == 0)
                ADD2ARGS (XtNforceBars, True);
            else {
                Gerr (POS, G_ERRBADATTRVALUE, attrp[ai].u.t);
                return -1;
            }
            break;
        case G_ATTRUSERDATA:
            widget->udata = attrp[ai].u.u;
            break;
        default:
            Gerr (POS, G_ERRBADATTRID, attrp[ai].id);
            return -1;
        }
    }
    ADD2ARGS (XtNallowHoriz, True);
    ADD2ARGS (XtNallowVert, True);
    ADD2ARGS (XtNwidth, ps.x);
    ADD2ARGS (XtNheight, ps.y);
    if (!(widget->w = XtCreateWidget ("scroll", viewportWidgetClass,
            parent->w, argp, argn))) {
        Gerr (POS, G_ERRCANNOTCREATE);
        return -1;
    }
#if XlibSpecificationRelease < 5
    RESETARGS;
    ADD2ARGS (XtNwidth, ps.x);
    ADD2ARGS (XtNheight, ps.y);
    if (!(w = XtCreateWidget ("owsucks", formWidgetClass,
            widget->w, argp, argn))) {
        Gerr (POS, G_ERRCANNOTCREATE);
        return -1;
    }
    Glazymanage (w);
    Glazymanage (widget->w);
    XtDestroyWidget (w);
#else
    Glazymanage (widget->w);
#endif
    return 0;
}

int GSsetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
    PIXpoint_t po;
    PIXsize_t ps;
    Dimension width, height;
    int ai;

    RESETARGS;
    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRSIZE:
            GETSIZE (attrp[ai].u.s, ps, MINSWSIZE);
            ADD2ARGS (XtNwidth, ps.x);
            ADD2ARGS (XtNheight, ps.y);
            break;
        case G_ATTRBORDERWIDTH:
            ADD2ARGS (XtNborderWidth, attrp[ai].u.i);
            break;
        case G_ATTRCHILDCENTER:
            GETORIGIN (attrp[ai].u.p, po);
            ADD2ARGS (XtNwidth, &width);
            ADD2ARGS (XtNheight, &height);
            XtGetValues (widget->w, argp, argn);
            po.x -= width / 2, po.y -= height / 2;
            if (po.x < 0)
                po.x = 0;
            if (po.y < 0)
                po.y = 0;
            XawViewportSetCoordinates (widget->w, po.x, po.y);
            break;
        case G_ATTRMODE:
            if (Strcmp ("forcebars", attrp[ai].u.t) == 0)
                ADD2ARGS (XtNforceBars, True);
            else {
                Gerr (POS, G_ERRBADATTRVALUE, attrp[ai].u.t);
                return -1;
            }
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

int GSgetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
    Dimension width, height;
    Position x, y;
    Boolean tf;
    Gwidget_t *child;
    int ai, wi;

    for (ai = 0; ai < attrn; ai++) {
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
        case G_ATTRCHILDCENTER:
            for (wi = 0; wi < Gwidgetn; wi++) {
                child = &Gwidgets[wi];
                if (child->inuse && child->pwi == widget - &Gwidgets[0])
                    break;
            }
            if (wi == Gwidgetn) {
                Gerr (POS, G_ERRNOCHILDWIDGET);
                return -1;
            }
            ADD2ARGS (XtNwidth, &width);
            ADD2ARGS (XtNheight, &height);
            XtGetValues (widget->w, argp, argn);
            RESETARGS;
            ADD2ARGS (XtNx, &x);
            ADD2ARGS (XtNy, &y);
            XtGetValues (child->w, argp, argn);
            attrp[ai].u.p.x = width / 2 - x, attrp[ai].u.p.y = height / 2 - y;
            break;
        case G_ATTRMODE:
            ADD2ARGS (XtNforceBars, &tf);
            attrp[ai].u.t = (tf == True) ? "forcebars" : "";
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

int GSdestroywidget (Gwidget_t *widget) {
    XtDestroyWidget (widget->w);
    return 0;
}

#if XlibSpecificationRelease < 5
#include <X11/IntrinsicP.h>
#include <X11/Xaw/ViewportP.h>

void XawViewportSetCoordinates (Widget gw, Position x, Position y) {
    ViewportWidget w = (ViewportWidget) gw;
    Widget child = w->viewport.child;
    Widget clip = w->viewport.clip;
    Position mx, my;

    if (x > (int) child->core.width)
      x = child->core.width;
    else if (x < 0)
      x = child->core.x;

    if (y > (int) child->core.height)
      y = child->core.height;
    else if (y < 0)
      y = child->core.y;

    mx = -x, my = -y;

    if (-mx + (int) clip->core.width > (int) child->core.width)
        mx = -(child->core.width - clip->core.width);

    if (-my + (int) clip->core.height > (int) child->core.height)
        my = -(child->core.height - clip->core.height);

    /* make sure we never move past left/top borders */
    if (mx >= 0) mx = 0;
    if (my >= 0) my = 0;

    XtMoveWidget (child, mx, my);
    XtUnmanageChild (child);
    XtManageChild (child);
}
#endif
