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

Gwidget_t *Gwidgets;
int Gwidgetn;

char *Gdefaultfont;
int Gneedredraw;
int Gbuttonsdown;
int Gerrflag;
int Gerrno;

static long wsizes[G_WTYPESIZE];
static Gtextline_t tlarray[100];

Gwattrmap_t MSNEAR Gwattrmap[] = {
    G_ATTRORIGIN,      G_ATTRTYPEPOINT, "origin",
    G_ATTRSIZE,        G_ATTRTYPESIZE,  "size",
    G_ATTRBORDERWIDTH, G_ATTRTYPEINT,   "borderwidth",
    G_ATTRNAME,        G_ATTRTYPETEXT,  "name",
    G_ATTRTEXT,        G_ATTRTYPETEXT,  "text",
    G_ATTRAPPENDTEXT,  G_ATTRTYPETEXT,  "appendtext",
    G_ATTRCURSOR,      G_ATTRTYPETEXT,  "cursor",
    G_ATTRMODE,        G_ATTRTYPETEXT,  "mode",
    G_ATTRLAYOUT,      G_ATTRTYPETEXT,  "layout",
    G_ATTRZORDER,      G_ATTRTYPETEXT,  "zorder",
    G_ATTRCOLOR,       G_ATTRTYPECOLOR, "color",
    G_ATTRVIEWPORT,    G_ATTRTYPESIZE,  "viewport",
    G_ATTRWINDOW,      G_ATTRTYPERECT,  "window",
    G_ATTRCHILDCENTER, G_ATTRTYPEPOINT, "childcenter",
    G_ATTRNEWLINECB,   G_ATTRTYPEFUNC,  "newlinecb",
    G_ATTRRESIZECB,    G_ATTRTYPEFUNC,  "resizecb",
    G_ATTRBUTTONCB,    G_ATTRTYPEFUNC,  "buttoncb",
    G_ATTREVENTCB,     G_ATTRTYPEFUNC,  "eventcb",
    G_ATTRUSERDATA,    G_ATTRTYPEFUNC,  "userdata",
    -1,                -1,              NULL,
};

static int arrayattr[] = {
    G_ATTRSIZE,
    G_ATTRBORDERWIDTH,
    G_ATTRMODE,
    G_ATTRLAYOUT,
    G_ATTRRESIZECB,
    G_ATTRUSERDATA,
    -1
};

static int buttonattr[] = {
    G_ATTRSIZE,
    G_ATTRBORDERWIDTH,
    G_ATTRTEXT,
    G_ATTRBUTTONCB,
    G_ATTRUSERDATA,
    -1
};

static int canvasattr[] = {
    G_ATTRSIZE,
    G_ATTRBORDERWIDTH,
    G_ATTRCURSOR,
    G_ATTRCOLOR,
    G_ATTRVIEWPORT,
    G_ATTRWINDOW,
    G_ATTREVENTCB,
    G_ATTRUSERDATA,
    -1
};

static int labelattr[] = {
    G_ATTRSIZE,
    G_ATTRBORDERWIDTH,
    G_ATTRTEXT,
    G_ATTREVENTCB,
    G_ATTRUSERDATA,
    -1
};

static int menuattr[] = {
    G_ATTRUSERDATA,
    -1
};

static int pcanvasattr[] = {
    G_ATTRORIGIN,
    G_ATTRSIZE,
    G_ATTRNAME,
    G_ATTRMODE,
    G_ATTRCOLOR,
    G_ATTRWINDOW,
    G_ATTREVENTCB,
    G_ATTRUSERDATA,
    -1
};

static int queryattr[] = {
    G_ATTRUSERDATA,
    -1
};

static int scrollattr[] = {
    G_ATTRSIZE,
    G_ATTRBORDERWIDTH,
    G_ATTRCHILDCENTER,
    G_ATTRMODE,
    G_ATTRUSERDATA,
    -1,
};

static int textattr[] = {
    G_ATTRSIZE,
    G_ATTRBORDERWIDTH,
    G_ATTRTEXT,
    G_ATTRAPPENDTEXT,
    G_ATTRMODE,
    G_ATTRNEWLINECB,
    G_ATTRUSERDATA,
    -1
};

static int viewattr[] = {
    G_ATTRORIGIN,
    G_ATTRSIZE,
    G_ATTRNAME,
    G_ATTRZORDER,
    G_ATTREVENTCB,
    G_ATTRUSERDATA,
    -1
};

Gwlist_t MSNEAR Gwlist[] = {
    { G_ARRAYWIDGET,   "array",  &arrayattr[0],   },
    { G_BUTTONWIDGET,  "button", &buttonattr[0],  },
    { G_CANVASWIDGET,  "canvas", &canvasattr[0],  },
    { G_LABELWIDGET,   "label",  &labelattr[0],   },
    { G_MENUWIDGET,    "menu",   &menuattr[0],    },
    { G_PCANVASWIDGET, "ps",     &pcanvasattr[0], },
    { G_QUERYWIDGET,   "query",  &queryattr[0],   },
    { G_SCROLLWIDGET,  "scroll", &scrollattr[0],  },
    { G_TEXTWIDGET,    "text",   &textattr[0],    },
    { G_VIEWWIDGET,    "view",   &viewattr[0],    },
    { -1,               NULL,    NULL,            },
};

static char *errmsg[] = {
    /* Gerrno starts at 1     */ "no error",
    /* G_ERRBADATTRID         */ "bad attribute id %d",
    /* G_ERRBADATTRVALUE      */ "bad attribute value %s",
    /* G_ERRBADCOLORINDEX     */ "bad color index %d",
    /* G_ERRBADPARENTWIDGETID */ "bad parent widget id %d",
    /* G_ERRBADWIDGETID       */ "bad widget id %d",
    /* G_ERRBADWIDGETTYPE     */ "bad widget type %d",
    /* G_ERRCANNOTCREATE      */ "cannot create widget",
    /* G_ERRCANNOTGETATTR     */ "cannot get attribute %s",
    /* G_ERRCANNOTOPENFILE    */ "cannot open file %s",
    /* G_ERRCANNOTSETATTR1    */ "cannot set attribute %s in createwidget",
    /* G_ERRCANNOTSETATTR2    */ "cannot set attribute %s in setwidgetattr",
    /* G_ERRINITFAILED        */ "initialization failed",
    /* G_ERRNOCHILDWIDGET     */ "no child widget",
    /* G_ERRNOPARENTWIDGET    */ "no parent widget",
    /* G_ERRNOSUCHCURSOR      */ "no such cursor %s",
    /* G_ERRNOTACANVAS        */ "widget %d is not a canvas",
    /* G_ERRNOTIMPLEMENTED    */ "not implemented",
    /* G_ERRNOTSUPPORTED      */ "not supported"
};

static int unpackstring (char *);

int Ginit (void) {
    int wi;

    wsizes[G_ARRAYWIDGET]   = AWSIZE;
    wsizes[G_BUTTONWIDGET]  = BWSIZE;
    wsizes[G_CANVASWIDGET]  = CWSIZE;
    wsizes[G_LABELWIDGET]   = LWSIZE;
    wsizes[G_MENUWIDGET]    = MWSIZE;
    wsizes[G_PCANVASWIDGET] = PWSIZE;
    wsizes[G_QUERYWIDGET]   = QWSIZE;
    wsizes[G_SCROLLWIDGET]  = SWSIZE;
    wsizes[G_TEXTWIDGET]    = TWSIZE;
    wsizes[G_VIEWWIDGET]    = VWSIZE;
    Gwidgets = Marrayalloc ((long) WIDGETINCR * WIDGETSIZE);
    Gwidgetn = WIDGETINCR;
    for (wi = 0; wi < Gwidgetn; wi++)
        Gwidgets[wi].inuse = FALSE;
    Gneedredraw = FALSE;
    Gbuttonsdown = 0;
    Gdefaultfont = "";
    Gerrflag = FALSE;
    Ginitgraphics ();
    return 0;
}

int Gterm (void) {
    int wi;

    for (wi = 0; wi < Gwidgetn; wi++)
        if (Gwidgets[wi].inuse)
            Gdestroywidget (wi);
    Gtermgraphics ();
    Marrayfree (Gwidgets), Gwidgets = NULL, Gwidgetn = 0;
    return 0;
}

int Gcreatewidget (int pwi, int type, int attrn, Gwattr_t *attrp) {
    Gwidget_t *parent, *widget;
    int rtn;

    if ((pwi != -1) && (pwi < 0 || pwi > Gwidgetn || !Gwidgets[pwi].inuse)) {
        Gerr (POS, G_ERRBADPARENTWIDGETID, pwi);
        return -1;
    }
    if (type < 0 || type >= G_WTYPESIZE) {
        Gerr (POS, G_ERRBADWIDGETTYPE, type);
        return -1;
    }
    parent = (pwi == -1) ? NULL : &Gwidgets[pwi];
    widget = newwidget (type);
    widget->inuse = TRUE;
    widget->pwi = pwi;
    rtn = -1;
    switch (type) {
    case G_ARRAYWIDGET:
        rtn = GAcreatewidget (parent, widget, attrn, attrp);
        break;
    case G_BUTTONWIDGET:
        rtn = GBcreatewidget (parent, widget, attrn, attrp);
        break;
    case G_CANVASWIDGET:
        rtn = GCcreatewidget (parent, widget, attrn, attrp);
        break;
    case G_LABELWIDGET:
        rtn = GLcreatewidget (parent, widget, attrn, attrp);
        break;
    case G_MENUWIDGET:
        rtn = GMcreatewidget (parent, widget, attrn, attrp);
        break;
    case G_PCANVASWIDGET:
        rtn = GPcreatewidget (parent, widget, attrn, attrp);
        break;
    case G_QUERYWIDGET:
        rtn = GQcreatewidget (parent, widget, attrn, attrp);
        break;
    case G_SCROLLWIDGET:
        rtn = GScreatewidget (parent, widget, attrn, attrp);
        break;
    case G_TEXTWIDGET:
        rtn = GTcreatewidget (parent, widget, attrn, attrp);
        break;
    case G_VIEWWIDGET:
        rtn = GVcreatewidget (parent, widget, attrn, attrp);
        break;
    }
    if (rtn == -1) {
        widget->inuse = FALSE;
        return -1;
    }
    return widget - &Gwidgets[0];
}

int Gsetwidgetattr (int wi, int attrn, Gwattr_t *attrp) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    switch (widget->type) {
    case G_ARRAYWIDGET:   return GAsetwidgetattr (widget, attrn, attrp);
    case G_BUTTONWIDGET:  return GBsetwidgetattr (widget, attrn, attrp);
    case G_CANVASWIDGET:  return GCsetwidgetattr (widget, attrn, attrp);
    case G_LABELWIDGET:   return GLsetwidgetattr (widget, attrn, attrp);
    case G_MENUWIDGET:    return GMsetwidgetattr (widget, attrn, attrp);
    case G_PCANVASWIDGET: return GPsetwidgetattr (widget, attrn, attrp);
    case G_QUERYWIDGET:   return GQsetwidgetattr (widget, attrn, attrp);
    case G_SCROLLWIDGET:  return GSsetwidgetattr (widget, attrn, attrp);
    case G_TEXTWIDGET:    return GTsetwidgetattr (widget, attrn, attrp);
    case G_VIEWWIDGET:    return GVsetwidgetattr (widget, attrn, attrp);
    }
    return -1;
}

int Ggetwidgetattr (int wi, int attrn, Gwattr_t *attrp) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    switch (widget->type) {
    case G_ARRAYWIDGET:   return GAgetwidgetattr (widget, attrn, attrp);
    case G_BUTTONWIDGET:  return GBgetwidgetattr (widget, attrn, attrp);
    case G_CANVASWIDGET:  return GCgetwidgetattr (widget, attrn, attrp);
    case G_LABELWIDGET:   return GLgetwidgetattr (widget, attrn, attrp);
    case G_MENUWIDGET:    return GMgetwidgetattr (widget, attrn, attrp);
    case G_PCANVASWIDGET: return GPgetwidgetattr (widget, attrn, attrp);
    case G_QUERYWIDGET:   return GQgetwidgetattr (widget, attrn, attrp);
    case G_SCROLLWIDGET:  return GSgetwidgetattr (widget, attrn, attrp);
    case G_TEXTWIDGET:    return GTgetwidgetattr (widget, attrn, attrp);
    case G_VIEWWIDGET:    return GVgetwidgetattr (widget, attrn, attrp);
    }
    return -1;
}

int Gdestroywidget (int wi) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    switch (widget->type) {
    case G_ARRAYWIDGET:   GAdestroywidget (widget); break;
    case G_BUTTONWIDGET:  GBdestroywidget (widget); break;
    case G_CANVASWIDGET:  GCdestroywidget (widget); break;
    case G_LABELWIDGET:   GLdestroywidget (widget); break;
    case G_MENUWIDGET:    GMdestroywidget (widget); break;
    case G_PCANVASWIDGET: GPdestroywidget (widget); break;
    case G_QUERYWIDGET:   GQdestroywidget (widget); break;
    case G_SCROLLWIDGET:  GSdestroywidget (widget); break;
    case G_TEXTWIDGET:    GTdestroywidget (widget); break;
    case G_VIEWWIDGET:    GVdestroywidget (widget); break;
    }
    /* HACK: should do switch on type ... */
    free (widget->u.c);
    widget->inuse = FALSE;
    return 0;
}

int Gqueryask (int wi, char *prompt, char *responsep, int responsen) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    return GQqueryask (widget, prompt, responsep, responsen);
}

int Gmenuaddentries (int wi, int en, char **ep) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    return GMmenuaddentries (widget, en, ep);
}

int Gmenudisplay (int pwi, int wi) {
    Gwidget_t *parent, *widget;

    if (pwi < 0 || pwi > Gwidgetn || !Gwidgets[pwi].inuse) {
        Gerr (POS, G_ERRBADPARENTWIDGETID, wi);
        return -1;
    }
    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    parent = &Gwidgets[pwi];
    widget = &Gwidgets[wi];
    return GMmenudisplay (parent, widget);
}

/* functions for drawing on canvas and pcanvas widgets */

int Gcanvasclear (int wi) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    switch (widget->type) {
    case G_CANVASWIDGET:  return GCcanvasclear (widget);
    case G_PCANVASWIDGET: return GPcanvasclear (widget);
    }
    Gerr (POS, G_ERRNOTACANVAS, wi);
    return -1;
}

int Gsetgfxattr (int wi, Ggattr_t *ap) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    switch (widget->type) {
    case G_CANVASWIDGET:  return GCsetgfxattr (widget, ap);
    case G_PCANVASWIDGET: return GPsetgfxattr (widget, ap);
    }
    Gerr (POS, G_ERRNOTACANVAS, wi);
    return -1;
}

int Ggetgfxattr (int wi, Ggattr_t *ap) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    switch (widget->type) {
    case G_CANVASWIDGET:  return GCgetgfxattr (widget, ap);
    case G_PCANVASWIDGET: return GPgetgfxattr (widget, ap);
    }
    Gerr (POS, G_ERRNOTACANVAS, wi);
    return -1;
}

int Garrow (int wi, Gpoint_t gp1, Gpoint_t gp2, Ggattr_t *ap) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    switch (widget->type) {
    case G_CANVASWIDGET:  return GCarrow (widget, gp1, gp2, ap);
    case G_PCANVASWIDGET: return GParrow (widget, gp1, gp2, ap);
    }
    Gerr (POS, G_ERRNOTACANVAS, wi);
    return -1;
}

int Gline (int wi, Gpoint_t gp1, Gpoint_t gp2, Ggattr_t *ap) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    switch (widget->type) {
    case G_CANVASWIDGET:  return GCline (widget, gp1, gp2, ap);
    case G_PCANVASWIDGET: return GPline (widget, gp1, gp2, ap);
    }
    Gerr (POS, G_ERRNOTACANVAS, wi);
    return -1;
}

int Gbox (int wi, Grect_t gr, Ggattr_t *ap) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    switch (widget->type) {
    case G_CANVASWIDGET:  return GCbox (widget, gr, ap);
    case G_PCANVASWIDGET: return GPbox (widget, gr, ap);
    }
    Gerr (POS, G_ERRNOTACANVAS, wi);
    return -1;
}

int Gpolygon (int wi, int gpn, Gpoint_t *gpp, Ggattr_t *ap) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    switch (widget->type) {
    case G_CANVASWIDGET:  return GCpolygon (widget, gpn, gpp, ap);
    case G_PCANVASWIDGET: return GPpolygon (widget, gpn, gpp, ap);
    }
    Gerr (POS, G_ERRNOTACANVAS, wi);
    return -1;
}

int Gsplinegon (int wi, int gpn, Gpoint_t *gpp, Ggattr_t *ap) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    switch (widget->type) {
    case G_CANVASWIDGET:  return GCsplinegon (widget, gpn, gpp, ap);
    case G_PCANVASWIDGET: return GPsplinegon (widget, gpn, gpp, ap);
    }
    Gerr (POS, G_ERRNOTACANVAS, wi);
    return -1;
}

int Garc (int wi, Gpoint_t gc, Gsize_t gs, double ang1, double ang2,
        Ggattr_t *ap) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    switch (widget->type) {
    case G_CANVASWIDGET:  return GCarc (widget, gc, gs, ang1, ang2, ap);
    case G_PCANVASWIDGET: return GParc (widget, gc, gs, ang1, ang2, ap);
    }
    Gerr (POS, G_ERRNOTACANVAS, wi);
    return -1;
}

int Gtext (int wi, char *string, Gpoint_t go, char *fn, double fs,
        char *justs, Ggattr_t *ap) {
    Gwidget_t *widget;
    char js[2];
    int n;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    if (string[0] == '\000')
        return 0;
    n = unpackstring (string);
    if (!justs[0] || !justs[1])
        js[0] = 'c', js[1] = 'c';
    else {
        js[0] = justs[0], js[1] = justs[1];
        if (js[0] != 'l' && js[0] != 'c' && js[0] != 'r')
            js[0] = 'c';
        if (js[1] != 'd' && js[1] != 'c' && js[1] != 'u')
            js[1] = 'c';
    }
    switch (widget->type) {
    case G_CANVASWIDGET:
        return GCtext (widget, &tlarray[0], n, go, fn, fs, &js[0], ap);
    case G_PCANVASWIDGET:
        return GPtext (widget, &tlarray[0], n, go, fn, fs, &js[0], ap);
    }
    Gerr (POS, G_ERRNOTACANVAS, wi);
    return -1;
}

int Ggettextsize (int wi, char *string, char *fn, double fs, Gsize_t *gsp) {
    Gwidget_t *widget;
    int n;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    if (widget->type != G_CANVASWIDGET) {
        Gerr (POS, G_ERRNOTACANVAS, wi);
        return -1;
    }
    if (string[0] == '\000') {
        gsp->x = gsp->y = 0.0;
        return 0;
    }
    n = unpackstring (string);
    return GCgettextsize (widget, &tlarray[0], n, fn, fs, gsp);
}

int Ggetmousecoords (int wi, Gpoint_t *gsp, int *count) {
    Gwidget_t *widget;

    if (wi < 0 || wi > Gwidgetn || !Gwidgets[wi].inuse) {
        Gerr (POS, G_ERRBADWIDGETID, wi);
        return -1;
    }
    widget = &Gwidgets[wi];
    if (widget->type != G_CANVASWIDGET) {
        Gerr (POS, G_ERRNOTACANVAS, wi);
        return -1;
    }
    return GCgetmousecoords (widget, gsp, count);
}

Gwidget_t *newwidget (int type) {
    Gwidget_t *new;
    int wi;

    for (wi = 0; wi < Gwidgetn; wi++)
        if (!Gwidgets[wi].inuse)
            goto found;

    Gwidgets = Marraygrow (Gwidgets,
            (long) (Gwidgetn + WIDGETINCR) * WIDGETSIZE);
    for (wi = Gwidgetn; wi < Gwidgetn + WIDGETINCR; wi++)
        Gwidgets[wi].inuse = FALSE;
    wi = Gwidgetn, Gwidgetn += WIDGETINCR;

found:
    new = &Gwidgets[wi];
    new->type = type;
    new->w = 0;
    new->udata = 0;
    /* HACK: should do a switch on type, but ... */
    if (!(new->u.c = (Gcw_t *) malloc (wsizes[type])))
        panic (POS, "newwidget", "cannot allocate data");
    return new;
}

Gwidget_t *findwidget (unsigned long w, int type) {
    int wi;

    if (type == G_WTYPESIZE) {
        for (wi = 0; wi < Gwidgetn; wi++)
            if (Gwidgets[wi].inuse && (unsigned long) Gwidgets[wi].w == w)
                return &Gwidgets[wi];
    } else {
        for (wi = 0; wi < Gwidgetn; wi++)
            if (Gwidgets[wi].inuse && Gwidgets[wi].type == type &&
                    (unsigned long) Gwidgets[wi].w == w)
                return &Gwidgets[wi];
    }
    return NULL;
}

void Gerr (char *file, int line, int errnum, ...) {
    va_list args;

#ifndef HAVEMSWIN
    Gerrno = errnum;
    if (!Gerrflag)
        return;

    va_start(args, errnum);
    fprintf (stderr, "warning: (file %s, line %d) ", file, line);
    vfprintf (stderr, errmsg[errnum], args);
    fprintf (stderr, "\n");
    va_end(args);
#else
    char buf[256];

    Gerrno = errnum;
    if (!warnflag)
        return;

    va_start(args, errnum);
    vsprintf (buf, errmsg[errnum], args);
    Gnocallbacks = TRUE;
    MessageBox ((HWND) NULL, buf, "Lefty Warning", MB_APPLMODAL);
    Gnocallbacks = FALSE;
    va_end(args);
#endif
}

static int unpackstring (char *s) {
    char *p;
    int n;

    n = 0;
    p = s;
    tlarray[0].p = p;
    while (*p) {
        if (p[0] == '\\' && (p[1] == 'n' || p[1] == 'l' || p[1] == 'r')) {
            tlarray[n].n = p - tlarray[n].p;
            tlarray[n].j = p[1];
            tlarray[++n].p = (p += 2);
        } else
            p++;
    }
    if ((tlarray[n].n = p - tlarray[n].p) > 0)
        tlarray[n++].j = 'n';
    return n;
}
