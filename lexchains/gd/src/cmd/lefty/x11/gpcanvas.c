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

#define PSDPI 300.0
#define PSMAXPIXW (8.0  * PSDPI)
#define PSMAXPIXH (10.5 * PSDPI)
#define PSPTPI 72.0
#define PSMAXPTW (8.0  * PSPTPI)
#define PSMAXPTH (10.5 * PSPTPI)
#define PSXOFF 18
#define PSYOFF 18

static PIXsize_t maxsize = { PSMAXPIXW, PSMAXPIXH };
static int count;

#define WPU widget->u.p
#define FP widget->u.p->fp

#define RED   WPU->colors[WPU->gattr.color].nr
#define GREEN WPU->colors[WPU->gattr.color].ng
#define BLUE  WPU->colors[WPU->gattr.color].nb

static char *gstyles[5] = {
    /* G_SOLID */       "16  0",
    /* G_DASHED */      " 4  4",
    /* G_DOTTED */      " 2  2",
    /* G_LONGDASHED */  " 4 12",
    /* G_SHORTDASHED */ "12  4",
};

char *Gpscanvasname = "out.ps";

static char *findfont (char *);
static void setgattr (Gwidget_t *, Ggattr_t *);

static PIXrect_t rdrawtopix (Gwidget_t *, Grect_t);
static PIXpoint_t pdrawtopix (Gwidget_t *, Gpoint_t);
static PIXsize_t sdrawtopix (Gwidget_t *, Gsize_t);

int GPcreatewidget (Gwidget_t *parent, Gwidget_t *widget,
        int attrn, Gwattr_t *attrp) {
    PIXpoint_t po;
    PIXsize_t ps;
    struct Gpwcolor_t *cp;
    FILE *pfp;
    char buf[120];
    char *s, *path;
    int color, lflag, ai, i, x, y, w, h, r, g, b;

    if (!(path = buildpath ("lefty.psp", 0)) ||
            !(pfp = fopen (path, "r"))) {
        Gerr (POS, G_ERRCANNOTOPENFILE, "lefty.psp");
        return -1;
    }
    s = Gpscanvasname;
    lflag = FALSE;
    po.x = po.y = 0;
    ps.x = ps.y = MINPWSIZE;
    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRORIGIN:
            GETORIGIN (attrp[ai].u.p, po);
            break;
        case G_ATTRSIZE:
            GETSIZE (attrp[ai].u.s, ps, MINPWSIZE);
            break;
        case G_ATTRNAME:
            if (attrp[ai].u.t && attrp[ai].u.t[0])
                s = attrp[ai].u.t;
            break;
        case G_ATTRMODE:
            if (Strcmp ("landscape", attrp[ai].u.t) == 0)
                lflag = TRUE;
            else if (Strcmp ("portrait", attrp[ai].u.t) == 0)
                lflag = FALSE;
            else {
                Gerr (POS, G_ERRBADATTRVALUE, attrp[ai].u.t);
                return -1;
            }
            break;
        case G_ATTRCOLOR:
            /* will do it after the widget is created */
            break;
        case G_ATTRVIEWPORT:
            /* will do it after the widget is created */
            break;
        case G_ATTRWINDOW:
            /* will do it after the widget is created */
            break;
        case G_ATTRUSERDATA:
            widget->udata = attrp[ai].u.u;
            break;
        default:
            Gerr (POS, G_ERRBADATTRID, attrp[ai].id);
            return -1;
        }
    }
    if (!(FP = fopen (s, "w"))) {
        Gerr (POS, G_ERRCANNOTOPENFILE, s);
        return -1;
    }
    WPU->colors[0].r = WPU->colors[0].g = WPU->colors[0].b = 255;
    WPU->colors[0].nr = WPU->colors[0].ng = WPU->colors[0].nb = 1.0;
    WPU->colors[0].inuse = TRUE;
    WPU->colors[1].r = WPU->colors[1].g = WPU->colors[1].b = 0;
    WPU->colors[1].nr = WPU->colors[1].ng = WPU->colors[1].nb = 0.0;
    WPU->colors[1].inuse = TRUE;
    for (i = 2; i < G_MAXCOLORS; i++)
        WPU->colors[i].inuse = FALSE;
    WPU->gattr.color = 1;
    WPU->gattr.width = 0;
    WPU->gattr.mode = -1;
    WPU->gattr.fill = 0;
    WPU->gattr.style = 0;
    WPU->wrect.o.x = 0.0, WPU->wrect.o.y = 0.0;
    WPU->wrect.c.x = 1.0, WPU->wrect.c.y = 1.0;
    WPU->vsize.x = ps.x, WPU->vsize.y = ps.y;
    if (lflag)
        x = po.y, y = po.x, w = ps.y, h = ps.x;
    else
        x = po.x, y = po.y, w = ps.x, h = ps.y;
    fprintf (FP, "%%! LEFTY Output\n");
    fprintf (FP, "%%%%BoundingBox: %d %d %d %d\n",
            (int) (PSXOFF + PSMAXPTW * x / maxsize.x + 0.5),
            (int) (PSYOFF + PSMAXPTH * y / maxsize.y + 0.5),
            (int) (PSXOFF + PSMAXPTW * w / maxsize.x + 0.5),
            (int) (PSYOFF + PSMAXPTH * h / maxsize.y + 0.5));
    fprintf (FP, "%%%%EndComments\n");
    while (fgets (buf, 120, pfp))
        fputs (&buf[0], FP);
    fclose (pfp);
    fprintf (FP, "/ICP { %d %d %d %d SCP } def\n",
            (int) po.x, (int) po.y,
            (int) (po.x + ps.x - 1), (int) (po.y + ps.y - 1));
    fprintf (FP, "/BB { %d %d %d %d } def\n",
            (int) po.x, (int) po.y,
            (int) (po.x + ps.x - 1), (int) (po.y + ps.y - 1));
    if (!lflag)
        fprintf (FP, "[%f 0 0 %f %d %d] concat\n",
                PSPTPI / PSDPI, PSPTPI / PSDPI, PSXOFF, PSYOFF);
    else
        fprintf (FP, "[0 %f %f 0 %d %d] concat\n",
                PSPTPI / PSDPI, - PSPTPI / PSDPI,
                (int) (PSXOFF + ps.y * PSPTPI / PSDPI), PSYOFF);
    fprintf (FP, "%d %d translate ICP\n", (int) po.x, (int) po.y);
    fprintf (FP, "1 setlinecap\n");
    fprintf (FP, "ICP\n");
    WPU->gattr.color = 1;
    fprintf (FP, "%f %f %f CL\n", RED, GREEN, BLUE);
    WPU->defgattr = WPU->gattr;
    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRCOLOR:
            color = attrp[ai].u.c.index;
            if (color < 0 || color > G_MAXCOLORS) {
                Gerr (POS, G_ERRBADCOLORINDEX, attrp[ai].u.c.index);
                return -1;
            }
            cp = &WPU->colors[color];
            r = attrp[ai].u.c.r * 257;
            g = attrp[ai].u.c.g * 257;
            b = attrp[ai].u.c.b * 257;
            cp->r = r, cp->g = g, cp->b = b;
            cp->nr = r / 256.0, cp->ng = g / 256.0, cp->nb = b / 256.0;
            cp->inuse = TRUE;
            break;
        case G_ATTRVIEWPORT:
            WPU->vsize.x = attrp[ai].u.s.x + 0.5;
            WPU->vsize.y = attrp[ai].u.s.y + 0.5;
            fprintf (FP, "0 0 %d %d SCP\n",
                    (int) WPU->vsize.x, (int) WPU->vsize.y);
            break;
        case G_ATTRWINDOW:
            WPU->wrect = attrp[ai].u.r;
            break;
        }
    }
    return 0;
}

int GPsetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
    struct Gpwcolor_t *cp;
    int color, ai, r, g, b;

    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRORIGIN:
            break;
        case G_ATTRSIZE:
            break;
        case G_ATTRNAME:
            break;
        case G_ATTRMODE:
            break;
        case G_ATTRCOLOR:
            color = attrp[ai].u.c.index;
            if (color < 0 || color > G_MAXCOLORS) {
                Gerr (POS, G_ERRBADCOLORINDEX, attrp[ai].u.c.index);
                return -1;
            }
            cp = &WPU->colors[color];
            r = attrp[ai].u.c.r * 257;
            g = attrp[ai].u.c.g * 257;
            b = attrp[ai].u.c.b * 257;
            cp->r = r, cp->g = g, cp->b = b;
            cp->nr = r / 256.0, cp->ng = g / 256.0, cp->nb = b / 256.0;
            cp->inuse = TRUE;
            break;
        case G_ATTRVIEWPORT:
            WPU->vsize.x = attrp[ai].u.s.x + 0.5;
            WPU->vsize.y = attrp[ai].u.s.y + 0.5;
            fprintf (FP, "0 0 %d %d SCP\n",
                    (int) WPU->vsize.x, (int) WPU->vsize.y);
            break;
        case G_ATTRWINDOW:
            WPU->wrect = attrp[ai].u.r;
            break;
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

int GPgetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
    struct Gpwcolor_t *cp;
    int color, ai;

    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRORIGIN:
            break;
        case G_ATTRSIZE:
            break;
        case G_ATTRNAME:
            break;
        case G_ATTRMODE:
            break;
        case G_ATTRCOLOR:
            color = attrp[ai].u.c.index;
            if (color < 0 || color > G_MAXCOLORS || !WPU->colors[color].inuse) {
                Gerr (POS, G_ERRBADCOLORINDEX, attrp[ai].u.c.index);
                return -1;
            }
            cp = &WPU->colors[color];
            attrp[ai].u.c.r = cp->r;
            attrp[ai].u.c.g = cp->g;
            attrp[ai].u.c.b = cp->b;
            break;
        case G_ATTRVIEWPORT:
            attrp[ai].u.s = WPU->vsize;
            break;
        case G_ATTRWINDOW:
            attrp[ai].u.r = WPU->wrect;
            break;
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

int GPdestroywidget (Gwidget_t *widget) {
    fprintf (FP, "stroke showpage\n");
    fclose (FP);
    return 0;
}

int GPcanvasclear (Gwidget_t *widget) {
    fprintf (FP, "ICP DO\n");
    WPU->gattr.color = 0;
    fprintf (FP, "%f %f %f CL\n", RED, GREEN, BLUE);
    fprintf (FP, "BB BOX FI\n");
    WPU->gattr.color = -1;
    fprintf (FP, "0 0 %d %d SCP\n", (int) WPU->vsize.x, (int) WPU->vsize.y);
    return 0;
}

int GPsetgfxattr (Gwidget_t *widget, Ggattr_t *ap) {
    setgattr (widget, ap);
    WPU->defgattr = WPU->gattr;
    return 0;
}

int GPgetgfxattr (Gwidget_t *widget, Ggattr_t *ap) {
    if ((ap->flags & G_GATTRCOLOR))
        ap->color = WPU->gattr.color;
    if ((ap->flags & G_GATTRWIDTH))
        ap->width = WPU->gattr.width;
    if ((ap->flags & G_GATTRMODE))
        ap->mode = WPU->gattr.mode;
    if ((ap->flags & G_GATTRFILL))
        ap->fill = WPU->gattr.fill;
    if ((ap->flags & G_GATTRSTYLE))
        ap->style = WPU->gattr.style;
    return 0;
}

int GParrow (Gwidget_t *widget, Gpoint_t gp1, Gpoint_t gp2, Ggattr_t *ap) {
    PIXpoint_t pp1, pp2, pa, pb, pd;
    double tangent;
    int l;

    pp1 = pdrawtopix (widget, gp1), pp2 = pdrawtopix (widget, gp2);
    pd.x = pp1.x - pp2.x, pd.y = pp1.y - pp2.y;
    if (pd.x == 0 && pd.y == 0)
        return 0;
    tangent = atan2 ((double) pd.y, (double) pd.x);
    if ((l = sqrt ((double) (pd.x * pd.x + pd.y * pd.y))) < 30)
        l = 30;
    pa.x = l * cos (tangent + M_PI / 7) + pp2.x;
    pa.y = l * sin (tangent + M_PI / 7) + pp2.y;
    pb.x = l * cos (tangent - M_PI / 7) + pp2.x;
    pb.y = l * sin (tangent - M_PI / 7) + pp2.y;
    setgattr (widget, ap);
    fprintf (FP, "%d %d %d %d LI\n",
            (int) pp2.x, (int) pp2.y, (int) pp1.x, (int) pp1.y);
    fprintf (FP, "%d %d %d %d LI\n",
            (int) pp2.x, (int) pp2.y, (int) pa.x, (int) pa.y);
    fprintf (FP, "%d %d %d %d LI\n",
            (int) pp2.x, (int) pp2.y, (int) pb.x, (int) pb.y);
    return 0;
}

int GPline (Gwidget_t *widget, Gpoint_t gp1, Gpoint_t gp2, Ggattr_t *ap) {
    PIXpoint_t pp1, pp2;

    pp1 = pdrawtopix (widget, gp1), pp2 = pdrawtopix (widget, gp2);
    if (count++ == 100)
        count = 0, fprintf (FP, "DO\n");
    setgattr (widget, ap);
    fprintf (FP, "%d %d %d %d LI\n",
            (int) pp2.x, (int) pp2.y, (int) pp1.x, (int) pp1.y);
    return 0;
}

int GPbox (Gwidget_t *widget, Grect_t gr, Ggattr_t *ap) {
    PIXrect_t pr;

    pr = rdrawtopix (widget, gr);
    setgattr (widget, ap);
    if (WPU->gattr.fill)
        fprintf (FP, "DO %d %d %d %d BOX FI\n",
                (int) pr.o.x, (int) pr.o.y, (int) pr.c.x, (int) pr.c.y);
    else
        fprintf (FP, "DO %d %d %d %d BOX DO\n",
                (int) pr.o.x, (int) pr.o.y, (int) pr.c.x, (int) pr.c.y);
    return 0;
}

int GPpolygon (Gwidget_t *widget, int gpn, Gpoint_t *gpp, Ggattr_t *ap) {
    PIXpoint_t pp;
    int i;

    if (gpn == 0)
        return 0;

    pp = pdrawtopix (widget, gpp[0]);
    setgattr (widget, ap);
    fprintf (FP, "DO %d %d moveto\n", (int) pp.x, (int) pp.y);
    for (i = 1; i < gpn; i++) {
        pp = pdrawtopix (widget, gpp[i]);
        fprintf (FP, "%d %d lineto\n", (int) pp.x, (int) pp.y);
    }
    if (WPU->gattr.fill)
        fprintf (FP, "FI\n");
    else
        fprintf (FP, "DO\n");
    return 0;
}

int GPsplinegon (Gwidget_t *widget, int gpn, Gpoint_t *gpp, Ggattr_t *ap) {
    PIXpoint_t p0, p1, p2, p3;
    int i;

    if (gpn == 0)
        return 0;

    p0 = pdrawtopix (widget, gpp[0]);
    setgattr (widget, ap);
    fprintf (FP, "DO %d %d moveto\n", (int) p0.x, (int) p0.y);
    for (i = 1; i < gpn; i += 3) {
        p1 = pdrawtopix (widget, gpp[i]);
        p2 = pdrawtopix (widget, gpp[i + 1]);
        p3 = pdrawtopix (widget, gpp[i + 2]);
        fprintf (FP, "%d %d %d %d %d %d CT\n", (int) p1.x, (int) p1.y,
                (int) p2.x, (int) p2.y, (int) p3.x, (int) p3.y);
    }
    if (WPU->gattr.fill)
        fprintf (FP, "FI\n");
    else
        fprintf (FP, "DO\n");
    return 0;
}

int GParc (Gwidget_t *widget, Gpoint_t gc, Gsize_t gs,
        double ang1, double ang2, Ggattr_t *ap) {
    PIXpoint_t pc;
    PIXsize_t ps;

    pc = pdrawtopix (widget, gc), ps = sdrawtopix (widget, gs);
    setgattr (widget, ap);
    if (WPU->gattr.fill)
        fprintf (FP, "DO %d %d %f %d %f %f ARF\n",
                pc.x, pc.y, (double) ps.y / (double) ps.x, ps.x, ang1, ang2);
    else
        fprintf (FP, "DO %d %d %f %d %f %f AR\n",
                pc.x, pc.y, (double) ps.y / (double) ps.x, ps.x, ang1, ang2);
    return 0;
}

int GPtext (Gwidget_t *widget, Gtextline_t *tlp, int n, Gpoint_t go, char *fn,
        double fs, char *justs, Ggattr_t *ap) {
    Gsize_t gs;
    PIXpoint_t po;
    PIXsize_t ps;
    char *font;
    char c;
    int i;

    po = pdrawtopix (widget, go);
    gs.x = 0, gs.y = fs;
    ps = sdrawtopix (widget, gs);
    font = findfont (fn);
    setgattr (widget, ap);
    fprintf (FP, "DO %d (%c) %d (%c) [",
            (int) po.x, justs[0], (int) po.y, justs[1]);
    for (i = 0; i < n; i++) {
        c = tlp[i].p[tlp[i].n], tlp[i].p[tlp[i].n] = '\000';
        fprintf (FP, "[ (%s) (%c) ] ", tlp[i].p, tlp[i].j);
        tlp[i].p[tlp[i].n] = c;
    }
    fprintf (FP, "] %d /%s %d TXT\n", n, font, (int) ps.y);
    return 0;
}

static char *findfont (char *name) {
    char *font;

    if (name[0] == '\000' || Strcmp (name, "default") == 0)
        font = "Times-Roman";
    else
        font = name;
    return font;
}

static void setgattr (Gwidget_t *widget, Ggattr_t *ap) {
    int color, width, style;

    if (!(ap->flags & G_GATTRCOLOR))
        ap->color = WPU->defgattr.color;
    if (!(ap->flags & G_GATTRWIDTH))
        ap->width = WPU->defgattr.width;
    if (!(ap->flags & G_GATTRFILL))
        ap->fill = WPU->defgattr.fill;
    if (!(ap->flags & G_GATTRSTYLE))
    ap->style = WPU->defgattr.style;
    color = ap->color;
    if (color >= G_MAXCOLORS || !(WPU->colors[color].inuse))
        color = 1;
    if (color != WPU->gattr.color) {
        WPU->gattr.color = color;
        fprintf (FP, "%f %f %f CL\n", RED, GREEN, BLUE);
    }
    width = ap->width;
    if (width != WPU->gattr.width) {
        WPU->gattr.width = width;
        fprintf (FP, "DO %d setlinewidth NP\n", width);
    }
    WPU->gattr.fill = ap->fill;
    style = ap->style;
    if (style != WPU->gattr.style) {
        WPU->gattr.style = style;
        fprintf (FP, "DO [ %s ] 0 setdash NP\n", gstyles[style]);
    }
}

static PIXrect_t rdrawtopix (Gwidget_t *widget, Grect_t gr) {
    PIXrect_t pr;
    double tvx, tvy, twx, twy;

    tvx = WPU->vsize.x, tvy = WPU->vsize.y;
    twx = WPU->wrect.c.x - WPU->wrect.o.x;
    twy = WPU->wrect.c.y - WPU->wrect.o.y;
    pr.o.x = tvx * (gr.o.x - WPU->wrect.o.x) / twx + 0.5;
    pr.o.y = tvy * (gr.o.y - WPU->wrect.o.y) / twy + 0.5;
    pr.c.x = tvx * (gr.c.x - WPU->wrect.o.x) / twx + 0.5;
    pr.c.y = tvy * (gr.c.y - WPU->wrect.o.y) / twy + 0.5;
    return pr;
}

static PIXpoint_t pdrawtopix (Gwidget_t *widget, Gpoint_t gp) {
    PIXpoint_t pp;
    double tvx, tvy, twx, twy;

    tvx = WPU->vsize.x, tvy = WPU->vsize.y;
    twx = WPU->wrect.c.x - WPU->wrect.o.x;
    twy = WPU->wrect.c.y - WPU->wrect.o.y;
    pp.x = tvx * (gp.x - WPU->wrect.o.x) / twx + 0.5;
    pp.y = tvy * (gp.y - WPU->wrect.o.y) / twy + 0.5;
    return pp;
}

static PIXsize_t sdrawtopix (Gwidget_t *widget, Gsize_t gs) {
    PIXsize_t ps;
    double tvx, tvy, twx, twy;

    tvx = WPU->vsize.x, tvy = WPU->vsize.y;
    twx = WPU->wrect.c.x - WPU->wrect.o.x;
    twy = WPU->wrect.c.y - WPU->wrect.o.y;
    ps.x = tvx * gs.x / twx + 0.5;
    ps.y = tvy * gs.y / twy + 0.5;
    return ps;
}
