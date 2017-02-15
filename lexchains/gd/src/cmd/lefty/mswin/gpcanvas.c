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

#define WPU widget->u.p
#define WINDOW widget->u.p->window
#define GC widget->u.p->gc

static long gstyles[5] = {
    /* G_SOLID */       PS_SOLID,
    /* G_DASHED */      PS_DASH,
    /* G_DOTTED */      PS_DOT,
    /* G_LONGDASHED */  PS_DASH,
    /* G_SHORTDASHED */ PS_DASH,
};

static char MSNEAR grays[][4] = {
  { 0x00,0x00,0x00,0x00, },
  { 0x08,0x00,0x00,0x00, },
  { 0x08,0x00,0x02,0x00, },
  { 0x0A,0x00,0x02,0x00, },
  { 0x0A,0x00,0x0A,0x00, },
  { 0x0A,0x04,0x0A,0x00, },
  { 0x0A,0x04,0x0A,0x01, },
  { 0x0A,0x05,0x0A,0x01, },
  { 0x0A,0x05,0x0A,0x05, },
  { 0x0E,0x05,0x0A,0x05, },
  { 0x0E,0x05,0x0B,0x05, },
  { 0x0F,0x05,0x0B,0x05, },
  { 0x0F,0x05,0x0F,0x05, },
  { 0x0F,0x0D,0x0F,0x05, },
  { 0x0F,0x0D,0x0F,0x07, },
  { 0x0F,0x0F,0x0F,0x07, },
  { 0x0F,0x0F,0x0F,0x0F, },
};

char *Gpscanvasname = "out.ps";

static void bezier (PIXpoint_t, PIXpoint_t, PIXpoint_t, PIXpoint_t);
static HFONT findfont (char *, int);
static void setgattr (Gwidget_t *, Ggattr_t *);

static PIXrect_t  rdrawtopix (Gwidget_t *, Grect_t);
static PIXpoint_t pdrawtopix (Gwidget_t *, Gpoint_t);
static PIXsize_t  sdrawtopix (Gwidget_t *, Gsize_t);

int GPcreatewidget (Gwidget_t *parent, Gwidget_t *widget,
        int attrn, Gwattr_t *attrp) {
    PIXpoint_t po;
    PIXsize_t ps;
    PRINTDLG pd;
    DOCINFO di;
    DEVMODE *dmp;
    LOGPALETTE pal[2]; /* the 2 here is to provide enough space
                          for palPalEntry[0] and [1] */
    HBRUSH brush;
    HPEN pen;
    HBITMAP bmap;
    char *s;
    int color, lflag, ai, dpix, dpiy, i, r, g, b;

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
    di.cbSize = sizeof (DOCINFO);
    di.lpszDocName = NULL;
    di.lpszOutput = NULL;
    pd.lStructSize = sizeof (pd);
    pd.hwndOwner = NULL;
    pd.hDevMode = NULL;
    pd.hDevNames = NULL;
    pd.hDC = NULL;
    pd.Flags = PD_RETURNDC | PD_RETURNDEFAULT;
    pd.nFromPage = 0;
    pd.nToPage = 0;
    pd.nMinPage = 0;
    pd.nMaxPage = 0;
    pd.nCopies = 0;
    pd.hInstance = NULL;
    pd.lCustData = NULL;
    pd.lpfnPrintHook = NULL;
    pd.lpfnSetupHook = NULL;
    pd.lpPrintTemplateName = NULL;
    pd.lpSetupTemplateName = NULL;
    pd.hPrintTemplate = NULL;
    pd.hSetupTemplate = NULL;
    if (!PrintDlg (&pd)) {
        Gerr (POS, G_ERRCANNOTCREATE);
        return -1;
    }
    if (lflag && pd.hDevMode) {
        dmp = (DEVMODE *) GlobalLock (pd.hDevMode);
        dmp->dmOrientation = DMORIENT_LANDSCAPE;
        GlobalUnlock (pd.hDevMode);
        pd.Flags = PD_RETURNDC;
        if (!PrintDlg (&pd)) {
            Gerr (POS, G_ERRCANNOTCREATE);
            return -1;
        }
    }
    GC = pd.hDC;
    dpix = GetDeviceCaps (GC, LOGPIXELSX);
    if (dpix != 300)
        ps.x = ps.x * (double) dpix / 300.0;
    dpiy = GetDeviceCaps (GC, LOGPIXELSY);
    if (dpiy != 300)
        ps.y = ps.y * (double) dpiy / 300.0;
    if (StartDoc (GC, &di) <= 0 || StartPage (GC) <= 0) {
        Gerr (POS, G_ERRCANNOTCREATE);
        return -1;
    }
    WPU->wrect.o.x = 0.0, WPU->wrect.o.y = 0.0;
    WPU->wrect.c.x = 1.0, WPU->wrect.c.y = 1.0;
    WPU->vsize.x = ps.x, WPU->vsize.y = ps.y;
    WPU->ncolor = 2;
    pal[0].palVersion = 0x300; /* HA HA HA */
    pal[0].palNumEntries = 2;
    pal[0].palPalEntry[0].peRed = 255;
    pal[0].palPalEntry[0].peGreen = 255;
    pal[0].palPalEntry[0].peBlue = 255;
    pal[0].palPalEntry[0].peFlags = 0;
    pal[0].palPalEntry[1].peRed = 0;
    pal[0].palPalEntry[1].peGreen = 0;
    pal[0].palPalEntry[1].peBlue = 0;
    pal[0].palPalEntry[1].peFlags = 0;
    WPU->cmap = CreatePalette (&pal[0]);
    WPU->colors[0].color = pal[0].palPalEntry[0];
    for (i = 1; i < G_MAXCOLORS; i++)
        WPU->colors[i].color = pal[0].palPalEntry[1];
    SelectPalette (GC, WPU->cmap, FALSE);
    RealizePalette (GC);
    WPU->colors[0].inuse = TRUE;
    WPU->colors[1].inuse = TRUE;
    for (i = 2; i < G_MAXCOLORS; i++)
        WPU->colors[i].inuse = FALSE;
    WPU->gattr.color = 1;
    brush = CreateSolidBrush (PALETTEINDEX (1));
    SelectObject (GC, brush);
    pen = CreatePen (PS_SOLID, 1, PALETTEINDEX (1));
    SelectObject (GC, pen);
    SetTextColor (GC, PALETTEINDEX (1));
    SetBkMode (GC, TRANSPARENT);
    WPU->gattr.width = 0;
    WPU->gattr.mode = G_SRC;
    WPU->gattr.fill = 0;
    WPU->gattr.style = 0;
    WPU->defgattr = WPU->gattr;
    WPU->font = NULL;
    if (Gdepth == 1) {
        for (i = 0; i < 17; i++) {
            if (!(bmap = CreateBitmap (4, 4, 1, 1, &grays[i][0])))
                continue;
            WPU->grays[i] = CreatePatternBrush (bmap);
        }
    }
    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRCOLOR:
            color = attrp[ai].u.c.index;
            if (color < 0 || color > G_MAXCOLORS) {
                Gerr (POS, G_ERRBADCOLORINDEX, color);
                return -1;
            }
            r = attrp[ai].u.c.r * 257;
            g = attrp[ai].u.c.g * 257;
            b = attrp[ai].u.c.b * 257;
            WPU->colors[color].color.peRed = r;
            WPU->colors[color].color.peGreen = g;
            WPU->colors[color].color.peBlue = b;
            WPU->colors[color].color.peFlags = 0;
            if (color >= WPU->ncolor)
                ResizePalette (WPU->cmap, color + 1), WPU->ncolor = color + 1;
            SetPaletteEntries (WPU->cmap, (int) color, 1,
                    &WPU->colors[color].color);
            RealizePalette (GC);
            WPU->colors[color].inuse = TRUE;
            if (color == WPU->gattr.color)
                WPU->gattr.color = -1;
            break;
        case G_ATTRVIEWPORT:
            if (attrp[ai].u.s.x == 0)
                attrp[ai].u.s.x = 1;
            if (attrp[ai].u.s.y == 0)
                attrp[ai].u.s.y = 1;
            WPU->vsize.x = attrp[ai].u.s.x + 0.5;
            WPU->vsize.y = attrp[ai].u.s.y + 0.5;
            SetWindowPos (widget->w, (HWND) NULL, 0, 0, WPU->vsize.x,
                    WPU->vsize.y, SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOMOVE);
            break;
        case G_ATTRWINDOW:
            if (attrp[ai].u.r.o.x == attrp[ai].u.r.c.x)
                attrp[ai].u.r.c.x = attrp[ai].u.r.o.x + 1;
            if (attrp[ai].u.r.o.y == attrp[ai].u.r.c.y)
                attrp[ai].u.r.c.y = attrp[ai].u.r.o.y + 1;
            WPU->wrect = attrp[ai].u.r;
            break;
        }
    }
    return 0;
}

int GPsetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
    PIXsize_t ps;
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
                Gerr (POS, G_ERRBADCOLORINDEX, color);
                return -1;
            }
            r = attrp[ai].u.c.r * 257;
            g = attrp[ai].u.c.g * 257;
            b = attrp[ai].u.c.b * 257;
            WPU->colors[color].color.peRed = r;
            WPU->colors[color].color.peGreen = g;
            WPU->colors[color].color.peBlue = b;
            WPU->colors[color].color.peFlags = 0;
            if (color >= WPU->ncolor)
                ResizePalette (WPU->cmap, color + 1), WPU->ncolor = color + 1;
            SetPaletteEntries (WPU->cmap, (int) color, 1,
                    &WPU->colors[color].color);
            RealizePalette (GC);
            WPU->colors[color].inuse = TRUE;
            if (color == WPU->gattr.color)
                WPU->gattr.color = -1;
            break;
        case G_ATTRVIEWPORT:
            if (attrp[ai].u.s.x == 0)
                attrp[ai].u.s.x = 1;
            if (attrp[ai].u.s.y == 0)
                attrp[ai].u.s.y = 1;
            WPU->vsize.x = attrp[ai].u.s.x + 0.5;
            WPU->vsize.y = attrp[ai].u.s.y + 0.5;
            ps.x = WPU->vsize.x, ps.y = WPU->vsize.y;
            break;
        case G_ATTRWINDOW:
            if (attrp[ai].u.r.o.x == attrp[ai].u.r.c.x)
                attrp[ai].u.r.c.x = attrp[ai].u.r.o.x + 1;
            if (attrp[ai].u.r.o.y == attrp[ai].u.r.c.y)
                attrp[ai].u.r.c.y = attrp[ai].u.r.o.y + 1;
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
    PALETTEENTRY *cp;
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
            if (color < 0 || color > G_MAXCOLORS) {
                Gerr (POS, G_ERRBADCOLORINDEX, color);
                return -1;
            }
            if (WPU->colors[color].inuse) {
                cp = &WPU->colors[color].color;
                attrp[ai].u.c.r = cp->peRed / 257.0;
                attrp[ai].u.c.g = cp->peGreen / 257.0;
                attrp[ai].u.c.b = cp->peBlue / 257.0;
            } else {
                attrp[ai].u.c.r = -1;
                attrp[ai].u.c.g = -1;
                attrp[ai].u.c.b = -1;
            }
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
    EndPage (GC);
    EndDoc (GC);
    return 0;
}

int GPcanvasclear (Gwidget_t *widget) {
    HBRUSH brush, pbrush;

    /* FIXME: drain repaint messages */
    brush = CreateSolidBrush (PALETTEINDEX (0));
    pbrush = SelectObject (GC, brush);
    Rectangle (GC, 0, 0, (int) WPU->vsize.x, (int) WPU->vsize.y);
    SelectObject (GC, pbrush);
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
    double tangent, l;

    pp1 = pdrawtopix (widget, gp1), pp2 = pdrawtopix (widget, gp2);
    pd.x = pp1.x - pp2.x, pd.y = pp1.y - pp2.y;
    if (pd.x == 0 && pd.y == 0)
        return 0;
    tangent = atan2 ((double) pd.y, (double) pd.x);
    if ((l = sqrt ((double) (pd.x * pd.x + pd.y * pd.y))) > 30)
        l = 30;
    pa.x = l * cos (tangent + M_PI / 7) + pp2.x;
    pa.y = l * sin (tangent + M_PI / 7) + pp2.y;
    pb.x = l * cos (tangent - M_PI / 7) + pp2.x;
    pb.y = l * sin (tangent - M_PI / 7) + pp2.y;
    setgattr (widget, ap);
    MoveTo (GC, pp1.x, pp1.y), LineTo (GC, pp2.x, pp2.y);
    MoveTo (GC, pa.x, pa.y), LineTo (GC, pp2.x, pp2.y);
    MoveTo (GC, pb.x, pb.y), LineTo (GC, pp2.x, pp2.y);
    return 0;
}

int GPline (Gwidget_t *widget, Gpoint_t gp1, Gpoint_t gp2, Ggattr_t *ap) {
    PIXpoint_t pp1, pp2;

    pp1 = pdrawtopix (widget, gp1), pp2 = pdrawtopix (widget, gp2);
    setgattr (widget, ap);
    MoveTo (GC, pp1.x, pp1.y);
    LineTo (GC, pp2.x, pp2.y);
    return 0;
}

int GPbox (Gwidget_t *widget, Grect_t gr, Ggattr_t *ap) {
    PIXrect_t pr;
    Grect_t gr2;

    if (gr.o.x <= gr.c.x)
        gr2.o.x = gr.o.x, gr2.c.x = gr.c.x;
    else
        gr2.o.x = gr.c.x, gr2.c.x = gr.o.x;
    if (gr.o.y <= gr.c.y)
        gr2.o.y = gr.o.y, gr2.c.y = gr.c.y;
    else
        gr2.o.y = gr.c.y, gr2.c.y = gr.o.y;
    pr = rdrawtopix (widget, gr);
    setgattr (widget, ap);
    if (WPU->gattr.fill)
        Rectangle (GC, pr.o.x, pr.o.y, pr.c.x, pr.c.y);
    else {
        Gppp[0].x = pr.o.x, Gppp[0].y = pr.o.y;
        Gppp[1].x = pr.c.x, Gppp[1].y = pr.o.y;
        Gppp[2].x = pr.c.x, Gppp[2].y = pr.c.y;
        Gppp[3].x = pr.o.x, Gppp[3].y = pr.c.y;
        Gppp[4].x = pr.o.x, Gppp[4].y = pr.o.y;
        Polyline (GC, Gppp, 5);
    }
    return 0;
}

int GPpolygon (Gwidget_t *widget, int gpn, Gpoint_t *gpp, Ggattr_t *ap) {
    int n, i;

    if (gpn == 0)
        return 0;
    if (gpn + 1 > Gppn) {
        n = (((gpn + 1) + PPINCR - 1) / PPINCR) * PPINCR;
        Gppp = Marraygrow (Gppp, (long) n * PPSIZE);
        Gppn = n;
    }
    for (i = 0; i < gpn; i++)
        Gppp[i] = pdrawtopix (widget, gpp[i]);
    setgattr (widget, ap);
    if (WPU->gattr.fill) {
        if (Gppp[gpn - 1].x != Gppp[0].x || Gppp[gpn - 1].y != Gppp[0].y)
            Gppp[gpn] = Gppp[0], gpn++;
        Polygon (GC, Gppp, (int) gpn);
    } else
        Polyline (GC, Gppp, (int) gpn);
    return 0;
}

int GPsplinegon (Gwidget_t *widget, int gpn, Gpoint_t *gpp, Ggattr_t *ap) {
    PIXpoint_t p0, p1, p2, p3;
    int n, i;

    if (gpn == 0)
        return 0;
    Gppi = 1;
    if (Gppi >= Gppn) {
        n = (((Gppi + 1) + PPINCR - 1) / PPINCR) * PPINCR;
        Gppp = Marraygrow (Gppp, (long) n * PPSIZE);
        Gppn = n;
    }
    Gppp[0] = p3 = pdrawtopix (widget, gpp[0]);
    for (i = 1; i < gpn; i += 3) {
        p0 = p3;
        p1 = pdrawtopix (widget, gpp[i]);
        p2 = pdrawtopix (widget, gpp[i + 1]);
        p3 = pdrawtopix (widget, gpp[i + 2]);
        bezier (p0, p1, p2, p3);
    }
    setgattr (widget, ap);
    if (WPU->gattr.fill) {
        if (Gppp[Gppi - 1].x != Gppp[0].x || Gppp[Gppi - 1].y != Gppp[0].y)
            Gppp[Gppi] = Gppp[0], Gppi++;
        Polygon (GC, Gppp, (int) Gppi);
    } else
        Polyline (GC, Gppp, (int) Gppi);
    return 0;
}

static void bezier (PIXpoint_t p0, PIXpoint_t p1,
        PIXpoint_t p2, PIXpoint_t p3) {
    Gpoint_t gp0, gp1, gp2;
    Gsize_t s;
    PIXpoint_t p;
    double t;
    int n, i, steps;

    if ((s.x = p3.x - p0.x) < 0)
        s.x = - s.x;
    if ((s.y = p3.y - p0.y) < 0)
        s.y = - s.y;
    if (s.x > s.y)
        steps = s.x / 5 + 1;
    else
        steps = s.y / 5 + 1;
    for (i = 0; i <= steps; i++) {
        t = i / (double) steps;
        gp0.x = p0.x + t * (p1.x - p0.x);
        gp0.y = p0.y + t * (p1.y - p0.y);
        gp1.x = p1.x + t * (p2.x - p1.x);
        gp1.y = p1.y + t * (p2.y - p1.y);
        gp2.x = p2.x + t * (p3.x - p2.x);
        gp2.y = p2.y + t * (p3.y - p2.y);
        gp0.x = gp0.x + t * (gp1.x - gp0.x);
        gp0.y = gp0.y + t * (gp1.y - gp0.y);
        gp1.x = gp1.x + t * (gp2.x - gp1.x);
        gp1.y = gp1.y + t * (gp2.y - gp1.y);
        p.x = gp0.x + t * (gp1.x - gp0.x) + 0.5;
        p.y = gp0.y + t * (gp1.y - gp0.y) + 0.5;
        if (Gppi >= Gppn) {
            n = (((Gppi + 1) + PPINCR - 1) / PPINCR) * PPINCR;
            Gppp = Marraygrow (Gppp, (long) n * PPSIZE);
            Gppn = n;
        }
        Gppp[Gppi++] = p;
    }
}

int GParc (Gwidget_t *widget, Gpoint_t gc, Gsize_t gs,
        double ang1, double ang2, Ggattr_t *ap) {
    PIXpoint_t pc;
    PIXsize_t ps;
    double a1, a2;

    pc = pdrawtopix (widget, gc), ps = sdrawtopix (widget, gs);
    setgattr (widget, ap);
    a1 = ang1 * M_PI / 180, a2 = ang2 * M_PI / 180;
    if (WPU->gattr.fill)
        Chord (GC, pc.x - ps.x, pc.y - ps.y, pc.x + ps.x, pc.y + ps.y,
                (int) (cos (a1) * ps.x), (int) (sin (a1) * ps.x),
                (int) (cos (a2) * ps.x), (int) (sin (a2) * ps.x));
    else
        Arc (GC, pc.x - ps.x, pc.y - ps.y, pc.x + ps.x, pc.y + ps.y,
                (int) (cos (a1) * ps.x), (int) (sin (a1) * ps.x),
                (int) (cos (a2) * ps.x), (int) (sin (a2) * ps.x));
    return 0;
}

int GPtext (Gwidget_t *widget, Gtextline_t *tlp, int n, Gpoint_t go, char *fn,
        double fs, char *justs, Ggattr_t *ap) {
    Gsize_t gs;
    PIXpoint_t po;
    PIXsize_t ps;
    PIXrect_t pr;
    HFONT font;
    DWORD size;
    RECT r;
    int x, y, w, h, i;

    po = pdrawtopix (widget, go);
    gs.x = 0, gs.y = fs;
    ps = sdrawtopix (widget, gs);
    if (!(font = findfont (fn, ps.y))) {
        Rectangle (GC, po.x, po.y, po.x + 1, po.y + 1);
        return 0;
    }
    setgattr (widget, ap);
    if (font != WPU->font) {
        WPU->font = font;
        SelectObject (GC, font);
    }
    for (w = h = 0, i = 0; i < n; i++) {
        if (tlp[i].n)
            size = GetTextExtent (GC, tlp[i].p, (int) tlp[i].n);
        else
            size = GetTextExtent (GC, "M", (int) 1);
        tlp[i].w = LOWORD (size), tlp[i].h = HIWORD (size);
        w = max (w, LOWORD (size)), h += HIWORD (size);
    }
    switch (justs[0]) {
    case 'l': po.x += w / 2; break;
    case 'r': po.x -= w / 2; break;
    }
    switch (justs[1]) {
    case 'd': po.y -= h; break;
    case 'c': po.y -= h / 2; break;
    }
    pr.o.x = po.x - w / 2, pr.o.y = po.y;
    pr.c.x = po.x + w / 2, pr.c.y = po.y + h;
    for (i = 0; i < n; i++) {
        switch (tlp[i].j) {
        case 'l': x = po.x - w / 2; break;
        case 'n': x = po.x - tlp[i].w / 2; break;
        case 'r': x = po.x - (tlp[i].w - w / 2); break;
        }
        y = po.y + i * tlp[i].h;
        r.left = x, r.top = y;
        r.right = x + tlp[i].w, r.bottom = y + tlp[i].h;
        DrawText (GC, tlp[i].p, (int) tlp[i].n, &r, DT_LEFT | DT_TOP);
    }
    return 0;
}

static HFONT findfont (char *name, int size) {
    HFONT font;
    int fi;

    if (name[0] == '\000')
        return Gfontp[0].font;

    sprintf (&Gbufp[0], name, size);
    for (fi = 0; fi < Gfontn; fi++)
        if (Strcmp (&Gbufp[0], Gfontp[fi].name) == 0 && Gfontp[fi].size == size)
            return Gfontp[fi].font;
    font = CreateFont ((int) size,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &Gbufp[0]);
    if (!font)
        font = Gfontp[0].font;

    Gfontp = Marraygrow (Gfontp, (long) (Gfontn + 1) * FONTSIZE);
    Gfontp[Gfontn].name = strdup (&Gbufp[0]);
    Gfontp[Gfontn].size = size;
    Gfontp[Gfontn].font = font;
    Gfontn++;
    return font;
}

static void setgattr (Gwidget_t *widget, Ggattr_t *ap) {
    HBRUSH brush, pbrush;
    HPEN pen, ppen;
    PALETTEENTRY *colorp;
    long color, mode, style, width, flag, pati;
    double intens;

    if (!(ap->flags & G_GATTRCOLOR))
        ap->color = WPU->defgattr.color;
    if (!(ap->flags & G_GATTRWIDTH))
        ap->width = WPU->defgattr.width;
    if (!(ap->flags & G_GATTRMODE))
        ap->mode = WPU->defgattr.mode;
    if (!(ap->flags & G_GATTRFILL))
        ap->fill = WPU->defgattr.fill;
    if (!(ap->flags & G_GATTRSTYLE))
        ap->style = WPU->defgattr.style;
    flag = FALSE;
    mode = ap->mode;
    if (mode != WPU->gattr.mode) {
        WPU->gattr.mode = mode;
        SetROP2 (GC, (int) mode);
    }
    WPU->gattr.fill = ap->fill;
    color = ap->color;
    if (color >= G_MAXCOLORS || !(WPU->colors[color].inuse))
        color = 1;
    if (color != WPU->gattr.color)
        WPU->gattr.color = color, flag = TRUE;
    width = ap->width;
    if (width != WPU->gattr.width)
        WPU->gattr.width = width, flag = TRUE;
    style = ap->style;
    if (style != WPU->gattr.style)
        WPU->gattr.style = style, style = TRUE;

    if (!flag)
        return;
    WPU->gattr.color = color;
    if (Gdepth == 1) {
        colorp = &WPU->colors[color].color;
        intens = (0.3 * colorp->peBlue + 0.59 * colorp->peRed +
                0.11 * colorp->peGreen) / 255.0;
        pati = (intens <= 0.0625) ? 16 : -16.0 * (log (intens) / 2.7725887222);
        brush = WPU->grays[pati];
    } else
        brush = CreateSolidBrush (PALETTEINDEX (WPU->gattr.color));
    pbrush = SelectObject (GC, brush);
    if (Gdepth != 1)
        DeleteObject (pbrush);
    pen = CreatePen ((int) gstyles[WPU->gattr.style], WPU->gattr.width,
            PALETTEINDEX (WPU->gattr.color));
    ppen = SelectObject (GC, pen);
    DeleteObject (ppen);
    SetTextColor (GC, PALETTEINDEX (WPU->gattr.color));
}

static PIXrect_t rdrawtopix (Gwidget_t *widget, Grect_t gr) {
    PIXrect_t pr;
    double tvx, tvy, twx, twy;

    tvx = WPU->vsize.x, tvy = WPU->vsize.y;
    twx = WPU->wrect.c.x - WPU->wrect.o.x;
    twy = WPU->wrect.c.y - WPU->wrect.o.y;
    pr.o.x = tvx * (gr.o.x - WPU->wrect.o.x) / twx + 0.5;
    pr.o.y = tvy * (1.0  - (gr.c.y - WPU->wrect.o.y) / twy) + 0.5;
    pr.c.x = tvx * (gr.c.x - WPU->wrect.o.x) / twx + 0.5;
    pr.c.y = tvy * (1.0  - (gr.o.y - WPU->wrect.o.y) / twy) + 0.5;
    return pr;
}

static PIXpoint_t pdrawtopix (Gwidget_t *widget, Gpoint_t gp) {
    PIXpoint_t pp;
    double tvx, tvy, twx, twy;

    tvx = WPU->vsize.x, tvy = WPU->vsize.y;
    twx = WPU->wrect.c.x - WPU->wrect.o.x;
    twy = WPU->wrect.c.y - WPU->wrect.o.y;
    pp.x = tvx * (gp.x - WPU->wrect.o.x) / twx + 0.5;
    pp.y = tvy * (1.0  - (gp.y - WPU->wrect.o.y) / twy) + 0.5;
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
