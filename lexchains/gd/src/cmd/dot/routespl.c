/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dot.h"
	/* some of the following must be dispensible, see constants.h */
#ifndef INT_MAX
#define INT_MAX MAXINT
#define INT_MIN (-INT_MAX-1)
#endif

#define EPSILON 1e-6
#define MARGIN 3
#define MINSCALE 10
#define MAXSCALE 20

typedef struct XY {
    double x, y;
} XY;

typedef struct Spline {
    XY p[4];
    double scale0, scale3;
    int steps;
} Spline;

static box *bs = NULL;
static int bn;
static int maxbn = 0;
#define BINC 300

static point *ps = NULL;
static int pn;
static int maxpn = 0;
#define PINC 300

typedef struct b2bline {
    XY lp[2], p, a[2];
    double margin;
    int piscp, rating;
    double t;
} b2bline;
static b2bline *ls = NULL;
static int ln;
static int maxln = 0;
#define LINC 300

static box *boxes;
static int boxn;
static XY ftang, ltang;

static box minbbox;

static int cpn;
static int maxrating;

static edge_t *origedge, *realedge;
static path *thepath;

static int nedges, nboxes, nsplines;

void routesplinesinit ();
void routesplinesterm ();
point *routesplines ();

static void checkpath ();
static void computeb2bline ();
static void fitlines ();
static void rate ();
static void fitsplines ();
static int linefits ();
static int splinefits ();
static void computelinesplit ();
static void computesplinesplit ();
static void addspline ();
static Spline mkspline ();
static int intersectsbox ();
static int intersectsb2bline ();
static int onb2bline ();
static int online ();
static void add2bbox ();
static void parametrize ();
static XY linepoint ();
static XY splinepoint ();

static int ccw ();
static XY adjusttangent ();
static XY norm ();
static XY add ();
static XY sub ();
static double dist ();
static XY scale ();
static double dot ();
static double B1 ();
static double B2 ();
static double B01 ();
static double B23 ();

static void mkspaceb ();
static void mkspacep ();
static void mkspacel ();

static void printpath ();
static void printpsboxes ();

void routesplinesinit () {
    if (!(bs = (box *) malloc (BINC * sizeof (box)))) {
        fprintf (stderr, "cannot allocate bs\n");
        abort ();
    }
    maxbn = BINC;
    if (!(ps = (point *) malloc (PINC * sizeof (point)))) {
        fprintf (stderr, "cannot allocate ps\n");
        abort ();
    }
    maxpn = PINC;
    if (!(ls = (b2bline *) malloc (LINC * sizeof (b2bline)))) {
        fprintf (stderr, "cannot allocate ls\n");
        abort ();
    }
    maxln = LINC;
    minbbox.LL.x = minbbox.LL.y = INT_MAX;
    minbbox.UR.x = minbbox.UR.y = INT_MIN;
    Show_boxes = FALSE;
	if (Verbose) start_timer();
}

void routesplinesterm () {
    free (ls), ls = NULL, maxln = ln = 0;
    free (ps), ps = NULL, maxpn = pn = 0;
    free (bs), bs = NULL, maxbn = bn = 0;
    if (Verbose)
        fprintf (stderr, "routesplines: %d edges, %d boxes, %d splines %.2lf sec\n",
                nedges, nboxes, nsplines,elapsed_sec());
}

point *routesplines (pp, npoints)
    path *pp;
    int *npoints;
{
    point p1, p2, p3, p4;
    int pi, bi, li;

    nedges++;
    nboxes += pp->nbox;

    for (realedge = origedge = (edge_t *) pp->data;
            realedge && realedge->u.edge_type != NORMAL; realedge = realedge->u.to_orig)
        ;
    if (!realedge) {
        fprintf (stderr, "in routesplines, cannot find NORMAL edge\n");
        abort ();
    }
    thepath = pp;

    boxes = pp->boxes;
    boxn = pp->nbox;

    bn = 0;
    mkspaceb (boxn);
    bn = boxn;

    pn = 0;
    mkspacep (1);
    ps[0].x = pp->start.p.x, ps[0].y = pp->start.p.y;
    pn = 1;

    checkpath ();

    ln = 0;
    mkspacel (boxn + 1);
    ls[0].lp[0].x = ls[0].lp[1].x = pp->start.p.x;
    ls[0].lp[0].y = ls[0].lp[1].y = pp->start.p.y;
    ls[0].p = ls[0].lp[0];
    ls[0].margin = 0;
    ls[0].piscp = TRUE;
    for (li = 1; li < boxn; li++) {
        computeb2bline (li - 1, &ls[li]);
        ls[li].piscp = FALSE;
    }
    ls[boxn].lp[0].x = ls[boxn].lp[1].x = pp->end.p.x;
    ls[boxn].lp[0].y = ls[boxn].lp[1].y = pp->end.p.y;
    ls[boxn].p = ls[boxn].lp[0];
    ls[boxn].margin = 0;
    ls[boxn].piscp = TRUE;
    ln = boxn + 1;

    if (realedge->head->graph->u.showboxes == 1 ||
            realedge->tail->graph->u.showboxes == 1 ||
            realedge->u.showboxes == 1 ||
            realedge->head->u.showboxes == 1||
            realedge->tail->u.showboxes == 1)
        printpsboxes ();

    if (pp->start.constrained)
        ftang.x = cos (pp->start.theta), ftang.y = sin (pp->start.theta);
    else
        ftang.x = ftang.y = 0;
    if (pp->end.constrained)
        ltang.x = cos (pp->end.theta), ltang.y = sin (pp->end.theta);
    else
        ltang.x = ltang.y = 0;
    cpn = 2;
    fitlines (0, boxn - 1, cpn);
    rate (0, boxn, cpn);
    fitsplines (0, boxn - 1, cpn, ftang, ltang, pp->ulpp, pp->urpp, pp->llpp, pp->lrpp);
    if ((ftang.x == 0.0 && ftang.y == 0.0) && (pp->ulpp || pp->urpp)) {
        for (p1 = ps[0], pi = 1; pi < pn; pi++) {
            if (ps[pi].x != p1.x || ps[pi].y != p1.y) {
                p2 = ps[pi];
                break;
            }
        }
        if ((pp->ulpp && !ccw (p2, p1, *pp->ulpp)) || (pp->urpp && ccw (p2, p1, *pp->urpp))) {
            ftang = adjusttangent (pp->ulpp, pp->urpp, p1);
            if (ftang.y > 0)
                ftang.y = 0, ftang = norm (ftang);
            pn = 1;
            fitsplines (0, boxn - 1, cpn, ftang, ltang, pp->ulpp, pp->urpp, pp->llpp, pp->lrpp);
        }
    }
    if ((ltang.x == 0.0 && ltang.y == 0.0) && (pp->llpp || pp->lrpp)) {
        for (p3 = ps[pn - 1], pi = pn - 2; pi >= 0; pi--) {
            if (ps[pi].x != p3.x || ps[pi].y != p3.y) {
                p4 = ps[pi];
                break;
            }
        }
        if ((pp->llpp && !ccw (*pp->llpp, p3, p4)) || (pp->lrpp && ccw (*pp->lrpp, p3, p4))) {
            ltang = adjusttangent (pp->lrpp, pp->llpp, p3);
            if (ltang.y < 0)
                ltang.y = 0, ltang = norm (ltang);
            pn = 1;
            fitsplines (0, boxn - 1, cpn, ftang, ltang, pp->ulpp, pp->urpp, pp->llpp, pp->lrpp);
        }
    }
    for (bi = 0; bi < boxn; bi++)
        boxes[bi] = bs[bi];

    if (realedge->head->graph->u.showboxes == 2 ||
            realedge->tail->graph->u.showboxes == 2 ||
            realedge->u.showboxes == 2 ||
            realedge->head->u.showboxes == 2||
            realedge->tail->u.showboxes == 2)
        printpsboxes ();

    nsplines += (pn - 1) / 3;
    *npoints = pn;
    return ps;
}

static void checkpath ()
{
    box *ba, *bb;
    int bi, i, errs, l, r, d, u;

    ba = &boxes[0];
    if (ba->LL.x > ba->UR.x || ba->LL.y > ba->UR.y) {
        fprintf (stderr, "in checkpath, box 0 has LL coord > UR coord\n");
        printpath (thepath);
        abort ();
    }
    for (bi = 0; bi < boxn - 1; bi++) {
        ba = &boxes[bi], bb = &boxes[bi + 1];
        if (bb->LL.x > bb->UR.x || bb->LL.y > bb->UR.y) {
            fprintf (stderr, "in checkpath, box %d has LL coord > UR coord\n", bi + 1);
            printpath (thepath);
            abort ();
        }
        l = (ba->UR.x < bb->LL.x) ? 1 : 0;
        r = (ba->LL.x > bb->UR.x) ? 1 : 0;
        d = (ba->UR.y < bb->LL.y) ? 1 : 0;
        u = (ba->LL.y > bb->UR.y) ? 1 : 0;
        errs = l + r + d + u;
        if (errs > 0 && Verbose) {
            fprintf (stderr, "in checkpath, boxes %d and %d don't touch\n", bi, bi + 1);
            printpath (thepath);
        }
#ifndef DONTFIXPATH
        if (errs > 0) {
            int xy;

            if (l == 1)
                xy = ba->UR.x, ba->UR.x = bb->LL.x, bb->LL.x = xy, l = 0;
            else if (r == 1)
                xy = ba->LL.x, ba->LL.x = bb->UR.x, bb->UR.x = xy, r = 0;
            else if (d == 1)
                xy = ba->UR.y, ba->UR.y = bb->LL.y, bb->LL.y = xy, d = 0;
            else if (u == 1)
                xy = ba->LL.y, ba->LL.y = bb->UR.y, bb->UR.y = xy, u = 0;
            for (i = 0; i < errs - 1; i++) {
                if (l == 1)
                    xy = (ba->UR.x + bb->LL.x) / 2.0 + 0.5, ba->UR.x = bb->LL.x = xy, l = 0;
                else if (r == 1)
                    xy = (ba->LL.x + bb->UR.x) / 2.0 + 0.5, ba->LL.x = bb->UR.x = xy, r = 0;
                else if (d == 1)
                    xy = (ba->UR.y + bb->LL.y) / 2.0 + 0.5, ba->UR.y = bb->LL.y = xy, d = 0;
                else if (u == 1)
                    xy = (ba->LL.y + bb->UR.y) / 2.0 + 0.5, ba->LL.y = bb->UR.y = xy, u = 0;
            }
        }
#else
        abort ();
#endif
    }
        
    if (thepath->start.p.x < boxes[0].LL.x || thepath->start.p.x > boxes[0].UR.x ||
            thepath->start.p.y < boxes[0].LL.y || thepath->start.p.y > boxes[0].UR.y) {
        if (Verbose) {
            fprintf (stderr, "in checkpath, start port not in first box\n");
            printpath (thepath);
        }
#ifndef DONTFIXPATH
        if (thepath->start.p.x < boxes[0].LL.x)
            thepath->start.p.x = boxes[0].LL.x;
        if (thepath->start.p.x > boxes[0].UR.x)
            thepath->start.p.x = boxes[0].UR.x;
        if (thepath->start.p.y < boxes[0].LL.y)
            thepath->start.p.y = boxes[0].LL.y;
        if (thepath->start.p.y > boxes[0].UR.y)
            thepath->start.p.y = boxes[0].UR.y;
#else
        abort ();
#endif
    }
    if (thepath->end.p.x < boxes[boxn - 1].LL.x || thepath->end.p.x > boxes[boxn - 1].UR.x ||
            thepath->end.p.y < boxes[boxn - 1].LL.y || thepath->end.p.y > boxes[boxn - 1].UR.y) {
        if (Verbose) {
            fprintf (stderr, "in checkpath, end port not in last box\n");
            printpath (thepath);
        }
#ifndef DONTFIXPATH
        if (thepath->end.p.x < boxes[boxn - 1].LL.x)
            thepath->end.p.x = boxes[boxn - 1].LL.x;
        if (thepath->end.p.x > boxes[boxn - 1].UR.x)
            thepath->end.p.x = boxes[boxn - 1].UR.x;
        if (thepath->end.p.y < boxes[boxn - 1].LL.y)
            thepath->end.p.y = boxes[boxn - 1].LL.y;
        if (thepath->end.p.y > boxes[boxn - 1].UR.y)
            thepath->end.p.y = boxes[boxn - 1].UR.y;
#else
        abort ();
#endif
    }
}

static void computeb2bline (bi, lsp)
    int bi;
    b2bline *lsp;
{
    box *ba, *bb;

    ba = &boxes[bi], bb = &boxes[bi + 1];
    if (ba->LL.y == bb->UR.y) {
        lsp->lp[0].y = lsp->lp[1].y = ba->LL.y;
        lsp->lp[0].x = MAX (ba->LL.x, bb->LL.x), lsp->lp[1].x = MIN (ba->UR.x, bb->UR.x);
    } else if (ba->UR.x == bb->LL.x) {
        lsp->lp[0].x = lsp->lp[1].x = ba->UR.x;
        lsp->lp[0].y = MAX (ba->LL.y, bb->LL.y), lsp->lp[1].y = MIN (ba->UR.y, bb->UR.y);
    } else if (ba->UR.y == bb->LL.y) {
        lsp->lp[0].y = lsp->lp[1].y = ba->UR.y;
        lsp->lp[0].x = MAX (ba->LL.x, bb->LL.x), lsp->lp[1].x = MIN (ba->UR.x, bb->UR.x);
    } else if (ba->LL.x == bb->UR.x) {
        lsp->lp[0].x = lsp->lp[1].x = ba->LL.x;
        lsp->lp[0].y = MAX (ba->LL.y, bb->LL.y), lsp->lp[1].y = MIN (ba->UR.y, bb->UR.y);
    } else {
        fprintf (stderr, "in computeb2bline, boxes don't touch, case 1\n");
        printpath (thepath);
        abort ();
    }
    if (lsp->lp[0].x > lsp->lp[1].x || lsp->lp[0].y > lsp->lp[1].y) {
        fprintf (stderr, "in computeb2bline, boxes don't touch, case 2\n");
        printpath (thepath);
        abort ();
    }
    lsp->p.x = (lsp->lp[0].x + lsp->lp[1].x) / 2.0;
    lsp->p.y = (lsp->lp[0].y + lsp->lp[1].y) / 2.0;
    lsp->margin = lsp->p.x - lsp->lp[0].x, lsp->margin = MIN (lsp->margin, MARGIN);
}

static void fitlines (fbi, lbi)
    int fbi;
    int lbi;
{
    XY maxp;
    int fli, lli, maxli;

    if (linefits (fbi, lbi, &fli, &lli))
        return;

    parametrize (fbi, lbi + 1);
    computelinesplit (fbi, lbi, fli, lli, &maxli, &maxp);
    ls[maxli].p = maxp;
    ls[maxli].piscp = TRUE;
    cpn++;
    fitlines (fbi, maxli - 1);
    fitlines (maxli, lbi);
}

static void rate (fli, lli, n)
    int fli, lli, n;
{
    int li, cnt, maxrating;
    XY t1, t2;

    maxrating = 2;
    if (n <= 3) {
        for (li = fli + 1; li < lli; li++)
            if (ls[li].piscp)
                ls[li].rating = 2;
    } else {
        for (li = fli + 1, cnt = 0; li < lli; li++) {
            if (ls[li].piscp) {
                t1 = norm (sub (ls[li].p, ls[li - 1].p));
                t2 = norm (sub (ls[li + 1].p, ls[li].p));
                if (dist (t1, t2) > 0.5)
                    ls[li].rating = 2, cnt++;
                else
                    ls[li].rating = 1;
            }
        }
        if (cnt == 0)
            maxrating = 1;
    }
    ls[fli].rating = ls[lli].rating = maxrating;
}

static void fitsplines (fbi, lbi, n, ft, lt, ulpp, urpp, llpp, lrpp)
    int fbi, lbi;
    int n;
    XY ft, lt;
    point *ulpp, *urpp, *llpp, *lrpp;
{
    Spline cv;
    XY v1, v2, maxp, maxt, p;
    point p1, p2 , p3, p4;
    int li, maxli, maxcpi;

    parametrize (fbi, lbi + 1);
    cv = mkspline (fbi, lbi + 1, ft, lt);
    for (;;) {
        if (splinefits (fbi, lbi, cv)) {
            if (ulpp || urpp || llpp || lrpp) {
                p1.x = cv.p[0].x, p1.y = cv.p[0].y;
                p = splinepoint (cv, 0.1);
                p2.x = p.x, p2.y = p.y;
                p3.x = cv.p[3].x, p3.y = cv.p[3].y;
                p = splinepoint (cv, 0.9);
                p4.x = p.x, p4.y = p.y;
                if ((n <= 2 && cv.scale0 <= 0.0 && cv.scale3 <= 0.0) ||
                        (!((ulpp && !ccw (p2, p1, *ulpp)) ||
                           (urpp &&  ccw (p2, p1, *urpp)) ||
                           (llpp && !ccw (*llpp, p3, p4)) ||
                           (lrpp &&  ccw (*lrpp, p3, p4))))) {
                    addspline (fbi, lbi, cv);
                    return;
                }
            } else {
                addspline (fbi, lbi, cv);
                return;
            }
        }
        if (n > 2 && cv.scale0 < MINSCALE && cv.scale3 < MINSCALE)
            break;
        if (cv.scale0 > MAXSCALE || cv.scale3 > MAXSCALE)
            cv.scale0 /= 2, cv.scale3 /= 2;
        else
            cv.scale0--, cv.scale3--;
        if (cv.scale0 < 0.0)
            cv.scale0 = 0.0;
        if (cv.scale3 < 0.0)
            cv.scale3 = 0.0;
        cv.p[1] = add (ls[fbi].p, scale (ft, cv.scale0));
        cv.p[2] = add (ls[lbi + 1].p, scale (lt, cv.scale3));
    }
    /* subdivide */
    computesplinesplit (fbi, lbi, cv, &maxli, &maxp, &maxcpi);
    for (li = maxli - 1; li >= fbi; li--)
        if (ls[li].piscp)
            break;
    v1 = sub (ls[li].p, maxp);
    for (li = maxli + 1; li <= lbi + 1; li++)
        if (ls[li].piscp)
            break;
    v2 = sub (maxp, ls[li].p);
    maxt.x = (v1.x + v2.x) / 2.0, maxt.y = (v1.y + v2.y) / 2.0;
    maxt = norm (maxt);

    fitsplines (fbi, maxli - 1, maxcpi + 1, ft, maxt, ulpp, urpp, NULL, NULL);
    maxt.x = - maxt.x, maxt.y = - maxt.y;
    fitsplines (maxli, lbi, n - maxcpi, maxt, lt, NULL, NULL, llpp, lrpp);
}

static int linefits (fbi, lbi, flip, llip)
    int fbi;
    int lbi;
    int *flip;
    int *llip;
{
    XY ip;
    int li;

    if (fbi == lbi)
        return TRUE;
    li = fbi + 1;
    while (intersectsb2bline (ls[fbi].p, ls[lbi + 1].p, li, &ip)) {
        if (!onb2bline (li, ip, TRUE)) {
            *flip = li;
            *llip = lbi;
            if (*flip > *llip) {
                fprintf (stderr, "in linefits, error in line array\n");
                printpath (thepath);
                abort ();
            }
            return FALSE;
        }
        ls[li].p = ip;
        if (++li > lbi)
            break;
    }
    if (li != lbi + 1) {
        *flip = li;
        *llip = lbi;
        return FALSE;
    }
    return TRUE;
}

static int splinefits (fbi, lbi, cv)
    int fbi;
    int lbi;
    Spline cv;
{
    XY cp, pp, ip;
    double t;
    int i, bi;

    for (i = 1, bi = fbi, pp = cv.p[0]; i <= cv.steps; i++) {
        t = i / (double) cv.steps;
        cp = splinepoint (cv, t);
        while (intersectsbox (pp, cp, bi, &ip)) {
            if (ABS (cp.x - ip.x) < EPSILON && ABS (cp.y - ip.y) < EPSILON)
	        ip = cp;
            if (!onb2bline (bi + 1, ip, FALSE))
                return FALSE;
            if (++bi > lbi)
                break;
	    if (cp.x == ip.x && cp.y == ip.y)
                break;
            pp = ip;
        }
        pp = cp;
    }
    if (bi < lbi)
        return FALSE;
    return TRUE;
}

static void computelinesplit (fbi, lbi, fli, lli, maxlip, maxpp)
    int fbi;
    int lbi;
    int fli;
    int lli;
    int *maxlip;
    XY *maxpp;
{
    XY maxp, cp, ip, p, lp0, lp1;
    double maxd, d, d0, d1;
    int maxli, li;

    maxd = INT_MIN;
    maxli = -1;
    for (li = fli; li <= lli; li++) {
        cp = linepoint (ls[fbi].p, ls[lbi + 1].p, ls[li].t);
        if (intersectsb2bline (ls[fbi].p, ls[lbi + 1].p, li, &ip)) {
            if (onb2bline (li, ip, TRUE))
                continue;
            else if (online (ls[fbi].p, ls[lbi + 1].p, ip))
                cp = ip;
        }
        lp0 = ls[li].lp[0], lp0.x += ls[li].margin, lp1 = ls[li].lp[1], lp1.x -= ls[li].margin;
        if ((d0 = dist (cp, lp0)) <= (d1 = dist (cp, lp1)))
            d = d0, p = lp0;
        else
            d = d1, p = lp1;
        if (d > maxd)
            maxd = d, maxli = li, maxp = p;
    }
    if (maxli == -1) {
        fprintf (stderr, "in computelinesplit, failed to find subdivision point\n");
        printpath (thepath);
        abort ();
    }
    *maxlip = maxli;
    *maxpp = maxp;
}

static void computesplinesplit (fbi, lbi, cv, maxlip, maxpp, maxcpip)
    int fbi;
    int lbi;
    Spline cv;
    int *maxlip;
    XY *maxpp;
    int *maxcpip;
{
    XY maxp, cp;
    double maxd, d;
    int maxli, li, maxcpi, cpi;

    maxd = INT_MIN;
    maxli = -1;
    maxcpi = -1;
    for (li = fbi + 1, cpi = 0; li <= lbi; li++) {
        if (!ls[li].piscp)
            continue;
        cpi++;
        cp = splinepoint (cv, ls[li].t);
        d = dist (cp, ls[li].p);
        if (d > maxd)
            maxd = d, maxli = li, maxp = ls[li].p, maxcpi = cpi;
    }
    if (maxli == -1) {
        fprintf (stderr, "in computesplinesplit, failed to find subdivision point\n");
        printpath (thepath);
        abort ();
    }
    *maxlip = maxli;
    *maxpp = maxp;
    *maxcpip = maxcpi;
}

static void addspline (fbi, lbi, cv)
    int fbi;
    int lbi;
    Spline cv;
{
    XY cp, pp, ip;
    double t;
    int i, bi;

    mkspacep (3);
    ps[pn].x = cv.p[1].x, ps[pn++].y = cv.p[1].y;
    ps[pn].x = cv.p[2].x, ps[pn++].y = cv.p[2].y;
    ps[pn].x = cv.p[3].x, ps[pn++].y = cv.p[3].y;

    for (bi = fbi; bi <= lbi; bi++)
        bs[bi] = minbbox;
    add2bbox (fbi, cv.p[0]);
    for (i = 1, bi = fbi, pp = cv.p[0]; i <= cv.steps; i++) {
        t = i / (double) cv.steps;
        cp = splinepoint (cv, t);
        while (intersectsb2bline (pp, cp, bi + 1, &ip) &&
                onb2bline (bi + 1, ip, FALSE)) {
            pp = ip;
            add2bbox (bi, ip);
            if (++bi > lbi)
                break;
            add2bbox (bi, ip);
        }
        if (bi > lbi)
            break;
        pp = cp;
        add2bbox (bi, cp);
    }
    while (++bi <= lbi)
        add2bbox (bi, cv.p[3]);
}

static Spline mkspline (fli, lli, tang0, tang3)

    int fli;
    int lli;
    XY tang0;
    XY tang3;
{
    Spline cv;
    XY tmp;
    double c[2][2], x[2], det01, det0X, detX1;
    double d01, scale0, scale3;
    int li;

    for (li = fli; li <= lli; li++) {
        if (ls[li].piscp && ls[li].rating == maxrating) {
            ls[li].a[0] = scale (tang0, B1 (ls[li].t));
            ls[li].a[1] = scale (tang3, B2 (ls[li].t));
        }
    }

    c[0][0] = c[0][1] = c[1][0] = c[1][1] = 0.0;
    x[0] = x[1] = 0.0;

    for (li = fli; li <= lli; li++) {
        if (!ls[li].piscp || ls[li].rating != maxrating)
            continue;
        c[0][0] += dot (ls[li].a[0], ls[li].a[0]);
        c[0][1] += dot (ls[li].a[0], ls[li].a[1]);
        c[1][0] = c[0][1];
        c[1][1] += dot (ls[li].a[1], ls[li].a[1]);

        tmp = sub (ls[li].p, add (scale (ls[fli].p, B01 (ls[li].t)),
                scale (ls[lli].p, B23 (ls[li].t))));
        x[0] += dot (ls[li].a[0], tmp);
        x[1] += dot (ls[li].a[1], tmp);
    }

    det01 = c[0][0] * c[1][1] - c[1][0] * c[0][1];
    det0X = c[0][0] * x[1] - c[0][1] * x[0];
    detX1 = x[0] * c[1][1] - x[1] * c[0][1];
    if (det01 != 0.0) {
        scale0 = detX1 / det01;
        scale3 = det0X / det01;
    }
    if (ABS (det01) < 1e-6 || scale0 <= 0.0 || scale3 <= 0.0) {
        d01 = dist (ls[fli].p, ls[lli].p) / 3.0;
        scale0 = d01;
        scale3 = d01;
    }
    cv.p[0] = ls[fli].p;
    tmp= scale (tang0, scale0);
    cv.p[1] = add (ls[fli].p, tmp);
    tmp= scale (tang3, scale3);
    cv.p[2] = add (ls[lli].p, tmp);
    cv.p[3] = ls[lli].p;
    cv.scale0 = scale0, cv.scale3 = scale3;
    cv.steps = dist (cv.p[0], cv.p[1]) + dist (cv.p[1], cv.p[2]) +
            dist (cv.p[2], cv.p[3]) - dist (cv.p[0], cv.p[3]);
    if (cv.steps <= 1)
        cv.steps = 2;
    return cv;
}

static int intersectsbox (pp, cp, bi, ipp)
    XY pp;
    XY cp;
    int bi;
    XY *ipp;
{  /* pp is always in the box */
    box *bp;

    bp = &boxes[bi];
    if (cp.x > bp->LL.x && cp.x < bp->UR.x && cp.y > bp->LL.y && cp.y < bp->UR.y)
        return FALSE;
    if (cp.x <= bp->LL.x) {
        ipp->x = bp->LL.x;
        ipp->y = pp.y + ((cp.x != pp.x) ? (ipp->x - pp.x) * (pp.y - cp.y) / (pp.x - cp.x) : 0);
        if (ipp->y >= bp->LL.y && ipp->y <= bp->UR.y)
            return TRUE;
    }
    if (cp.x >= bp->UR.x) {
        ipp->x = bp->UR.x;
        ipp->y = pp.y + ((cp.x != pp.x) ? (ipp->x - pp.x) * (pp.y - cp.y) / (pp.x - cp.x) : 0);
        if (ipp->y >= bp->LL.y && ipp->y <= bp->UR.y)
            return TRUE;
    }
    if (cp.y <= bp->LL.y) {
        ipp->y = bp->LL.y;
        ipp->x = pp.x + ((pp.y != cp.y) ? (ipp->y - pp.y) * (pp.x - cp.x) / (pp.y - cp.y) : 0);
        if (ipp->x >= bp->LL.x && ipp->x <= bp->UR.x)
            return TRUE;
    }
    if (cp.y >= bp->UR.y) {
        ipp->y = bp->UR.y;
        ipp->x = pp.x + ((pp.y != cp.y) ? (ipp->y - pp.y) * (pp.x - cp.x) / (pp.y - cp.y) : 0);
        if (ipp->x >= bp->LL.x && ipp->x <= bp->UR.x)
            return TRUE;
    }
    fprintf (stderr, "in intersectsbox, line intersects but cannot find intersection point\n");
    abort ();
}

static int intersectsb2bline (fp, lp, li, ipp)
    XY fp;
    XY lp;
    int li;
    XY *ipp;
{
    XY p0, p1;

    p0 = ls[li].lp[0], p1 = ls[li].lp[1];
    if (p0.x == p1.x) {
        if (fp.x == lp.x)
            return FALSE;
        if ((p0.x > fp.x && p0.x > lp.x) || (p0.x < fp.x && p0.x < lp.x))
            return FALSE;
        ipp->x = p0.x;
        ipp->y = fp.y + (ipp->x - fp.x) * (fp.y - lp.y) / (fp.x - lp.x);
    } else {
        if (fp.y == lp.y)
            return FALSE;
        if ((p0.y > fp.y && p0.y > lp.y) || (p0.y < fp.y && p0.y < lp.y))
            return FALSE;
        ipp->y = p0.y;
        ipp->x = fp.x + (ipp->y - fp.y) * (fp.x - lp.x) / (fp.y - lp.y);
    }
    return TRUE;
}

static int onb2bline (li, ip, usemargin)
    int li;
    XY ip;
    int usemargin;
{
    int margin;

    margin = usemargin ? ls[li].margin : 0;
    if (ls[li].lp[0].x == ls[li].lp[1].x) {
        if (ip.x == ls[li].lp[0].x &&
                ip.y >= ls[li].lp[0].y + margin && ip.y <= ls[li].lp[1].y - margin)
            return TRUE;
    } else {
        if (ip.y == ls[li].lp[0].y &&
                ip.x >= ls[li].lp[0].x + margin && ip.x <= ls[li].lp[1].x - margin)
            return TRUE;
    }
    return FALSE;
}

static int online (fp, lp, ip)
    XY fp;
    XY lp;
    XY ip;
{
    XY minp, maxp;

    if (fp.y <= lp.y)
        minp.y = fp.y, maxp.y = lp.y;
    else
        minp.y = lp.y, maxp.y = fp.y;
    if (fp.x <= lp.x)
        minp.x = fp.x, maxp.x = lp.x;
    else
        minp.x = lp.x, maxp.x = fp.x;
    return (ip.y >= minp.y && ip.y <= maxp.y && ip.x >= minp.x && ip.x <= maxp.x);
}

static void add2bbox (bi, p)
    int bi;
    XY p;
{
    box *bp;

    bp = &bs[bi];
    if (p.x < bp->LL.x)
        bp->LL.x = p.x;
    if (p.x > bp->UR.x)
        bp->UR.x = p.x;
    if (p.y < bp->LL.y)
        bp->LL.y = p.y;
    if (p.y > bp->UR.y)
        bp->UR.y = p.y;
}

static void parametrize (fli, lli)
    int fli;
    int lli;
{
    int li;

    for (ls[fli].t = 0, li = fli + 1; li <= lli; li++)
        ls[li].t = ls[li - 1].t + dist (ls[li].p, ls[li - 1].p);

    for (li = fli + 1; li <= lli; li++)
        ls[li].t /= ls[lli].t;
}

static XY linepoint (fp, lp, t)
    XY fp;
    XY lp;
    double t;
{
    XY p;

    p.x = fp.x + t * (lp.x - fp.x);
    p.y = fp.y + t * (lp.y - fp.y);
    return p;
}

static XY splinepoint (cv, t)
    Spline cv;
    double t;
{
    Spline tmpcv;

    tmpcv = cv;
    tmpcv.p[0].x = tmpcv.p[0].x + t * (tmpcv.p[1].x - tmpcv.p[0].x);
    tmpcv.p[0].y = tmpcv.p[0].y + t * (tmpcv.p[1].y - tmpcv.p[0].y);
    tmpcv.p[1].x = tmpcv.p[1].x + t * (tmpcv.p[2].x - tmpcv.p[1].x);
    tmpcv.p[1].y = tmpcv.p[1].y + t * (tmpcv.p[2].y - tmpcv.p[1].y);
    tmpcv.p[2].x = tmpcv.p[2].x + t * (tmpcv.p[3].x - tmpcv.p[2].x);
    tmpcv.p[2].y = tmpcv.p[2].y + t * (tmpcv.p[3].y - tmpcv.p[2].y);
    tmpcv.p[0].x = tmpcv.p[0].x + t * (tmpcv.p[1].x - tmpcv.p[0].x);
    tmpcv.p[0].y = tmpcv.p[0].y + t * (tmpcv.p[1].y - tmpcv.p[0].y);
    tmpcv.p[1].x = tmpcv.p[1].x + t * (tmpcv.p[2].x - tmpcv.p[1].x);
    tmpcv.p[1].y = tmpcv.p[1].y + t * (tmpcv.p[2].y - tmpcv.p[1].y);
    tmpcv.p[0].x = tmpcv.p[0].x + t * (tmpcv.p[1].x - tmpcv.p[0].x);
    tmpcv.p[0].y = tmpcv.p[0].y + t * (tmpcv.p[1].y - tmpcv.p[0].y);
    return tmpcv.p[0];
}

static int ccw (a, b, c)
    point a, b, c;
{
    if (a.x * (b.y - c.y) - a.y * (b.x - c.x) + (b.x * c.y - c.x * b.y) > 0)
        return 1;
    return 0;
}

static XY adjusttangent (lpp, rpp, p)
    point *lpp, *rpp;
    point p;
{
    XY lt, rt, t;

    if (lpp && rpp) {
        lt.x = lpp->x - p.x, lt.y = lpp->y - p.y;
        rt.x = rpp->x - p.x, rt.y = rpp->y - p.y;
        t.x = (lt.x + rt.x) / 2.0, t.y = (lt.y + rt.y) / 2.0;
    } else if (lpp) {
        lt.x = lpp->x - p.x, lt.y = lpp->y - p.y;
        t.x = lt.x - lt.y / 3, t.y = lt.y + lt.x / 3;
    } else if (rpp) {
        rt.x = rpp->x - p.x, rt.y = rpp->y - p.y;
        t.x = rt.x + rt.y / 3, t.y = rt.y - rt.x / 3;
    }
    t = norm (t);
    return t;
}

static XY norm (edge)
    XY edge;
{
    double length;

    if ((length = sqrt (edge.x * edge.x + edge.y * edge.y)) != 0.0)
        edge.x /= length, edge.y /= length;
    return edge;
}

static XY add (p1, p2)
    XY p1;
    XY p2;
{
    p1.x += p2.x, p1.y += p2.y;
    return p1;
}

static XY sub (p1, p2)
    XY p1;
    XY p2;
{
    p1.x -= p2.x, p1.y -= p2.y;
    return p1;
}

static double dist (p1, p2)
    XY p1;
    XY p2;
{
    double dx, dy;

    dx = p2.x - p1.x, dy = p2.y - p1.y;
    return sqrt (dx * dx + dy * dy);
}

static XY scale (p, c)
    XY p;
    double c;
{
    p.x *= c, p.y *= c;
    return p;
}

static double dot (p1, p2)
    XY p1;
    XY p2;
{
    return p1.x * p2.x + p1.y * p2.y;
}

static double B1 (t)
    double t;
{
    double tmp = 1.0 - t;
    return 3 * t * tmp * tmp;
}

static double B2 (t)
    double t;
{
    double tmp = 1.0 - t;
    return 3 * t * t * tmp;
}

static double B01 (t)
    double t;
{
    double tmp = 1.0 - t;
    return tmp * tmp * (tmp + 3 * t);
}

static double B23 (t)
    double t;
{
    double tmp = 1.0 - t;
    return t * t * (3 * tmp + t);
}

static void mkspaceb (size)
    int size;
{
    if (bn + size > maxbn) {
        int newmax = maxbn + (size / BINC + 1) * BINC;
        if (!(bs = (box *) realloc ((void *) bs, newmax * sizeof (box)))) {
            fprintf (stderr, "cannot reallocate bs\n");
            abort ();
        }
        maxbn = newmax;
    }
}

static void mkspacep (size)
    int size;
{
    if (pn + size > maxpn) {
        int newmax = maxpn + (size / PINC + 1) * PINC;
        if (!(ps = (point *) realloc ((void *) ps, newmax * sizeof (point)))) {
            fprintf (stderr, "cannot reallocate ps\n");
            abort ();
        }
        maxpn = newmax;
    }
}

static void mkspacel (size)
    int size;
{
    if (ln + size > maxln) {
        int newmax = maxln + (size / LINC + 1) * LINC;
        if (!(ls = (b2bline *) realloc ((void *) ls, newmax * sizeof (b2bline)))) {
            fprintf (stderr, "cannot reallocate ls\n");
            abort ();
        }
        maxln = newmax;
    }
}

static void printpath (pp)
    path *pp;
{
    int bi;

    fprintf (stderr, "edge %d from %s to %s\n", nedges, realedge->tail->name, realedge->head->name);
    if (origedge->u.count > 1)
        fprintf (stderr, "    (it's part of a concentrator edge)\n");
    fprintf (stderr, "%d boxes:\n", pp->nbox);
    for (bi = 0; bi < pp->nbox; bi++)
        fprintf (stderr, "%d (%d, %d), (%d, %d)\n", bi, pp->boxes[bi].LL.x, pp->boxes[bi].LL.y,
                pp->boxes[bi].UR.x, pp->boxes[bi].UR.y);
    fprintf (stderr, "start port: (%d, %d), tangent angle: %lf, %s\n",
            pp->start.p.x, pp->start.p.y, pp->start.theta,
            pp->start.constrained ? "constrained" : "not constrained");
    fprintf (stderr, "end port: (%d, %d), tangent angle: %lf, %s\n",
            pp->end.p.x, pp->end.p.y, pp->end.theta,
            pp->end.constrained ? "constrained" : "not constrained");
}

static void printpsboxes ()
{
    point ll, ur;
    int bi;

    Show_boxes = TRUE;
    for (bi = 0; bi < boxn; bi++) {
	ll = boxes[bi].LL, ur = boxes[bi].UR;
        fprintf (stderr, "%d %d %d %d pathbox\n", ll.x, ll.y, ur.x, ur.y);
    }
}
