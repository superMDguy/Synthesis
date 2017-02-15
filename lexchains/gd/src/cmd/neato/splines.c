/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include "neato.h"

compute_bb(g)
graph_t		*g;
{
	node_t		*n;
	box			b,bb;
	point		pt,s2;

	bb.LL = pointof(MAXINT,MAXINT);
	bb.UR = pointof(-MAXINT,-MAXINT);
	for (n = agfstnode(g); n; n = agnxtnode(g,n)) {
		pt = coord(n);
		s2.x = n->u.xsize/2+1; s2.y = n->u.ysize/2+1;
		b.LL = sub_points(pt,s2);
		b.UR = add_points(pt,s2);

		bb.LL.x = MIN(bb.LL.x,b.LL.x);
		bb.LL.y = MIN(bb.LL.y,b.LL.y);
		bb.UR.x = MAX(bb.UR.x,b.UR.x);
		bb.UR.y = MAX(bb.UR.y,b.UR.y);
	}
	g->u.bb = bb;
}

static bezier *new_spline (e, sz)
	edge_t *e;
	int sz;
{
	bezier *rv;

	if (e->u.spl == NULL) e->u.spl = NEW (splines);
	e->u.spl->list = ALLOC (e->u.spl->size + 1, e->u.spl->list, bezier);
	rv = &(e->u.spl->list[e->u.spl->size++]);
	rv->list = N_NEW (sz, point);
	rv->size = sz;
	rv->sflag = rv->eflag = FALSE;
	return rv;
}

spline_edges(g)
graph_t		*g;
{
	node_t		*n;
	edge_t		*e;
	bezier		*newspl;
	point		p[4],d,ld;
	pointf		offset;

	compute_bb(g);
	offset = cvt2ptf(g->u.bb.LL);
	for (n = agfstnode(g); n; n = agnxtnode(g,n)) {
		n->u.pos[0] -= offset.x;
		n->u.pos[1] -= offset.y;
	}
	set_aspect(g);
	for (n = agfstnode(g); n; n = agnxtnode(g,n)) {
		n->u.coord.x = POINTS(n->u.pos[0]);
		n->u.coord.y = POINTS(n->u.pos[1]);
		for (e = agfstout(g,n); e; e = agnxtout(g,e)) {
			p[0] = coord(e->tail);
			p[3] = coord(e->head);
			d = sub_points(p[3],p[0]);
			d.x = d.x / 3;
			d.y = d.y / 3;
			p[1] = add_points(p[0],d);
			p[2] = sub_points(p[3],d);
			clip_and_install(e,p,4);

			if (e->u.label) {
				d.x = (p[0].x + p[3].x)/ 2;
				d.y = (p[0].y + p[3].y)/ 2;
				if (abs(p[0].x - p[3].x) > abs(p[0].y - p[3].y)) {
					ld.x = 0; ld.y = POINTS(e->u.label->dimen.y)/2 + 2;
				}
				else {
					ld.x = POINTS(e->u.label->dimen.y)/2+e->u.label->fontsize;
					ld.y = 0;
				}
				d = add_points(d,ld);
				e->u.label->p = d;
			}
		}
	}
	g->u.bb.UR = sub_points(g->u.bb.UR,g->u.bb.LL);
	g->u.bb.LL = pointof(0,0);
}

static void arrow_clip();

clip_and_install (e, ps, pn)
	edge_t		*e;
	point		*ps;	/* spline points */
	int 		pn;		/* number of points */
{
	pointf 		p2;
	bezier 		*newspl;
	node_t		*tn, *hn;
	int 		start, end, i;
	point		pt;

	tn = e->tail; hn = e->head;
	newspl = new_spline (e, pn);
		/* spline may be interior to node */
	for (start = 0; start < pn - 4; start+=3) {
		pt = coord(tn);
		p2.x = ps[start+3].x - pt.x;
		p2.y = ps[start+3].y - pt.y;
		if (tn->u.shape == NULL) break;
		if (tn->u.shape->insidefn == NULL) break;
		if (tn->u.shape->insidefn (tn, p2, e) == FALSE) break;
	}
	shape_clip (tn, &ps[start], e);
	for (end = pn - 4; end > 0; end -= 3) {
		pt = coord(hn);
		p2.x = ps[end].x - pt.x;
		p2.y = ps[end].y - pt.y;
		if (hn->u.shape == NULL) break;
		if (hn->u.shape->insidefn == NULL) break;
		if (hn->u.shape->insidefn (hn, p2, e) == FALSE) break;
	}
	shape_clip (hn, &ps[end], e);
	for (; start < pn - 4; start+=3)
		if (ps[start].x != ps[start + 3].x || ps[start].y != ps[start + 3].y)
			break;
	for (; end > 0; end -= 3)
		if (ps[end].x != ps[end + 3].x || ps[end].y != ps[end + 3].y)
			break;
	arrow_clip (e, e, ps, &start, &end, newspl);
	for (i = start; i < end + 4; i++)
		newspl->list[i - start] = ps[i];
	newspl->size = end - start + 4;
}

shape_clip (n, curve, e)
	node_t		*n;
	point		curve[4];
	edge_t		*e;
{
	int			i, save_real_size;
	boolean		found, inside, left_inside;
	pointf		pt, opt, c[4], seg[4], best[4], *left, *right;
	point		p;
	double		low, high, t;

	if (n->u.shape == NULL) return;
	if (n->u.shape->insidefn == NULL) return;
	for (i = 0; i < 4; i++) {
		p = coord(n);
		c[i].x = curve[i].x - p.x;
		c[i].y = curve[i].y - p.y;
	}

	left_inside = n->u.shape->insidefn (n, c[0], e);
	if (left_inside)
		left = NULL, right = seg;
	else
		left = seg, right = NULL;

	found = FALSE;
	low = 0.0; high = 1.0;
	if (left_inside)
		pt = c[0];
	else
		pt = c[3];
	do {
		opt = pt;
		t = (high + low) / 2.0;
		pt = Bezier (c, 3, t, left, right);
		inside = n->u.shape->insidefn (n, pt, e);
		if (inside == FALSE) {
			for (i = 0; i < 4; i++)
				best[i] = seg[i];
			found = TRUE;
		}
		if (inside == left_inside)
			low = t;
		else
			high = t;
	} while (ABS (opt.x - pt.x) > .5 || ABS (opt.y - pt.y) > .5);
	if (found == FALSE)
		for (i = 0; i < 4; i++)
			best[i] = seg[i];

	for (i = 0; i < 4; i++) {
		p = coord(n);
		curve[i].x = ROUND(best[i].x + p.x);
		curve[i].y = ROUND(best[i].y + p.y);
	}
}

/* edge arrow placement (flags) */
#define		E_NONE		1
#define		E_START 	2
#define		E_END		4

#define ARROWLENGTH 9
#define ARROWLENGTHSQ 81
#define sqr(a) ((long) (a) * (a))
#define dstsq(a, b) (sqr (a.x - b.x) + sqr (a.y - b.y))
#define ldstsq(a, b) (sqr ((long) a.x - b.x) + sqr ((long) a.y - b.y))
#define dst(a, b) sqrt ((double) dstsq (a, b))
#define P2PF(p, pf) (pf.x = p.x, pf.y = p.y)
#define PF2P(pf, p) (p.x = ROUND (pf.x), p.y = ROUND (pf.y))

static char *names[] = {"forward", "back", "both", "none", (char*) 0};
static int value[2][4] = {
	{E_END, E_START, E_END | E_START, E_NONE},
	{E_START, E_END, E_END | E_START, E_NONE},
};

static int spline_merge(e)
edge_t	*e;
{
	return FALSE;
}

static void arrow_clip (fe, le, ps, startp, endp, spl)
	edge_t *fe, *le;
	point *ps;
	int *startp, *endp;
	bezier *spl;
{
	edge_t *e;
	pointf sp[4], sp2[4], pf;
	char *p, *attr;
	int i, j, dirflag;
	double d, t, t0;

#ifdef NOTDEF
	for (e = fe; e->u.to_orig; e = e->u.to_orig)
		;
	if (e->head->u.rank > e->tail->u.rank)
		j = 0;
	else if (e->head->u.rank < e->tail->u.rank)
		j = 1;
	else if (e->head->u.order >= e->tail->u.order)
		j = 0;
	else
		j = 1;
#else
	e = fe;
	j = 0;
#endif
	dirflag = value[j][AG_IS_DIRECTED(e->tail->graph)?0:3];
	if (E_dir) {
		attr = agxget (e, E_dir->index);
		for (i = 0; p = names[i]; i++) {
			if (strcmp(attr,p) == 0) {
				dirflag = value[j][i];
				break;
			}
		}
	}
	if (spline_merge (le->head))
		dirflag &= ~(j == 0 ? E_END : E_START);
	if (spline_merge (fe->tail))
		dirflag &= ~(j == 0 ? E_START : E_END);
	if (dirflag == E_NONE)
		return;
	if (dirflag & E_START) {
		spl->sflag = TRUE, spl->sp = ps[*startp];
		if (*endp > *startp && ldstsq (ps[*startp], ps[*startp + 3]) < ARROWLENGTHSQ) {
			*startp += 3;
		}
		P2PF (ps[*startp], sp[0]);
		P2PF (ps[*startp + 1], sp[1]);
		P2PF (ps[*startp + 2], sp[2]);
		P2PF (ps[*startp + 3], sp[3]);
		d = dst (sp[0], sp[1]) + dst (sp[1], sp[2]) + dst (sp[2], sp[3]);
		if ((t = ARROWLENGTH / d) > 1.0)
			t = 1.0;
		for (;;) {
			pf = Bezier (sp, 3, t, NULL, sp2);
			if ((t0 = ldstsq (pf, spl->sp)) <= ARROWLENGTHSQ)
				break;
			t *= (2.0/3.0);
		}
		PF2P (sp2[0], ps[*startp]);
		PF2P (sp2[1], ps[*startp + 1]);
		PF2P (sp2[2], ps[*startp + 2]);
		PF2P (sp2[3], ps[*startp + 3]);
	}
	if (dirflag & E_END) {
		spl->eflag = TRUE, spl->ep = ps[*endp + 3];
		if (*endp > *startp && ldstsq (ps[*endp], ps[*endp + 3]) < ARROWLENGTHSQ) {
			*endp -= 3;
		}
		P2PF (ps[*endp], sp[3]);
		P2PF (ps[*endp + 1], sp[2]);
		P2PF (ps[*endp + 2], sp[1]);
		P2PF (ps[*endp + 3], sp[0]);
		d = dst (sp[0], sp[1]) + dst (sp[1], sp[2]) + dst (sp[2], sp[3]);
		if ((t = ARROWLENGTH / d) > 1.0)
			t = 1.0;
		for (;;) {
			pf = Bezier (sp, 3, t, NULL, sp2);
			if ((t0 = ldstsq (pf, spl->ep)) <= ARROWLENGTHSQ)
				break;
			t *= (2.0/3.0);
		}
		PF2P (sp2[3], ps[*endp]);
		PF2P (sp2[2], ps[*endp + 1]);
		PF2P (sp2[1], ps[*endp + 2]);
		PF2P (sp2[0], ps[*endp + 3]);
	}
}

set_aspect(g)
graph_t	*g;
{
	int		i;
	double	xf,yf,actual,desired;
	char	*str;
	node_t	*n;

	compute_bb(g,g);
	if (/* (g->u.maxrank > 0) && */(str = agget(g,"ratio"))) {
		g->u.bb.UR.x -= g->u.bb.LL.x;
		g->u.bb.UR.y -= g->u.bb.LL.y;	/* normalize */
		if (g->u.left_to_right)
			{int t = g->u.bb.UR.x; g->u.bb.UR.x = g->u.bb.UR.y; g->u.bb.UR.y = t;}
		if (strcmp(str,"fill") == 0) {
			/* fill is weird because both X and Y can stretch */
			if (g->u.drawing->size.x <= 0) return;
			xf = (double)g->u.drawing->size.x / (double)g->u.bb.UR.x;
			yf = (double)g->u.drawing->size.y / (double)g->u.bb.UR.y;
			if ((xf < 1.0) || (yf < 1.0)) {
				if (xf < yf) {yf = yf / xf; xf = 1.0;}
				else {xf = xf / yf; yf = 1.0;}
			}
		}
		else {
			desired = atof(str);
			if (desired == 0.0) return;
			actual = ((float)g->u.bb.UR.y)/((float)g->u.bb.UR.x);
			if (actual < desired) {yf = desired/actual; xf = 1.0;}
			else {xf = actual/desired; yf = 1.0;}
		}
		if (g->u.left_to_right) {float t = xf; xf = yf; yf = t;}
		for (i = 0; n = g->u.nlist[i]; i++) {
			n->u.pos[0] = n->u.pos[0] * xf;
			n->u.pos[1] = n->u.pos[1] * yf;
		}
		g->u.bb.UR.x *= xf;
		g->u.bb.UR.y *= yf;
	}
}
