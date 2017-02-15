/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/*
 * set edge splines.
 */

#include "dot.h"

#define	NSUB	3	/* number of subdivisions, re-aiming splines */
#define	CHUNK	128	/* in building list of edges */

#define MINW 20		/* minimum width of a box in the edge path */
#define HALFMINW 10

#define REGULAREDGE 1
#define FLATEDGE    2
#define SELFWPEDGE  4
#define SELFNPEDGE  8
#define SELFEDGE    8
#define EDGETYPEMASK	 15	/* the OR of the above */
#define FWDEDGE    16
#define BWDEDGE    32
#define EDGEDIRMASK	 48	/* the OR of the above */
#define MAINGRAPH  64
#define AUXGRAPH  128
#define GRAPHTYPEMASK	192	/* the OR of the above */
#define TPORT(e) ((e)->attr[1])
#define HPORT(e) ((e)->attr[2])

/* borrow e->u.tree_index */
#define EFLAGS(e)	((e)->u.tree_index)
#define ISBWDEDGE(e) (EFLAGS (e) & BWDEDGE)
#define MAKEFWDEDGE(new, old) { \
	edge_t *newp; \
	newp = new; \
	*newp = *old; \
	newp->tail = old->head; \
	newp->head = old->tail; \
	newp->u.tail_port = old->u.head_port; \
	newp->u.head_port = old->u.tail_port; \
	newp->u.edge_type = VIRTUAL; \
	newp->u.to_orig = old; \
}

#define CCW  -1 /* counter clock-wise */
#define CW    1 /* clock-wise */
#define ANYW  0 /* could go either way */

#define OTHERDIR(dir) ((dir == CCW) ? CW : CCW)

static selfsidemap[16][3] = {
    { BOTTOM, BOTTOM, ANYW },
    {    TOP,    TOP, ANYW },
    {  RIGHT,  RIGHT, ANYW },
    {   LEFT,   LEFT, ANYW },
    { BOTTOM,   LEFT,  CCW },
    {   LEFT, BOTTOM,   CW },
    {    TOP,  RIGHT,   CW },
    {  RIGHT,    TOP,  CCW },
    {    TOP,   LEFT,  CCW },
    {   LEFT,    TOP,   CW },
    { BOTTOM,  RIGHT,  CCW },
    {  RIGHT, BOTTOM,   CW },
    { BOTTOM,    TOP,  CCW },
    {    TOP, BOTTOM,   CW },
    {   LEFT,  RIGHT,  CCW },
    {  RIGHT,   LEFT,   CW },
};

static flatsidemap[16][6] = {
    { BOTTOM, BOTTOM, BOTTOM, CCW, CCW, FALSE },
    {    TOP,    TOP,    TOP,  CW,  CW, FALSE },
    {  RIGHT,   LEFT, BOTTOM,  CW,  CW,  TRUE },
    { BOTTOM,    TOP,  RIGHT, CCW,  CW,  TRUE },
    {    TOP, BOTTOM,  RIGHT,  CW, CCW,  TRUE },
    {  RIGHT,    TOP,  RIGHT, CCW,  CW,  TRUE },
    {  RIGHT, BOTTOM,  RIGHT,  CW, CCW,  TRUE },
    {    TOP,   LEFT,    TOP,  CW, CCW,  TRUE },
    { BOTTOM,   LEFT, BOTTOM, CCW,  CW,  TRUE },
    {  RIGHT,  RIGHT, BOTTOM,  CW, CCW,  TRUE },
    {   LEFT,   LEFT, BOTTOM, CCW,  CW,  TRUE },
    {   LEFT, BOTTOM, BOTTOM, CCW, CCW, FALSE },
    {    TOP,  RIGHT,    TOP,  CW,  CW, FALSE },
    {   LEFT,    TOP,    TOP,  CW,  CW, FALSE },
    { BOTTOM,  RIGHT, BOTTOM, CCW, CCW, FALSE },
    {   LEFT,  RIGHT, BOTTOM, CCW, CCW, FALSE },
};

#define NEXTSIDE(side, dir) ((dir == CCW) ? \
		((side & 0x8) ? BOTTOM : (side << 1)) : \
		((side & 0x1) ? LEFT : (side >> 1)))

#define AVG(a, b) ((a + b) / 2)

typedef struct pathend_t {
	box nb; /* the node box */
	point np; /* node port */
	int sidemask;
	int boxn;
	box boxes[20];
} pathend_t;

static path *P;
static int LeftBound, RightBound, FlatHeight, Splinesep, Multisep;
static box *Rank_box;

static point points[1000], points2[1000];
static int pointn;
static box boxes[1000];

static void setflags ();
static int edgecmp ();
static void make_self_edge ();
static void make_flat_edge ();
static void make_regular_edge ();
static void chooseselfsides ();
static void completeselfpath ();
static box makeselfend ();
static box makeselfcomponent ();
static void adjustselfends ();
static void chooseflatsides ();
static void completeflatpath ();
static box makeflatend ();
static box makeflatcomponent ();
static void completeregularpath ();
static void makeregularend ();
#if 0 /* box subdivision is obsolete, I think... ek */
static void refineregularends ();
#endif
static void adjustregularpath ();
static box rank_box ();
static int straight_len ();
static edge_t *straight_path ();
static void recover_slack ();
static void resize_vn ();
static edge_t *top_bound ();
static edge_t *bot_bound ();
point spline_at_y ();
static void beginpath ();
static void endpath ();
static edge_t *getmainedge ();
static splines *getsplinepoints ();
static box maximal_bbox ();
static node_t *neighbor ();
static boolean pathscross ();
static void add_box ();
static void clip_and_install ();
static bezier *new_spline ();
static void shape_clip ();
static void arrow_clip ();
static void place_vnlabel ();

#define GROWEDGES (edges = ALLOC (n_edges + CHUNK, edges, edge_t*))

dot_splines (g)
	graph_t	*g;
{
	int i, j, k, n_nodes, n_edges, ind, cnt;
	node_t *n;
	edge_t fwdedgea, fwdedgeb;
	edge_t *e, *e0, *e1, *ea, *eb, *le0, *le1, **edges;

	mark_lowclusters(g);
	routesplinesinit();
	P = NEW(path);
	FlatHeight = 2 * g->u.nodesep;
	Splinesep = g->u.nodesep / 4;
	Multisep = g->u.nodesep;
	edges = N_NEW (CHUNK, edge_t*);
	E_dir = agfindattr(g->proto->e,"dir");

	/* compute boundaries and list of splines */
	LeftBound = RightBound = 0;
	n_edges = n_nodes = 0;
	for (i = g->u.minrank; i <= g->u.maxrank; i++) {
		n_nodes += g->u.rank[i].n;
		if (n = g->u.rank[i].v[0])
			LeftBound = MIN (LeftBound, (n->u.coord.x - n->u.lw));
		if (g->u.rank[i].n && (n = g->u.rank[i].v[g->u.rank[i].n - 1]))
			RightBound = MAX (RightBound, (n->u.coord.x + n->u.rw));

		for (j = 0; j < g->u.rank[i].n; j++) {
			n = g->u.rank[i].v[j];
			if ((n->u.node_type != NORMAL) &&
					(spline_merge(n) == FALSE))
				continue;
			for (k = 0; e = n->u.out.list[k]; k++) {
				if (e->u.edge_type == FLATORDER)
					continue;
				setflags (e, REGULAREDGE, FWDEDGE, MAINGRAPH);
				edges[n_edges++] = e;
				if (n_edges % CHUNK == 0)
					GROWEDGES;
			}
			if (n->u.flat_out.list)
				for (k = 0; e = n->u.flat_out.list[k]; k++) {
					setflags (e, FLATEDGE, 0, AUXGRAPH);
					edges[n_edges++] = e;
					if (n_edges % CHUNK == 0)
						GROWEDGES;
				}
			if (n->u.other.list)
				for (k = 0; e = n->u.other.list[k]; k++) {
					setflags (e, 0, 0, AUXGRAPH);
					edges[n_edges++] = e;
					if (n_edges % CHUNK == 0)
						GROWEDGES;
				}
		}
	}

	qsort((char*) &edges[0], n_edges, sizeof (edges[0]), edgecmp);

	/* FIXME: just how many boxes can there be? */
	P->boxes = N_NEW (n_nodes + 20 * 2 * NSUB, box);
	Rank_box = N_NEW (i, box);

	for (i = 0; i < n_edges; ) {
		ind = i;
		le0 = getmainedge ((e0 = edges[i++]));
		ea = (e0->u.tail_port.defined) ? e0 : le0;
		if (EFLAGS (ea) & BWDEDGE) {
			MAKEFWDEDGE (&fwdedgea, ea);
			ea = &fwdedgea;
		}
		for (cnt = 1; i < n_edges; cnt++, i++) {
			if (le0 != (le1 = getmainedge ((e1 = edges[i]))))
				break;
			eb = (e1->u.tail_port.defined) ? e1 : le1;
			if (EFLAGS (eb) & BWDEDGE) {
				MAKEFWDEDGE (&fwdedgeb, eb);
				eb = &fwdedgeb;
			}
			if (portcmp(ea->u.tail_port,eb->u.tail_port)) break;
			if (portcmp(ea->u.head_port,eb->u.head_port)) break;
			if ((EFLAGS (e0) & EDGETYPEMASK) == FLATEDGE && e0->u.label != e1->u.label)
				break;
			if (EFLAGS (edges[i]) & MAINGRAPH) /* Aha! -C is on */
				break;
		}
		if (e0->tail == e0->head)
			make_self_edge (edges, ind, cnt);
		else if (e0->tail->u.rank == e0->head->u.rank)
			make_flat_edge (edges, ind, cnt);
		else
			make_regular_edge (edges, ind, cnt);
	}

	/* make the other splines and place labels */
	for (n = g->u.nlist; n; n = n->u.next) {
		if ((n->u.node_type == VIRTUAL) && (n->u.label)) place_vnlabel(n);
	}

	free (edges);
	free (P->boxes);
	free (P);
	free (Rank_box);
	routesplinesterm ();
}

	/* compute position of an edge label from its virtual node */
static void place_vnlabel(n)
node_t		*n;
{
	pointf		dimen;
	double		width;
	edge_t		*e;
	if (n->u.in.size == 0) return;	/* skip flat edge labels here */
	for (e = n->u.out.list[0]; e->u.edge_type != NORMAL; e = e->u.to_orig);
	dimen = e->u.label->dimen;
	width = n->graph->u.left_to_right? dimen.y : dimen.x;
	e->u.label->p.x = n->u.coord.x + POINTS(width/2.0);
	e->u.label->p.y = n->u.coord.y;
}

static void setflags (e, hint1, hint2, f3)
	edge_t *e;
	int hint1, hint2, f3;
{
	int f1, f2;
	if (hint1 != 0)
		f1 = hint1;
	else {
		if (e->tail == e->head)
			if (e->u.tail_port.defined || e->u.head_port.defined)
				f1 = SELFWPEDGE;
			else
				f1 = SELFNPEDGE;
		else if (e->tail->u.rank == e->head->u.rank)
			f1 = FLATEDGE;
		else
			f1 = REGULAREDGE;
	}
	if (hint2 != 0)
		f2 = hint2;
	else {
		if (f1 == REGULAREDGE)
			f2 = (e->tail->u.rank < e->head->u.rank) ?
					FWDEDGE : BWDEDGE;
		else if (f1 == FLATEDGE)
			f2 = (e->tail->u.order < e->head->u.order) ?
					FWDEDGE : BWDEDGE;
		else /* f1 == SELF*EDGE */
			f2 = FWDEDGE;
	}
	EFLAGS (e) = (f1 | f2 | f3);
}
			
static int edgecmp (ptr0, ptr1)
	edge_t **ptr0, **ptr1;
{
	edge_t fwdedgea, fwdedgeb, *e0, *e1, *ea, *eb, *le0, *le1;
	int et0, et1, v0, v1, rv;

	e0 = (edge_t *) *ptr0;
	e1 = (edge_t *) *ptr1;
	et0 = EFLAGS (e0) & EDGETYPEMASK;
	et1 = EFLAGS (e1) & EDGETYPEMASK;
	if (et0 != et1)
		return (et1 - et0);
	le0 = getmainedge (e0);
	le1 = getmainedge (e1);
	v0 = le0->tail->u.rank - le0->head->u.rank, v0 = ABS (v0);
	v1 = le1->tail->u.rank - le1->head->u.rank, v1 = ABS (v1);
	if (v0 != v1)
		return (v0 - v1);
	v0 = le0->tail->u.coord.x - le0->head->u.coord.x, v0 = ABS (v0);
	v1 = le1->tail->u.coord.x - le1->head->u.coord.x, v1 = ABS (v1);
	if (v0 != v1)
		return (v0 - v1);
	if (le0->id != le1->id)
		return (le0->id - le1->id);
	ea = (e0->u.tail_port.defined || e0->u.head_port.defined) ? e0 : le0;
	if (EFLAGS (ea) & BWDEDGE) {
		MAKEFWDEDGE (&fwdedgea, ea);
		ea = &fwdedgea;
	}
	eb = (e1->u.tail_port.defined || e1->u.head_port.defined) ? e1 : le1;
	if (EFLAGS (eb) & BWDEDGE) {
		MAKEFWDEDGE (&fwdedgeb, eb);
		eb = &fwdedgeb;
	}
	if (rv = portcmp(ea->u.tail_port,eb->u.tail_port)) return rv;
	if (rv = portcmp(ea->u.head_port,eb->u.head_port)) return rv;
	v0 = EFLAGS (e0) & GRAPHTYPEMASK;
	v1 = EFLAGS (e1) & GRAPHTYPEMASK;
	if (v0 != v1)
		return (v0 - v1);
	if (et0 == FLATEDGE && e0->u.label != e1->u.label)
		return (int) (e0->u.label - e1->u.label);
	return (e0->id - e1->id);
}

static void make_self_edge (edges, ind, cnt)
	edge_t **edges;
	int ind, cnt;
{
	node_t *n;
	edge_t *e;
	point *ps, np;
	pathend_t tend, hend;
	int i, j, maxx, stepx, stepy, dx, dy, tside, hside, dir, pn;
	double width, height;

	e = edges[ind];
	n = e->tail;
	i = n->u.rw, n->u.rw = n->u.mval, n->u.mval = i;    /* recover original size */

	/* self edge without ports */

	if ((!e->u.tail_port.defined) && (!e->u.head_port.defined)) {
		stepx = Multisep, stepy = (n->u.ht / 2) / cnt;
		pointn = 0;
		np = n->u.coord;
		dx = n->u.rw, dy = 0;
		for (i = 0; i < cnt; i++) {
			e = edges[ind++];
			dx += stepx, dy += stepy;
			pointn = 0;
			points[pointn++] = np;
			points[pointn++] = pointof (np.x + dx / 3, np.y - dy);
			points[pointn++] = pointof (np.x + dx,     np.y - dy);
			points[pointn++] = pointof (np.x + dx,     np.y);
			points[pointn++] = pointof (np.x + dx,     np.y + dy);
			points[pointn++] = pointof (np.x + dx / 3, np.y + dy);
			points[pointn++] = np;
			if (e->u.label) {
				if (e->tail->graph->u.left_to_right) {
					width = e->u.label->dimen.y;
					height = e->u.label->dimen.x;
				} else {
					width = e->u.label->dimen.x;
					height = e->u.label->dimen.y;
				}
				e->u.label->p.x = n->u.coord.x + dx + POINTS(width/2.0);
				e->u.label->p.y = n->u.coord.y;
				if (POINTS (width) > stepx)
					dx += POINTS(width) - stepx;
				if (dy + stepy < POINTS(height))
					dy += (POINTS(height) - stepy);
			}
			clip_and_install (e, e, points, pointn);
		}
		return;
	}

	/* self edge with ports */

	tend.nb = boxof (n->u.coord.x - n->u.lw, n->u.coord.y - n->u.ht / 2,
			n->u.coord.x + n->u.rw, n->u.coord.y + n->u.ht / 2);
	hend.nb = tend.nb;
	stepx = Multisep, stepy = Multisep / 2;
	dx = 0, dy = 0;
	for (i = 0; i < cnt; i++) {
		e = edges[ind++];
		dx += stepx, dy += stepy;

		/* tail setup */
		beginpath (e, SELFEDGE, &tend);

		/* head setup */
		endpath (e, SELFEDGE, &hend);

		chooseselfsides (&tend, &hend, &tside, &hside, &dir);
		completeselfpath (&tend, &hend, tside, hside, dir,
				dx, dy, Multisep, Multisep);

		ps = routesplines (P, &pn);
		if (e->u.label) {
			/* FIXME: labels only right for BOTTOM -> TOP edges */
			for (j = 0, maxx = n->u.coord.x; j < P->nbox; j++)
				if (P->boxes[j].UR.x > maxx)
					maxx = P->boxes[j].UR.x;
			if (e->tail->graph->u.left_to_right)
				width = e->u.label->dimen.y;
			else
				width = e->u.label->dimen.x;
			e->u.label->p.x = maxx + POINTS(width/2.0);
			e->u.label->p.y = n->u.coord.y;
			if (POINTS (width) > stepx)
				dx += POINTS (width) - stepx;
		}
		clip_and_install (e, e, ps, pn);
	}
}

static void make_flat_edge (edges, ind, cnt)
	edge_t **edges;
	int ind, cnt;
{
	node_t *tn, *hn, *n;
	edge_t fwdedge, *e;
	int i, stepx, stepy, dx, dy, ht2;
	int tside, hside, mside, tdir, hdir, cross, pn;
	point *ps;
	point tp, hp, np;
	pathend_t tend, hend;
	box lb, rb, wlb, wrb;
	rank_t *rank;

	dx = 0;
	e = edges[ind];
	if (EFLAGS (e) & BWDEDGE) {
		MAKEFWDEDGE (&fwdedge, e);
		e = &fwdedge;
	}
	tn = e->tail, hn = e->head;

	/* flat edge without ports that can go straight left to right */

	if (e->u.label) {
		edge_t		*f;
		for (f = e->u.to_virt; f->u.to_virt; f = f->u.to_virt);
		e->u.label->p = f->tail->u.coord;
	}

	if ((!e->u.tail_port.defined) && (!e->u.head_port.defined)) {
		rank = &(tn->graph->u.rank[tn->u.rank]);
		for (i = tn->u.order + 1; i < hn->u.order; i++) {
			n = rank->v[i];
			if ((n->u.node_type == VIRTUAL && n->u.label) ||
					n->u.node_type == NORMAL)
				break;
		}
		if (i != hn->u.order)
			goto flatnostraight;

		stepy = (cnt > 1) ? tn->u.ht / (cnt - 1) : 0;
		tp = tn->u.coord;
		hp = hn->u.coord;
		dy = tp.y - ((cnt > 1) ? tn->u.ht / 2 : 0);
		for (i = 0; i < cnt; i++) {
			e = edges[ind + i];
			pointn = 0;
			points[pointn++] = tp;
			points[pointn++] = pointof ((2 * tp.x + hp.x) / 3, dy);
			points[pointn++] = pointof ((2 * hp.x + tp.x) / 3, dy);
			points[pointn++] = hp;
#ifdef OBSOLETE
			if (e->u.label) { /* FIXME: fix label positioning */
				labelw = e->u.label->dimen.x * 72;
				e->u.label->p = pointof (tp.x + labelw / 2, tp.y);
				/* dx += labelw;*/
			}
#endif
			dy += stepy;
			clip_and_install (e, e, points, pointn);
		}
		return;
	}

	/* !(flat edge without ports that can go straight left to right) */

flatnostraight:
	tend.nb = boxof (tn->u.coord.x - tn->u.lw, tn->u.coord.y - tn->u.ht / 2,
			tn->u.coord.x + tn->u.rw, tn->u.coord.y + tn->u.ht / 2);
	hend.nb = boxof (hn->u.coord.x - hn->u.lw, hn->u.coord.y - hn->u.ht / 2,
			hn->u.coord.x + hn->u.rw, hn->u.coord.y + hn->u.ht / 2);
	ht2 = tn->graph->u.rank[tn->u.rank].ht2;
	stepx = Multisep / cnt, stepy = ht2 / cnt;
	lb = boxof (tn->u.coord.x - tn->u.lw, tn->u.coord.y - ht2,
			tn->u.coord.x + tn->u.rw, tn->u.coord.y + ht2);
	rb = boxof (hn->u.coord.x - hn->u.lw, hn->u.coord.y - ht2,
			hn->u.coord.x + hn->u.rw, hn->u.coord.y + ht2);
	for (i = 0; i < cnt; i++) {
		e = edges[ind + i];
		if (EFLAGS (e) & BWDEDGE) {
			MAKEFWDEDGE (&fwdedge, e);
			e = &fwdedge;
		}

		/* tail setup */
		beginpath (e, FLATEDGE, &tend);

		/* head setup */
		endpath (e, FLATEDGE, &hend);

		chooseflatsides (&tend, &hend, &tside, &hside, &mside,
				&tdir, &hdir, &cross);
		if (e->u.label) { /* edges with labels aren't multi-edges */
			edge_t *le;
			node_t *ln;
			for (le = e; le->u.to_virt; le = le->u.to_virt)
				;
			ln = le->tail;
			wlb.LL.x = lb.LL.x;
			wlb.LL.y = lb.LL.y;
			wlb.UR.x = lb.UR.x;
			wlb.UR.y = ln->u.coord.y - ln->u.ht / 2;
			wrb.LL.x = rb.LL.x;
			wrb.LL.y = rb.LL.y;
			wrb.UR.x = rb.UR.x;
			wrb.UR.y = ln->u.coord.y - ln->u.ht / 2;
		} else {
			wlb.LL.x = lb.LL.x - (i + 1) * stepx;
			wlb.LL.y = lb.LL.y - (i + 1) * stepy;
			wlb.UR.x = lb.UR.x + (i + 1) * stepx;
			wlb.UR.y = lb.UR.y + (i + 1) * stepy;
			if (cross) {
				wrb.LL.x = rb.LL.x - (cnt - i) * stepx;
				wrb.LL.y = rb.LL.y - (cnt - i) * stepy;
				wrb.UR.x = rb.UR.x + (cnt - i) * stepx;
				wrb.UR.y = rb.UR.y + (cnt - i) * stepy;
			} else {
				wrb.LL.x = rb.LL.x - (i + 1) * stepx;
				wrb.LL.y = rb.LL.y - (i + 1) * stepy;
				wrb.UR.x = rb.UR.x + (i + 1) * stepx;
				wrb.UR.y = rb.UR.y + (i + 1) * stepy;
			}
		}
		completeflatpath (&tend, &hend, tside, hside, mside,
				tdir, hdir, wlb, wrb, stepx, stepy);

		ps = routesplines (P, &pn);
		clip_and_install (e, e, ps, pn);
	}
}

static void make_regular_edge (edges, ind, cnt)
	edge_t **edges;
	int ind, cnt;
{
	graph_t *g;
	node_t *tn, *hn;
	edge_t fwdedgea, fwdedgeb, fwdedge, *e, *fe, *le, *segfirst;
	point *ps;
	pathend_t tend, hend;
	box b;
	int boxn, sl, si, smode, i, j, dx, pn, hackflag, longedge;

	e = edges[ind];
	hackflag = FALSE;
	if (ABS (e->tail->u.rank - e->head->u.rank) > 1) {
		fwdedgea = *e;
		if (EFLAGS (e) & BWDEDGE) {
			MAKEFWDEDGE (&fwdedgeb, e);
			fwdedgea.tail = e->head;
			fwdedgea.u.tail_port = e->u.head_port;
		} else {
			fwdedgeb = *e;
			fwdedgea.tail = e->tail;
		}
		le = getmainedge (e);
		while (le->u.to_virt) le = le->u.to_virt;
		fwdedgea.head = le->head;
		fwdedgea.u.head_port.defined = FALSE;
		fwdedgea.u.edge_type = VIRTUAL;
		fwdedgea.u.head_port.p.x = fwdedgea.u.head_port.p.y = 0;
		fwdedgea.u.to_orig = e;
		e = &fwdedgea;
		hackflag = TRUE;
	} else {
		if (EFLAGS (e) & BWDEDGE) {
			MAKEFWDEDGE (&fwdedgea, e);
			e = &fwdedgea;
		}
	}
	fe = e;
		
	/* compute the spline points for the edge */

	boxn = 0;
	pointn = 0;
	segfirst = e;
	g = e->tail->graph;
	tn = e->tail;
	hn = e->head;
	tend.nb = maximal_bbox (tn, NULL, e);
	beginpath (e, REGULAREDGE, &tend);
	makeregularend (tend.boxes[tend.boxn - 1], BOTTOM,
		tn->u.coord.y - tn->graph->u.rank[tn->u.rank].ht2, &b);
	if (b.LL.x < b.UR.x && b.LL.y < b.UR.y)
		tend.boxes[tend.boxn++] = b;
	longedge = 0;
	smode = FALSE, si = -1;
	while (hn->u.node_type == VIRTUAL && !spline_merge (hn)) {
		longedge = 1;
		boxes[boxn++] = rank_box (g, tn->u.rank);
		if (!smode && ((sl = straight_len (hn)) >= (g->u.has_edge_labels ? 4 + 1 : 2 +1))) {
			smode = TRUE;
			si = 1, sl -= 2;
		}
		if (!smode || si > 0) {
			si--;
			boxes[boxn++] = maximal_bbox (hn, e, hn->u.out.list[0]);
			e = hn->u.out.list[0];
			tn = e->tail;
			hn = e->head;
			continue;
		}
		hend.nb = maximal_bbox (hn, e, hn->u.out.list[0]);
		endpath (e, REGULAREDGE, &hend);
		makeregularend (hend.boxes[hend.boxn - 1], TOP,
			hn->u.coord.y + hn->graph->u.rank[hn->u.rank].ht2, &b);
		if (b.LL.x < b.UR.x && b.LL.y < b.UR.y)
			hend.boxes[hend.boxn++] = b;
		P->end.theta = PI / 2, P->end.constrained = TRUE;
		completeregularpath (segfirst, e, &tend, &hend, boxes, boxn, 1);
		ps = routesplines (P, &pn);
		for (i = 0; i < pn; i++)
			points[pointn++] = ps[i];
		e = straight_path (hn->u.out.list[0], sl, points, &pointn);
		recover_slack (segfirst, P);
		segfirst = e;
		tn = e->tail;
		hn = e->head;
		boxn = 0;
		tend.nb = maximal_bbox (tn, tn->u.in.list[0], e);
		beginpath (e, REGULAREDGE, &tend);
		makeregularend (tend.boxes[tend.boxn - 1], BOTTOM,
			tn->u.coord.y - tn->graph->u.rank[tn->u.rank].ht2, &b);
		if (b.LL.x < b.UR.x && b.LL.y < b.UR.y)
			tend.boxes[tend.boxn++] = b;
		P->start.theta = - PI / 2, P->start.constrained = TRUE;
		smode = FALSE;
	}
	boxes[boxn++] = rank_box (g, tn->u.rank);
	hend.nb = maximal_bbox (hn, e, NULL);
	endpath (hackflag ? &fwdedgeb : e, REGULAREDGE, &hend);
	makeregularend (hend.boxes[hend.boxn - 1], TOP,
		hn->u.coord.y + hn->graph->u.rank[hn->u.rank].ht2, &b);
	if (b.LL.x < b.UR.x && b.LL.y < b.UR.y)
		hend.boxes[hend.boxn++] = b;
	completeregularpath (segfirst, e, &tend, &hend, boxes, boxn, longedge);
	ps = routesplines (P, &pn);
	for (i = 0; i < pn; i++)
		points[pointn++] = ps[i];
	recover_slack (segfirst, P);

	/* make copies of the spline points, one per multi-edge */

	if (cnt == 1) {
		clip_and_install (fe, hackflag ? &fwdedgeb : e, points, pointn);
		return;
	}
	dx = Multisep * (cnt - 1) / 2;
	for (i = 1; i < pointn - 1; i++)
		points[i].x -= dx;
	for (i = 0; i < pointn; i++)
		points2[i] = points[i];
	clip_and_install (fe, hackflag ? &fwdedgeb : e, points2, pointn);
	for (j = 1; j < cnt; j++) {
		e = edges[ind + j];
		if (EFLAGS (e) & BWDEDGE) {
			MAKEFWDEDGE (&fwdedge, e);
			e = &fwdedge;
		}
		for (i = 1; i < pointn - 1; i++)
			points[i].x += Multisep;
		for (i = 0; i < pointn; i++)
			points2[i] = points[i];
		clip_and_install (e, e, points2, pointn);
	}
}

/* self edges */

static void chooseselfsides (tendp, hendp, tsidep, hsidep, dirp)
	pathend_t *tendp, *hendp;
	int *tsidep, *hsidep, *dirp;
{
	int i;

	for (i = 0; i < 16; i++)
		if ((selfsidemap[i][0] & tendp->sidemask) &&
				(selfsidemap[i][1] & hendp->sidemask))
			break;
	if (i == 16)
		abort ();
	*tsidep = selfsidemap[i][0], *hsidep = selfsidemap[i][1];
	*dirp = selfsidemap[i][2];
	if (*dirp == ANYW) { /* ANYW can appear when tside == hside */
		switch (*tsidep) {
		case BOTTOM:
			*dirp = (tendp->np.x < hendp->np.x) ? CCW : CW;
			break;
		case RIGHT:
			*dirp = (tendp->np.y < hendp->np.y) ? CCW : CW;
			break;
		case TOP:
			*dirp = (tendp->np.x > hendp->np.x) ? CCW : CW;
			break;
		case LEFT:
			*dirp = (tendp->np.y > hendp->np.y) ? CCW : CW;
			break;
		}
	}
}

static void completeselfpath (tendp, hendp, tside, hside, dir, dx, dy, w, h)
	pathend_t *tendp, *hendp;
	int tside, hside, dir, dx, dy, w, h;
{
	int i, side;
	box boxes[4]; /* can't have more than 6 boxes */
	box tb, hb;
	int boxn;

	tb = makeselfend (tendp->boxes[tendp->boxn - 1],
			tside, dir, dx, dy);
	hb = makeselfend (hendp->boxes[hendp->boxn - 1],
			hside, OTHERDIR (dir), dx, dy);

	if (tside == hside && tendp->np.x == hendp->np.x &&
			tendp->np.y == hendp->np.y)
		adjustselfends (&tb, &hb, tendp->np, tside, dir);

	boxn = 0;
	for (side = tside; ; side = NEXTSIDE (side, dir)) {
		boxes[boxn++] = makeselfcomponent (tendp->nb, side, dx, dy, w, h);
		if (side == hside)
			break;
	}
	for (i = 0; i < tendp->boxn; i++)
		add_box (tendp->boxes[i]);
	add_box (tb);
	for (i = 0; i < boxn; i++)
		add_box (boxes[i]);
	add_box (hb);
	for (i = hendp->boxn - 1; i >= 0 ; i--)
		add_box (hendp->boxes[i]);
}

static box makeselfend (b, side, dir, dx, dy)
	box b;
	int side, dir, dx, dy;
{
	box eb;

	switch (side) {
	case BOTTOM:
		eb = boxof (b.LL.x, b.LL.y - dy, b.UR.x, b.LL.y);
		(dir == CCW) ? (eb.UR.x += dx / 2) : (eb.LL.x -= dx / 2);
		break;
	case RIGHT:
		eb = boxof (b.UR.x, b.LL.y, b.UR.x + dx, b.UR.y);
		(dir == CCW) ? (eb.UR.y += dy / 2) : (eb.LL.y -= dy / 2);
		break;
	case TOP:
		eb = boxof (b.LL.x, b.UR.y, b.UR.x, b.UR.y + dy);
		(dir == CCW) ? (eb.LL.x -= dx / 2) : (eb.UR.x += dx / 2);
		break;
	case LEFT:
		eb = boxof (b.LL.x - dx, b.LL.y, b.LL.x, b.UR.y);
		(dir == CCW) ? (eb.LL.y -= dy / 2) : (eb.UR.y += dy / 2);
		break;
	}
	return eb;
}

static box makeselfcomponent (nb, side, dx, dy, w, h)
	box nb;
	int side, dx, dy, w, h;
{
	box b;

	switch (side) {
	case BOTTOM:
		b.LL.x = nb.LL.x - dx - w, b.LL.y = nb.LL.y - dy - h;
		b.UR.x = nb.UR.x + dx + w, b.UR.y = b.LL.y + h;
		break;
	case RIGHT:
		b.LL.x = nb.UR.x + dx,     b.LL.y = nb.LL.y - dy;
		b.UR.x = b.LL.x + w,       b.UR.y = nb.UR.y + dy;
		break;
	case TOP:
		b.LL.x = nb.LL.x - dx - w, b.LL.y = nb.UR.y + dy;
		b.UR.x = nb.UR.x + dx + w, b.UR.y = b.LL.y + h;
		break;
	case LEFT:
		b.LL.x = nb.LL.x - dx - w, b.LL.y = nb.LL.y - dy;
		b.UR.x = b.LL.x + w,       b.UR.y = nb.UR.y + dy;
		break;
	}
	return b;
}

static void adjustselfends (tbp, hbp, p, side, dir)
	box *tbp, *hbp;
	point p;
	int side, dir;
{
	switch (side) {
	case BOTTOM:
		if (dir == CCW) {
			tbp->LL.x -= (tbp->UR.x - p.x), tbp->UR.x = p.x;
			hbp->UR.x += (p.x - hbp->LL.x), hbp->LL.x = p.x;
		} else {
			tbp->UR.x -= (tbp->LL.x - p.x), tbp->LL.x = p.x;
			hbp->LL.x += (p.x - hbp->UR.x), hbp->UR.x = p.x;
		}
		break;
	case RIGHT:
		if (dir == CCW) {
			tbp->LL.y -= (tbp->UR.y - p.y), tbp->UR.y = p.y;
			hbp->UR.y += (p.y - hbp->LL.y), hbp->LL.y = p.y;
		} else {
			tbp->UR.y -= (tbp->LL.y - p.y), tbp->LL.y = p.y;
			hbp->LL.y += (p.y - hbp->UR.y), hbp->UR.y = p.y;
		}
		break;
	case TOP:
		if (dir == CW) {
			tbp->LL.x -= (tbp->UR.x - p.x), tbp->UR.x = p.x;
			hbp->UR.x += (p.x - hbp->LL.x), hbp->LL.x = p.x;
		} else {
			tbp->UR.x -= (tbp->LL.x - p.x), tbp->LL.x = p.x;
			hbp->LL.x += (p.x - hbp->UR.x), hbp->UR.x = p.x;
		}
		break;
	case LEFT:
		if (dir == CW) {
			tbp->LL.y -= (tbp->UR.y - p.y), tbp->UR.y = p.y;
			hbp->UR.y += (p.y - hbp->LL.y), hbp->LL.y = p.y;
		} else {
			tbp->UR.y -= (tbp->LL.y - p.y), tbp->LL.y = p.y;
			hbp->LL.y += (p.y - hbp->UR.y), hbp->UR.y = p.y;
		}
		break;
	}
}

/* flat edges */

static void chooseflatsides (tendp, hendp,
		tsidep, hsidep, msidep, tdirp, hdirp, crossp)
	pathend_t *tendp, *hendp;
	int *tsidep, *hsidep, *tdirp, *hdirp, *msidep, *crossp;
{
	int i;

	for (i = 0; i < 16; i++)
		if ((flatsidemap[i][0] & tendp->sidemask) &&
				(flatsidemap[i][1] & hendp->sidemask))
			break;
	if (i == 16)
		abort ();
	*tsidep = flatsidemap[i][0], *hsidep = flatsidemap[i][1];
	*msidep = flatsidemap[i][2];
	*tdirp = flatsidemap[i][3], *hdirp = flatsidemap[i][4];
	*crossp = flatsidemap[i][5];
}

static void completeflatpath (tendp, hendp, tside, hside, mside,
		tdir, hdir, lb, rb, w, h)
	pathend_t *tendp, *hendp;
	int tside, hside, mside, tdir, hdir;
	box lb, rb;
	int w, h;
{
	int i, side, boxn;
	box boxes[8];
	box tb, hb;

	tb = makeflatend (tendp->boxes[tendp->boxn - 1], tside, tdir, lb);
	hb = makeflatend (hendp->boxes[hendp->boxn - 1], hside, OTHERDIR (hdir), rb);

	boxn = 0;
	for (side = tside; ; side = NEXTSIDE (side, tdir)) {
		boxes[boxn++] = makeflatcomponent (lb, rb, side,
				(side == mside) ? 0 : -1, tdir, w, h);
		if (side == mside)
			break;
	}
	if (mside == RIGHT)
		mside = LEFT;
	if (mside != hside) {
		for (side = NEXTSIDE (mside, hdir); ; side = NEXTSIDE (side, hdir)) {
			boxes[boxn++] = makeflatcomponent (lb, rb,
					side, 1, hdir, w, h);
			if (side == hside)
				break;
		}
	}

	for (i = 0; i < tendp->boxn; i++)
		add_box (tendp->boxes[i]);
	if (tb.LL.x != tb.UR.x && tb.LL.y != tb.UR.y)
		add_box (tb);
	for (i = 0; i < boxn; i++)
		add_box (boxes[i]);
	if (hb.LL.x != hb.UR.x && hb.LL.y != hb.UR.y)
		add_box (hb);
	for (i = hendp->boxn - 1; i >= 0 ; i--)
		add_box (hendp->boxes[i]);
}

static box makeflatend (b, side, dir, bb)
	box b;
	int side, dir;
	box bb;
{
	box eb;

	switch (side) {
	case BOTTOM:
		eb = boxof (b.LL.x, bb.LL.y, b.UR.x, b.LL.y);
		if (dir == CCW)
			eb.UR.x += (bb.UR.x - b.UR.x) / 2;
		else
			eb.LL.x -= (b.LL.x - bb.LL.x) / 2;
		break;
	case RIGHT:
		eb = boxof (b.UR.x, b.LL.y, bb.UR.x, b.UR.y);
		if (dir == CCW)
			eb.UR.y += (bb.UR.y - b.UR.y) / 2;
		else
			eb.LL.y -= (b.LL.y - bb.LL.y) / 2;
		break;
	case TOP:
		eb = boxof (b.LL.x, b.UR.y, b.UR.x, bb.UR.y);
		if (dir == CCW)
			eb.LL.x -= (b.LL.x - bb.LL.x) / 2;
		else
			eb.UR.x += (bb.UR.x - b.UR.x) / 2;
		break;
	case LEFT:
		eb = boxof (bb.LL.x, b.LL.y, b.LL.x, b.UR.y);
		if (dir == CCW)
			eb.LL.y -= (bb.UR.y - b.UR.y) / 2;
		else
			eb.UR.y += (b.LL.y - bb.LL.y) / 2;
		break;
	}
	return eb;
}

static box makeflatcomponent (lb, rb, side, mode, dir, w, h)
	box lb, rb;
	int side, mode, dir, w, h;
{
	box b;

	/* mode == -1 means use left box, 1 means use right box
	   and 0 means use mostly the left box */

	switch (side) {
	case BOTTOM:
		b.LL.x = lb.LL.x - w, b.UR.x = rb.UR.x + w;
		if (mode <= 0)
			b.LL.y = lb.LL.y - h, b.UR.y = lb.LL.y;
		else
			b.LL.y = rb.LL.y - h, b.UR.y = rb.LL.y;
		break;
	case RIGHT:
		if (mode == -1) {
			b.LL.x = lb.UR.x, b.UR.x = lb.UR.x + w;
			b.LL.y = lb.LL.y, b.UR.y = lb.UR.y;
		} else if (mode == 0) {
			b.LL.x = lb.UR.x, b.UR.x = lb.UR.x + w;
			if (dir == CCW)
				b.LL.y = lb.LL.y, b.UR.y = rb.UR.y;
			else
				b.LL.y = rb.LL.y, b.UR.y = lb.UR.y;
		} else {
			b.LL.x = rb.UR.x, b.UR.x = rb.UR.x + w;
			b.LL.y = rb.LL.y, b.UR.y = rb.UR.y;
		}
		break;
	case TOP:
		b.LL.x = lb.LL.x - w, b.UR.x = rb.UR.x + w;
		if (mode <= 0)
			b.LL.y = lb.UR.y, b.UR.y = lb.UR.y + h;
		else
			b.LL.y = rb.UR.y, b.UR.y = rb.UR.y + h;
		break;
	case LEFT:
		if (mode == -1) {
			b.LL.x = lb.LL.x - w, b.UR.x = lb.LL.x;
			b.LL.y = lb.LL.y, b.UR.y = lb.UR.y;
		} else if (mode == 0) {
			b.LL.x = lb.LL.x - w, b.UR.x = lb.LL.x;
			if (dir == CCW)
				b.LL.y = lb.LL.y, b.UR.y = rb.UR.y;
			else
				b.LL.y = rb.LL.y, b.UR.y = lb.UR.y;
		} else {
			b.LL.x = rb.LL.x - w, b.UR.x = rb.LL.x;
			b.LL.y = rb.LL.y, b.UR.y = rb.UR.y;
		}
		break;
	}
	return b;
}

/* regular edges */

static void completeregularpath (first, last, tendp, hendp, boxes, boxn, flag)
	edge_t *first, *last;
	pathend_t *tendp, *hendp;
	box *boxes;
	int boxn, flag;
{
	edge_t *uleft, *uright, *lleft, *lright;
	box b;
	int i, y, fb, lb;
	splines *spl;
	point *pp;
	int pn;

	fb = lb = -1;
	uleft = uright = NULL;
	uleft = top_bound (first, -1), uright = top_bound (first, 1);
	if (uleft) {
		spl = getsplinepoints (uleft);
		pp = spl->list[0].list, pn = spl->list[0].size;
		P->ulpp = &pp[0];
	}
	if (uright) {
		spl = getsplinepoints (uright);
		pp = spl->list[0].list, pn = spl->list[0].size;
		P->urpp = &pp[0];
	}
	lleft = lright = NULL;
	lleft = bot_bound (last, -1), lright = bot_bound (last, 1);
	if (lleft) {
		spl = getsplinepoints (lleft);
		pp = spl->list[spl->size - 1].list, pn = spl->list[spl->size - 1].size;
		P->llpp = &pp[pn - 1];
	}
	if (lright) {
		spl = getsplinepoints (lright);
		pp = spl->list[spl->size - 1].list, pn = spl->list[spl->size - 1].size;
		P->lrpp = &pp[pn - 1];
	}
	for (i = 0; i < tendp->boxn; i++)
		add_box (tendp->boxes[i]);
	fb = P->nbox + 1; lb = fb + boxn - 3;
	for (i = 0; i < boxn; i++)
		add_box (boxes[i]);
	for (i = hendp->boxn - 1; i >= 0 ; i--)
		add_box (hendp->boxes[i]);
	adjustregularpath (fb, lb);
}

#if 0 /* box subdivision is obsolete, I think... ek */
static void completeregularpath (first, last, tendp, hendp, boxes, boxn, flag)
	edge_t *first, *last;
	pathend_t *tendp, *hendp;
	box *boxes;
	int boxn, flag;
{
	edge_t *uleft, *uright, *lleft, *lright;
	box uboxes[NSUB], lboxes[NSUB];
	box b;
	int uboxn, lboxn, i, y, fb, lb;

	fb = lb = -1;
	uleft = uright = NULL;
	if (flag || first->tail->u.rank + 1 != last->head->u.rank)
		uleft = top_bound (first, -1), uright = top_bound (first, 1);
	refineregularends (uleft, uright, tendp, 1, boxes[0], uboxes, &uboxn);
	lleft = lright = NULL;
	if (flag || first->tail->u.rank + 1 != last->head->u.rank)
		lleft = bot_bound (last, -1), lright = bot_bound (last, 1);
	refineregularends (lleft, lright, hendp, -1, boxes[boxn - 1], lboxes, &lboxn);
	for (i = 0; i < tendp->boxn; i++)
		add_box (tendp->boxes[i]);
	if (first->tail->u.rank + 1 == last->head->u.rank) {
		if ((!uleft && !uright) && (lleft || lright)) {
			b = boxes[0];
			y = b.UR.y - b.LL.y;
			for (i = 0; i < NSUB; i++) {
				uboxes[i] = b;
				uboxes[i].UR.y = b.UR.y - y * i / NSUB;
				uboxes[i].LL.y = b.UR.y - y * (i + 1) / NSUB;
			}
			uboxn = NSUB;
		} else if ((uleft || uright) && (!lleft && !lright)) {
			b = boxes[boxn - 1];
			y = b.UR.y - b.LL.y;
			for (i = 0; i < NSUB; i++) {
				lboxes[i] = b;
				lboxes[i].UR.y = b.UR.y - y * i / NSUB;
				lboxes[i].LL.y = b.UR.y - y * (i + 1) / NSUB;
			}
			lboxn = NSUB;
		}
		for (i = 0; i < uboxn; i++) {
			uboxes[i].LL.x = MAX (uboxes[i].LL.x, lboxes[i].LL.x);
			uboxes[i].UR.x = MIN (uboxes[i].UR.x, lboxes[i].UR.x);
		}
		for (i = 0; i < uboxn; i++)
			add_box (uboxes[i]);
	} else {
		for (i = 0; i < uboxn; i++)
			add_box (uboxes[i]);
		fb = P->nbox; lb = fb + boxn - 3;
		for (i = 1; i < boxn - 1; i++)
			add_box (boxes[i]);
		for (i = 0; i < lboxn; i++)
			add_box (lboxes[i]);
	}
	for (i = hendp->boxn - 1; i >= 0 ; i--)
		add_box (hendp->boxes[i]);
	adjustregularpath (fb, lb);
}
#endif

/* for now, regular edges always go from top to bottom */
static void makeregularend (b, side, y, bp)
	box b;
	int side;
	int y;
	box *bp;
{
	box eb;

	switch (side) {
	case BOTTOM:
		*bp = boxof (b.LL.x, y, b.UR.x, b.LL.y);
		break;
	case TOP:
		*bp = boxof (b.LL.x, b.UR.y, b.UR.x, y);
		break;
	}
}

#if 0 /* box subdivision is obsolete, I think... ek */
static void refineregularends (left, right, endp, dir, b, boxes, boxnp)
	edge_t *left, *right;
	pathend_t *endp;
	int dir;
	box b;
	box *boxes;
	int *boxnp;
{
	splines *lspls, *rspls;
	point pp, cp;
	box eb;
	box *bp;
	int y, i, j, k;

	if (!left && !right) {
		boxes[0] = b;
		*boxnp = 1;
		return;
	}
	y = b.UR.y - b.LL.y;
	for (i = 0; i < NSUB; i++) {
		boxes[i] = b;
		boxes[i].UR.y = b.UR.y - y * i / NSUB;
		boxes[i].LL.y = b.UR.y - y * (i + 1) / NSUB;
	}
	*boxnp = NSUB;
	/* only break big boxes */
	for (j = 0; j < endp->boxn; j++) {
		eb = endp->boxes[j];
		y = eb.UR.y - eb.LL.y;
		if (y < 15)
			continue;
		for (k = endp->boxn - 1; k > j; k--)
			endp->boxes[k + 2] = endp->boxes[k];
		for (i = 0; i < NSUB; i++) {
			bp = &endp->boxes[j + ((dir == 1) ? i : (NSUB - i - 1))];
			*bp = eb;
			bp->UR.y = eb.UR.y - y * i / NSUB;
			bp->LL.y = eb.UR.y - y * (i + 1) / NSUB;
		}
		endp->boxn += (NSUB - 1);
		j += 2;
	}
	if (left) {
		lspls = getsplinepoints (left);
		pp = spline_at_y (lspls, boxes[0].UR.y);
		for (i = 0; i < NSUB; i++) {
			cp = spline_at_y (lspls, boxes[i].LL.y);
			/*boxes[i].LL.x = AVG (pp.x, cp.x);*/
			boxes[i].LL.x = AVG (pp.x, cp.x);
			pp = cp;
		}
		pp = spline_at_y (lspls, (dir == 1) ?
				endp->boxes[1].UR.y : endp->boxes[1].LL.y);
		for (i = 1; i < endp->boxn; i++) {
			cp = spline_at_y (lspls, (dir == 1) ?
					endp->boxes[i].LL.y : endp->boxes[i].UR.y);
			endp->boxes[i].LL.x = MIN (endp->nb.UR.x, AVG (pp.x, cp.x));
			pp = cp;
		}
		i = (dir == 1) ? 0 : *boxnp - 1;
		if (boxes[i].LL.x > endp->boxes[endp->boxn - 1].UR.x - MINW)
			boxes[i].LL.x = endp->boxes[endp->boxn - 1].UR.x - MINW;
	}
	if (right) {
		rspls = getsplinepoints (right);
		pp = spline_at_y (rspls, boxes[0].UR.y);
		for (i = 0; i < NSUB; i++) {
			cp = spline_at_y (rspls, boxes[i].LL.y);
			/*boxes[i].UR.x = AVG (pp.x, cp.x);*/
			boxes[i].UR.x = AVG (pp.x, cp.x);
			pp = cp;
		}
		pp = spline_at_y (rspls, (dir == 1) ?
				endp->boxes[1].UR.y : endp->boxes[1].LL.y);
		for (i = 1; i < endp->boxn; i++) {
			cp = spline_at_y (rspls, (dir == 1) ?
					endp->boxes[i].LL.y : endp->boxes[i].UR.y);
			endp->boxes[i].UR.x = MAX (endp->nb.LL.x, AVG (pp.x, cp.x));
			pp = cp;
		}
		i = (dir == 1) ? 0 : *boxnp - 1;
		if (boxes[i].UR.x < endp->boxes[endp->boxn - 1].LL.x + MINW)
			boxes[i].UR.x = endp->boxes[endp->boxn - 1].LL.x + MINW;
	}
}
#endif

static void adjustregularpath (fb, lb)
{
	box *bp1, *bp2;
	int i, x;

	for (i = 0; i < P->nbox; i++) {
		bp1 = &P->boxes[i];
		if ((i - fb) % 2 == 0) {
			if (bp1->LL.x >= bp1->UR.x) {
				x = (bp1->LL.x + bp1->UR.x) / 2;
				bp1->LL.x = x - HALFMINW, bp1->UR.x = x + HALFMINW;
			}
		} else {
			if (bp1->LL.x + MINW > bp1->UR.x) {
				x = (bp1->LL.x + bp1->UR.x) / 2;
				bp1->LL.x = x - HALFMINW, bp1->UR.x = x + HALFMINW;
			}
		}
	}
	for (i = 0; i < P->nbox - 1; i++) {
		bp1 = &P->boxes[i], bp2 = &P->boxes[i + 1];
		if (i >= fb && i <= lb && (i - fb) % 2 == 0) {
			if (bp1->LL.x + MINW > bp2->UR.x)
				bp2->UR.x = bp1->LL.x + MINW;
			if (bp1->UR.x - MINW < bp2->LL.x)
				bp2->LL.x = bp1->UR.x - MINW;
		} else if (i + 1 >= fb && i < lb && (i + 1 - fb) % 2 == 0) {
			if (bp1->LL.x + MINW > bp2->UR.x)
				bp1->LL.x = bp2->UR.x - MINW;
			if (bp1->UR.x - MINW < bp2->LL.x)
				bp1->UR.x = bp2->LL.x + MINW;
		} else {
			if (bp1->LL.x + MINW > bp2->UR.x) {
				x = (bp1->LL.x + bp2->UR.x) / 2;
				bp1->LL.x = x - HALFMINW;
				bp2->UR.x = x + HALFMINW;
			}
			if (bp1->UR.x - MINW < bp2->LL.x) {
				x = (bp1->UR.x + bp2->LL.x) / 2;
				bp1->UR.x = x + HALFMINW;
				bp2->LL.x = x - HALFMINW;
			}
		}
	}
}

static box rank_box (g, r)
	graph_t *g;
	int r;
{
	box b;
	node_t *right0, *right1, *left0, *left1;

	b = Rank_box[r];
	if (b.LL.x == b.UR.x) {
		left0  = g->u.rank[r].v[0];
		right0 = g->u.rank[r].v[g->u.rank[r].n - 1];
		left1  = g->u.rank[r + 1].v[0];
		right1 = g->u.rank[r + 1].v[g->u.rank[r + 1].n - 1];
		b.LL.x = LeftBound;
		b.UR.y = left0->u.coord.y - g->u.rank[r].ht2;
		b.UR.x = RightBound;
		b.LL.y = left1->u.coord.y + g->u.rank[r + 1].ht2;
		Rank_box[r] = b;
	}
	return b;
}

/* returns count of vertically aligned edges starting at n */
static int straight_len (n)
	node_t *n;
{
	int cnt = 0;
	node_t *v;

	v = n;
	while (1) {
		v = v->u.out.list[0]->head;
		if (v->u.node_type != VIRTUAL)
			break;
		if ((v->u.out.size != 1) || (v->u.in.size != 1))
			break;
		if (v->u.coord.x != n->u.coord.x)
			break;
		cnt++;
	}
	return cnt;
}

static edge_t *straight_path (e, cnt, plist, np)
	edge_t *e;
	int cnt;
	point *plist;
	int *np;
{
	int n = *np;
	edge_t *f = e;

	while (cnt--)
		f = f->head->u.out.list[0];
	plist[(*np)++] = plist[n - 1];
	plist[(*np)++] = plist[n - 1];
	plist[(*np)] = f->tail->u.coord;	/* will be overwritten by next spline */
	return f;
}

static void recover_slack (e, p)
	edge_t *e;
	path *p;
{
	int		b;
	node_t	*vn;

	b = 0;		/* skip first rank box */
	for (vn = e->head; vn->u.node_type == VIRTUAL && !spline_merge (vn);
			vn = vn->u.out.list[0]->head) {
		while ((b < p->nbox) && (p->boxes[b].LL.y > vn->u.coord.y))
			b++;
		if (b >= p->nbox)
			break;
		if (p->boxes[b].UR.y < vn->u.coord.y)
			continue;
		if (vn->u.label)
			resize_vn (vn, p->boxes[b].LL.x, p->boxes[b].UR.x,
					p->boxes[b].UR.x + vn->u.rw);
		else
			resize_vn (vn, p->boxes[b].LL.x, (p->boxes[b].LL.x +
					p->boxes[b].UR.x) / 2, p->boxes[b].UR.x);
	}
}

static void resize_vn (vn, lx, cx, rx)
	node_t *vn;
	int lx, cx, rx;
{
	vn->u.coord.x = cx;
	vn->u.lw = cx - lx, vn->u.rw = rx - cx;
}

/* side > 0 means right. side < 0 means left */
static edge_t *top_bound (e, side)
	edge_t *e;
	int side;
{
	edge_t *f, *ans = NULL;
	int i;

	for (i = 0; f = e->tail->u.out.list[i]; i++) {
		if (e->u.tail_port.p.x != f->u.tail_port.p.x)
			continue;
		if (side * (f->head->u.order - e->head->u.order) <= 0)
			continue;
		if ((f->u.spl == NULL) && ((f->u.to_orig == NULL) || (f->u.to_orig->u.spl == NULL)))
			continue;
		if ((ans == NULL) || (side * (ans->head->u.order - f->head->u.order) > 0))
			ans = f;
	}
	return ans;
}

static edge_t *bot_bound (e, side)
	edge_t *e;
	int side;
{
	edge_t *f, *ans = NULL;
	int i;

	for (i = 0; f = e->head->u.in.list[i]; i++) {
		if (e->u.head_port.p.x != f->u.head_port.p.x)
			continue;
		if (side * (f->tail->u.order - e->tail->u.order) <= 0)
			continue;
		if ((f->u.spl == NULL) && ((f->u.to_orig == NULL) || (f->u.to_orig->u.spl == NULL)))
			continue;
		if ((ans == NULL) || (side * (ans->tail->u.order - f->tail->u.order) > 0))
			ans = f;
	}
	return ans;
}


point spline_at_y(spl,y)
	splines *spl;
	int y;
{
	int i,j;
	double low, high, d, t;
	pointf c[4], pt2;
	point pt;
	static bezier bz;
	static splines *mem = NULL;

	if (mem != spl) {
		mem = spl;
		for (i = 0; i < spl->size; i++) {
			bz = spl->list[i];
			if (BETWEEN (bz.list[bz.size-1].y, y, bz.list[0].y))
				break;
		}
	}
	if (y > bz.list[0].y)
		pt = bz.list[0];
	else if (y < bz.list[bz.size-1].y)
		pt = bz.list[bz.size - 1];
	else {
		for (i = 0; i < bz.size; i += 3) {
			for (j = 0; j < 3; j++) {
				if ((bz.list[i+j].y <= y) && (y <= bz.list[i+j+1].y))
					break;
				if ((bz.list[i+j].y >= y) && (y >= bz.list[i+j+1].y))
					break;
			}
			if (j < 3)
				break;
		}
		assert (i < bz.size);
		for (j = 0; j < 4; j++) {
			c[j].x = bz.list[i + j].x;
			c[j].y = bz.list[i + j].y;
			/* make the spline be monotonic in Y, awful but it works for now */
			if ((j > 0) && (c[j].y > c[j - 1].y))
				c[j].y = c[j - 1].y;
		}
		low = 0.0; high = 1.0;
		do {
			t = (low + high) / 2.0;
			pt2 = Bezier (c, 3, t, NULL, NULL);
			d = pt2.y - y;
			if (ABS(d) <= 1)
				break;
			if (d < 0)
				high = t;
			else
				low = t;
		} while (1);
		pt.x = pt2.x;
		pt.y = pt2.y;
	}
	pt.y = y;
	return pt;
}

/* common routines */

static double conc_slope(n)
	node_t	*n;
{
	double	s_in, s_out,m_in,m_out;
	int		cnt_in,cnt_out;
	pointf	p;
	edge_t	*e;

	s_in = s_out = 0.0;
	for (cnt_in = 0; e = n->u.in.list[cnt_in]; cnt_in++)
		s_in += e->tail->u.coord.x;
	for (cnt_out = 0; e = n->u.out.list[cnt_out]; cnt_out++)
		s_out += e->head->u.coord.x;
	p.x = n->u.coord.x - (s_in / cnt_in);
	p.y = n->u.coord.y - n->u.in.list[0]->tail->u.coord.y;
	m_in = atan2(p.y,p.x);
	p.x = (s_out / cnt_out) - n->u.coord.x;
	p.y = n->u.out.list[0]->head->u.coord.y - n->u.coord.y;
	m_out = atan2(p.y,p.x);
	return ((m_in + m_out) / 2.0);
}

static void beginpath (e, et, endp) /* build boxes around the tail */
	edge_t *e;
	int et;
	pathend_t *endp;
{
	node_t *n;
	int ((*pboxfn) ());

	n = e->tail;
        if (n->u.shape)
		pboxfn = n->u.shape->pboxfn;
	else
		pboxfn = NULL;
	P->start.p = add_points (n->u.coord, e->u.tail_port.p);
	P->ulpp = P->urpp = P->llpp = P->lrpp = NULL;
	if (spline_merge (e->tail)) {
		/*P->start.theta = - PI / 2;*/
		P->start.theta = conc_slope(e->tail);
		P->start.constrained = TRUE;
	} else {
		if (e->u.tail_port.constrained) {
			P->start.theta = e->u.tail_port.theta;
			P->start.constrained = TRUE;
		} else
			P->start.constrained = FALSE;
	}
	P->nbox = 0;
	P->data = (void*)e;
	endp->np = P->start.p;
	/* FIXME: check that record_path returns a good path */
	if (pboxfn)
		endp->sidemask = (*pboxfn) (n, e, 1,
				&endp->boxes[0], &endp->boxn);
	else {
		endp->boxes[0] = endp->nb;
		endp->boxn = 1;
	}
	switch (et) {
	case SELFEDGE:
		endp->boxes[0].UR.y = P->start.p.y;
		endp->sidemask = BOTTOM;
		break;
	case FLATEDGE:
		endp->boxes[0].LL.y = P->start.p.y;
		endp->sidemask = TOP;
		break;
	case REGULAREDGE:
		endp->boxes[0].UR.y = P->start.p.y;
		endp->sidemask = BOTTOM;
		break;
	}
}

static void endpath (e, et, endp) /* build boxes around the head */
	edge_t *e;
	int et;
	pathend_t *endp;
{
	node_t *n;
	int ((*pboxfn) ());

	n = e->head;
        if (n->u.shape)
		pboxfn = n->u.shape->pboxfn;
	else
		pboxfn = NULL;
	P->end.p = add_points (n->u.coord, e->u.head_port.p);
	if (spline_merge (e->head)) {
		/*P->end.theta = PI / 2;*/
		P->end.theta = conc_slope(e->head) + PI;
		assert(P->end.theta < 2*PI);
		P->end.constrained = TRUE;
	} else {
		if (e->u.head_port.constrained) {
			P->end.theta = e->u.head_port.theta;
			P->end.constrained = TRUE;
		} else
			P->end.constrained = FALSE;
	}
	endp->np = P->end.p;
	if (pboxfn)
		endp->sidemask = (*pboxfn) (n, e, 2,
				&endp->boxes[0], &endp->boxn);
	else {
		endp->boxes[0] = endp->nb;
		endp->boxn = 1;
	}
	switch (et) {
	case SELFEDGE:
		endp->boxes[0].LL.y = P->end.p.y;
		endp->sidemask = TOP;
		break;
	case FLATEDGE:
		endp->boxes[0].LL.y = P->end.p.y;
		endp->sidemask = TOP;
		break;
	case REGULAREDGE:
		endp->boxes[0].LL.y = P->end.p.y;
		endp->sidemask = TOP;
		break;
	}
}

static edge_t *getmainedge (e)
	edge_t *e;
{
	edge_t *le = e;
	while (le->u.to_virt)
		le = le->u.to_virt;
	while (le->u.to_orig)
		le = le->u.to_orig;
	return le;
}

static splines *getsplinepoints (e)
	edge_t *e;
{
	edge_t *le;
	splines *sp;

	for (le = e; !(sp = le->u.spl) && le->u.edge_type != NORMAL; le = le->u.to_orig) ;
	if (sp == NULL) abort ();
	return sp;
}

static int
cl_vninside(cl, n)
graph_t	*cl;
node_t	*n;
{
	return (BETWEEN(cl->u.bb.LL.x,n->u.coord.x,cl->u.bb.UR.x) &&
		BETWEEN(cl->u.bb.LL.y,n->u.coord.y,cl->u.bb.UR.y));
}

/* returns the cluster of (adj) that interferes with n,
 */
static graph_t * 
cl_bound(n, adj)
node_t		*n,*adj;
{
    graph_t     *rv,*cl,*tcl,*hcl;
	edge_t		*orig;

    rv = NULL;
	if (n->u.node_type == NORMAL) tcl = hcl = n->u.clust;
	else {
		orig = n->u.out.list[0]->u.to_orig;
		tcl = orig->tail->u.clust; hcl = orig->head->u.clust;
	}
    if (adj->u.node_type == NORMAL) {
        cl = adj->u.clust;
        if (cl && (cl != tcl) && (cl != hcl)) rv = cl;
    }
    else {
        orig = adj->u.out.list[0]->u.to_orig;
        cl = orig->tail->u.clust;
        if (cl && (cl != tcl) && (cl != hcl) && cl_vninside(cl,adj)) rv=cl;
        else {
            cl = orig->head->u.clust;
            if (cl && (cl != tcl) && (cl != hcl) && cl_vninside(cl,adj)) rv=cl;
        }
    }
    return rv;
}

static box maximal_bbox (vn, ie, oe)
	node_t *vn;
	edge_t *ie, *oe;
{
    int     nb,b,ht2;
    graph_t *g = vn->graph, *left_cl,*right_cl;
    node_t  *left,*right;
    box     rv;

    ht2 = g->u.rank[vn->u.rank].ht2;
	left_cl = right_cl = NULL;

    /* give this node all the available space up to its neighbors */
	b = vn->u.coord.x - vn->u.lw;
    if (left = neighbor(vn, ie, oe, -1)) {
		if (left_cl = cl_bound(vn, left)) 
			nb = left_cl->u.bb.UR.x + Splinesep;
        else {
			nb = left->u.coord.x + left->u.mval;
			if (left->u.node_type == NORMAL) nb += g->u.nodesep/2;
			else nb += Splinesep;
		}
		if (b > nb) b = nb;
		rv.LL.x = b;
    }
    else rv.LL.x = MIN(b,LeftBound);

	b = vn->u.coord.x + vn->u.rw;
    if (right = neighbor(vn, ie, oe, 1)) {
		if (right_cl = cl_bound(vn, right))
            nb = right_cl->u.bb.LL.x - Splinesep;
        else {
			nb = right->u.coord.x - right->u.lw;
			if (right->u.node_type == NORMAL) nb -= g->u.nodesep/2;
			else nb -= Splinesep;
		}
		if (b < nb) b = nb;
		rv.UR.x = b;
    }
    else rv.UR.x = MAX(b,RightBound);

    if ((vn->u.node_type == VIRTUAL) && (vn->u.label))
        rv.UR.x -= vn->u.rw;

    rv.LL.y = vn->u.coord.y - ht2;
    rv.UR.y = vn->u.coord.y + ht2;
    return rv;
}

static node_t *neighbor (vn, ie, oe, dir)
	node_t *vn;
	edge_t *ie, *oe;
	int dir;
{
	int i;
	node_t *n, *rv = NULL;
	rank_t *rank = &(vn->graph->u.rank[vn->u.rank]);

	for (i = vn->u.order + dir; ((i >= 0) && (i < rank->n)); i += dir) {
		n = rank->v[i];
		if ((n->u.node_type == VIRTUAL) && (n->u.label)) {
			rv=n;
			break;
		}
		if (n->u.node_type == NORMAL) {
			rv = n;
			break;
		}
		if (pathscross(n, vn, ie, oe) == FALSE) {
			rv = n;
			break;
		}
	}
	return rv;
}

static boolean pathscross (n0, n1, ie1, oe1)
	node_t *n0, *n1;
	edge_t *ie1, *oe1;
{
	edge_t *e0, *e1;
	node_t *na, *nb;
	int order, cnt;

	order = (n0->u.order > n1->u.order);
	if ((n0->u.out.size != 1) && (n0->u.out.size != 1))
		return FALSE;
	e1 = oe1;
	if (n0->u.out.size == 1 && e1) {
		e0 = n0->u.out.list[0];
		for (cnt = 0; cnt < 2; cnt++) {
			if ((na = e0->head) == (nb = e1->head))
				break;
			if (order != (na->u.order > nb->u.order))
				return TRUE;
			if ((na->u.out.size != 1) || (na->u.node_type == NORMAL))
				break;
			e0 = na->u.out.list[0];
			if ((nb->u.out.size != 1) || (nb->u.node_type == NORMAL))
				break;
			e1 = nb->u.out.list[0];
		}
	}
	e1 = ie1;
	if (n0->u.in.size == 1 && e1) {
		e0 = n0->u.in.list[0];
		for (cnt = 0; cnt < 2; cnt++) {
			if ((na = e0->tail) == (nb = e1->tail))
				break;
			if (order != (na->u.order > nb->u.order))
				return TRUE;
			if ((na->u.in.size != 1) || (na->u.node_type == NORMAL))
				break;
			e0 = na->u.in.list[0];
			if ((nb->u.in.size != 1) || (nb->u.node_type == NORMAL))
				break;
			e1 = nb->u.in.list[0];
		}
	}
	return FALSE;
}

static void add_box (b)
	box b;
{
	P->boxes[P->nbox++] = b;
}

static void clip_and_install (fe, le, ps, pn)
	edge_t *fe, *le;
	point *ps;	/* spline points */
	int pn;		/* number of points */
{
	pointf p2;
	bezier *newspl;
	node_t *tn, *hn;
	int start, end, i;
	graph_t	*g;

	tn = fe->tail, hn = le->head;
	g = tn->graph;
	newspl = new_spline (fe, pn);
		/* spline may be interior to node */
	for (start = 0; start < pn - 4; start+=3) {
		p2.x = ps[start+3].x - tn->u.coord.x;
		p2.y = ps[start+3].y - tn->u.coord.y;
		if (tn->u.shape == NULL)
			break;
		if (tn->u.shape->insidefn == NULL)
			break;
		if (tn->u.shape->insidefn (tn, p2, fe) == FALSE)
			break;
	}
	shape_clip (tn, &ps[start], fe);
	for (end = pn - 4; end > 0; end -= 3) {
		p2.x = ps[end].x - hn->u.coord.x;
		p2.y = ps[end].y - hn->u.coord.y;
		if (hn->u.shape == NULL)
			break;
		if (hn->u.shape->insidefn == NULL)
			break;
		if (hn->u.shape->insidefn (hn, p2, le) == FALSE)
			break;
	}
	shape_clip (hn, &ps[end], le);
	for (; start < pn - 4; start+=3)
		if (ps[start].x != ps[start + 3].x || ps[start].y != ps[start + 3].y)
			break;
	for (; end > 0; end -= 3)
		if (ps[end].x != ps[end + 3].x || ps[end].y != ps[end + 3].y)
			break;
	arrow_clip (fe, le, ps, &start, &end, newspl);
	for (i = start; i < end + 4; i++) {
		point		pt;
		pt = newspl->list[i - start] = ps[i];
		update_bb(g,pt);
	}
	newspl->size = end - start + 4;
}

static bezier *new_spline (e, sz)
	edge_t *e;
	int sz;
{
	bezier *rv;

	while (e->u.edge_type != NORMAL)
		e = e->u.to_orig;
	if (e->u.spl == NULL)
		e->u.spl = NEW (splines);
	e->u.spl->list = ALLOC (e->u.spl->size + 1, e->u.spl->list, bezier);
	rv = &(e->u.spl->list[e->u.spl->size++]);
	rv->list = N_NEW (sz, point);
	rv->size = sz;
	rv->sflag = rv->eflag = FALSE;
	return rv;
}

static void shape_clip (n, curve, e)
	node_t *n;
	point curve[4];
	edge_t *e;
{
	int i, save_real_size;
	boolean found, inside, left_inside;
	pointf pt, opt, c[4], seg[4], best[4], *left, *right;
	double low, high, t;

	if (n->u.shape == NULL)
		return;
	if (n->u.shape->insidefn == NULL)
		return;
	save_real_size = n->u.rw;
#if 0 /* this should be wrong now, since make_self_edge has already swapped it */
	n->u.rw = n->u.mval;	/* please look the other way */
#endif
	for (i = 0; i < 4; i++) {
		c[i].x = curve[i].x - n->u.coord.x;
		c[i].y = curve[i].y - n->u.coord.y;
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
		curve[i].x = ROUND(best[i].x + n->u.coord.x);
		curve[i].y = ROUND(best[i].y + n->u.coord.y);
	}
	n->u.rw = save_real_size;
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
		else if (t < 0.1)
			t = 0.1;
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
		else if (t < 0.1)
			t = 0.1;
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

portcmp(p0, p1)
port_t          p0,p1;
{
        int             rv;
        if (p1.defined == FALSE)
                return (p0.defined ? 1 : 0);
        if (p0.defined == FALSE)
                return -1;
        rv = p0.p.x - p1.p.x;
        if (rv == 0) rv = p0.p.y - p1.p.y;
        return rv;
}
