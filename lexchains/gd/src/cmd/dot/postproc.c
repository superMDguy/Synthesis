/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dot.h"

static boolean	Flip;
static point	Offset;

#define M1 \
	"/pathbox { /Y exch %d sub def /X exch %d sub def /y exch %d sub def /x exch %d sub def newpath x y moveto X y lineto X Y lineto x Y lineto closepath stroke } def\n"
#define M2 \
	"/pathbox { /X exch neg %d sub def /Y exch %d sub def /x exch neg %d sub def /y exch %d sub def newpath x y moveto X y lineto X Y lineto x Y lineto closepath stroke } def\n"

dot_postprocess(g)
graph_t	*g;
{
	if (Flip = g->u.left_to_right) {
		if (g->u.label) {
			int		yd = POINTS(g->u.label->dimen.x);
			g->u.bb.LL.x -= POINTS(g->u.label->dimen.y);
				/* in case label is wide than the rest of the drawing */
			if (yd > g->u.bb.UR.y - g->u.bb.LL.y) {
				yd = yd/2;
				g->u.bb.LL.y -= yd; g->u.bb.UR.y += yd;
			}
		}
		Offset.x = -g->u.bb.UR.y;
		Offset.y = g->u.bb.LL.x;
	}
	else {
		if (g->u.label) {
			int		xd = POINTS(g->u.label->dimen.x);
			g->u.bb.LL.y -= POINTS(g->u.label->dimen.y);
			if (xd > g->u.bb.UR.x - g->u.bb.LL.x) {
				xd = xd/2;
				g->u.bb.LL.x -= xd; g->u.bb.UR.x += xd;
			}
		}
		Offset = g->u.bb.LL;
	}
	translate_drawing(g);
	place_graph_label(g);

	if (Show_boxes) {
		if (Flip)
			fprintf (stderr, M2, Offset.x, Offset.y, Offset.x, Offset.y);
		else
			fprintf (stderr, M1, Offset.y, Offset.x, Offset.y, Offset.x);
	}
}

point
map_point(p)
point		p;
{
	int		x = p.x;
	int 	y = p.y;

	if (Flip) { p.x = -y - Offset.x; p.y = x - Offset.y; }
	else { p.x = x - Offset.x; p.y = y - Offset.y; }
	return p;
}

map_edge(e)
edge_t	*e;
{
	int			j,k;
	bezier		bz;

if (e->u.spl == NULL) {
	if ((Concentrate == FALSE) || (e->u.edge_type != IGNORED))
		fprintf(stderr,"lost %s %s edge\n",e->tail->name,e->head->name);
	return;
}
	for (j = 0; j < e->u.spl->size; j++) {
		bz = e->u.spl->list[j];
		for (k = 0; k < bz.size; k++)
			bz.list[k] = map_point(bz.list[k]);
		if (bz.sflag)
			e->u.spl->list[j].sp = map_point (e->u.spl->list[j].sp);
		if (bz.eflag)
			e->u.spl->list[j].ep = map_point (e->u.spl->list[j].ep);
	}
	if (e->u.label) e->u.label->p = map_point(e->u.label->p);
}

translate_bb(g,lr)
graph_t		*g;
int			lr;
{
	int			c;
	box			bb,new_bb;

	bb = g->u.bb;
	if (lr) {
		new_bb.LL = map_point(pointof(bb.LL.x,bb.UR.y));
		new_bb.UR = map_point(pointof(bb.UR.x,bb.LL.y));
	}
	else {
		new_bb.LL = map_point(pointof(bb.LL.x,bb.LL.y));
		new_bb.UR = map_point(pointof(bb.UR.x,bb.UR.y));
	}
	g->u.bb = new_bb;
	for (c = 1; c <= g->u.n_cluster; c++) translate_bb(g->u.clust[c],lr);
}

translate_drawing(g)
graph_t		*g;
{
	node_t		*v;
	edge_t		*e;

	for (v = agfstnode(g); v; v = agnxtnode(g,v)) {
		nodesize(v,FALSE);
		v->u.coord = map_point(v->u.coord);
		for (e = agfstout(g,v); e; e = agnxtout(g,e)) map_edge(e);
	}
	translate_bb(g,g->u.left_to_right);
}

osize_label(label,b,t,l,r)
textlabel_t	*label;
int		*b,*t,*l,*r;
{
	point	pt,sz2;
	sz2.x = POINTS(label->dimen.x)/2;
	sz2.y = POINTS(label->dimen.y)/2;
	pt = add_points(label->p,sz2);
	if (*r < pt.x) *r = pt.x;
	if (*t < pt.y) *t = pt.y;
	pt = sub_points(label->p,sz2);
	if (*l > pt.x) *l = pt.x;
	if (*b > pt.y) *b = pt.y;
}

place_graph_label(g)
graph_t	*g;
{
	int			c,maxy;
	point		p,d;

	if (g->u.label) {
		d = cvt2pt(g->u.label->dimen);
		if (g == g->root) {
			p.x = (g->u.bb.LL.x + g->u.bb.UR.x)/2;
			p.y = g->u.bb.LL.y + d.y/2;
		}
		else {	/* cluster */
			p.x = g->u.bb.LL.x + d.x/2;
			p.y = g->u.bb.UR.y + d.y/2;
			maxy = g->u.bb.UR.y + d.y;
			if (g->root->u.bb.UR.y < maxy) g->root->u.bb.UR.y = maxy;
		}
		g->u.label->p = p;
	}
	for (c = 1; c <= g->u.n_cluster; c++)
		place_graph_label(g->u.clust[c]);
}
