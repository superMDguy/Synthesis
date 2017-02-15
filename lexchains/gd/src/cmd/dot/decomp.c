/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/* 
 * Decompose finds the connected components of a graph.
 * It searches the temporary edges and ignores non-root nodes.
 * The roots of the search are the real nodes of the graph,
 * but any virtual nodes discovered are also included in the
 * component.
 */

#include "dot.h"

static graph_t		*G;
static node_t		*Last_node;
static char			Cmark;

begin_component()
{
	Last_node = G->u.nlist = NULL;
}

add_to_component(n)
node_t	*n;
{
	G->u.n_nodes++;
	n->u.mark = Cmark;
	if (Last_node) {
		n->u.prev = Last_node;
		Last_node->u.next = n;
	}
	else {
		n->u.prev = NULL;
		G->u.nlist = n;
	}
	Last_node = n;
	n->u.next = NULL;
}

end_component()
{
	int		i;

	i = G->u.comp.size++;
	G->u.comp.list = ALLOC(G->u.comp.size,G->u.comp.list,node_t*);
	G->u.comp.list[i] = G->u.nlist;
}

search_component(g,n)
graph_t		*g;
node_t		*n;
{
	int		c,i;
	elist	vec[4];
	node_t	*other;
	edge_t	*e;

	add_to_component(n);
	vec[0] = n->u.out;		vec[1] = n->u.in;
	vec[2] = n->u.flat_out;	vec[3] = n->u.flat_in;

	for (c = 0; c <= 3; c++) {
		if (vec[c].list) for (i = 0; e = vec[c].list[i]; i++) {
			if ((other = e->head) == n) other = e->tail;
			if ((other->u.mark != Cmark) && (other == UF_find(other)))
				search_component(g,other);
		}
	}
}

decompose(g,pass)
graph_t		*g;
{
	graph_t	*subg;
	node_t	*n,*v;

	G = g;
	if (++Cmark == 0) Cmark = 1;
	g->u.n_nodes = g->u.comp.size = 0;
	for (n = agfstnode(g); n; n = agnxtnode(g,n)) {
		v = n;
		if ((pass > 0) && (subg = v->u.clust))
			v = subg->u.rankleader[v->u.rank];
		else if (v != UF_find(v)) continue;
		if (v->u.mark != Cmark) {
			begin_component();
			search_component(g,v);
			end_component();
		}
	}
}
