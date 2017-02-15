/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/*
 * Break cycles in a directed graph by depth-first search.
 */

#include "dot.h"

acyclic(g)
graph_t	*g;
{
	int		c;
	node_t	*n;

	for (c = 0; c < g->u.comp.size; c++) {
		g->u.nlist = g->u.comp.list[c];
		for (n = g->u.nlist; n; n = n->u.next) n->u.mark = FALSE;
		for (n = g->u.nlist; n; n = n->u.next) dfs(n);
	}
}

dfs(n)
node_t	*n;
{
	int		i;
	edge_t	*e;
	node_t	*w;
	
	if (n->u.mark) return;
	n->u.mark = TRUE;
	n->u.onstack = TRUE;
	for (i = 0; e = n->u.out.list[i]; i++) {
		w = e->head;
		if (w->u.onstack) { reverse_edge(e); i--; }
		else { if (w->u.mark == FALSE) dfs(w); }
	}
	n->u.onstack = FALSE;
}

reverse_edge(e)
edge_t		*e;
{
	edge_t		*f;

	delete_fast_edge(e);
	if (f = find_fast_edge(e->head,e->tail)) merge_oneway(e,f);
	else virtual_edge(e->head,e->tail,e);
}
