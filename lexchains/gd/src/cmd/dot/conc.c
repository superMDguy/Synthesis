/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/*
 *	build edge_t concentrators for parallel edges with a common endpoint
 */

#include	"dot.h"

#define		UP		0
#define		DOWN	1

static boolean
samedir(e,f)
edge_t	*e,*f;
{
	edge_t	*e0,*f0;

	for (e0 = e; e0->u.edge_type != NORMAL; e0 = e0->u.to_orig);
	for (f0 = f; f0->u.edge_type != NORMAL; f0 = f0->u.to_orig);
	return ((f0->tail->u.rank - f0->head->u.rank)
		*(e0->tail->u.rank - e0->head->u.rank) > 0);
}

boolean
downcandidate(v)
node_t	*v;
{
	return ((v->u.node_type == VIRTUAL) && (v->u.in.size == 1) && (v->u.out.size == 1) &&(v->u.label == NULL));
}

boolean
bothdowncandidates(u,v)
node_t	*u,*v;
{
	edge_t	*e,*f;
	if ((downcandidate(v) && (v->u.in.list[0]->tail == u->u.in.list[0]->tail))){
		e = u->u.in.list[0];
		f = v->u.in.list[0];
		return samedir(e,f);
	}
	return FALSE;
}

boolean
upcandidate(v)
node_t	*v;
{
	return ((v->u.node_type == VIRTUAL) && (v->u.out.size == 1) && (v->u.in.size == 1) && (v->u.label == NULL));
}

boolean
bothupcandidates(u,v)
node_t	*u,*v;
{
	edge_t	*e,*f;
	if ((upcandidate(v) && (v->u.out.list[0]->head == u->u.out.list[0]->head))){
		e = u->u.out.list[0];
		f = v->u.out.list[0];
		return samedir(e,f);
	}
	return FALSE;
}

mergevirtual(g,r,lpos,rpos,dir)
graph_t	*g;
int		r,lpos,rpos,dir;
{
	int		i,k;
	node_t	*left,*right;
	edge_t	*e,*f,*e0;

	left = g->u.rank[r].v[lpos];
	/* merge all right nodes into the leftmost one */
	for (i = lpos + 1; i <= rpos; i++) {
		right = g->u.rank[r].v[i];
		if (dir == DOWN) {
			while (e = right->u.out.list[0]) {
				for (k = 0; f = left->u.out.list[k]; k++)
					if (f->head == e->head) break;
				if (f == NULL) f = virtual_edge(left,e->head,e);
				while (e0 = right->u.in.list[0]) {
					merge_oneway(e0,f);
					/*f->u.weight += e0->u.weight;*/
					delete_fast_edge(e0);
				}
				delete_fast_edge(e);
			}
		}
		else {
			while (e = right->u.in.list[0]) {
				for (k = 0; f = left->u.in.list[k]; k++)
					if (f->tail == e->tail) break;
				if (f == NULL) f = virtual_edge(e->tail,left,e);
				while (e0 = right->u.out.list[0]) {
					merge_oneway(e0,f);
					delete_fast_edge(e0);
				}
				delete_fast_edge(e);
			}
		}
		assert(right->u.in.size + right->u.out.size == 0);
		delete_fast_node(g,right);
	}
	k = lpos + 1;
	i = rpos + 1;
	while (i < g->u.rank[r].n) {
		node_t	*n;
		n = g->u.rank[r].v[k] = g->u.rank[r].v[i];
		n->u.order = k;
		k++; i++;
	}
	g->u.rank[r].n = k;
	g->u.rank[r].v[k] = NULL;
}

static void remove_multi_edges(g)
graph_t	*g;
{
	int		i;
	node_t	*v;
	edge_t	*e;

	for (v = g->u.nlist; v; v = v->u.next) {
		if (v->u.other.list) for (i = 0; e = v->u.other.list[i]; i++) {
			if (e->u.to_virt) {
				e->u.count = 0;		/* this is a great badness */
				unmerge_oneway(e);
				delete_other_edge(e);
				e->u.edge_type = IGNORED;
				i--;
			}
		}
	}
}

dot_concentrate(g)
graph_t	*g;
{
	int		c,r,leftpos,rightpos;
	node_t	*left,*right;

	remove_multi_edges(g);
	if (g->u.maxrank - g->u.minrank <= 1) return;
	/* this is the downward looking pass. r is a candidate rank. */
	for (r = 1; g->u.rank[r+1].n; r++) {
		for (leftpos = 0; leftpos < g->u.rank[r].n; leftpos++) {
			left = g->u.rank[r].v[leftpos];
			if (downcandidate(left) == FALSE) continue;
			for (rightpos = leftpos + 1; rightpos < g->u.rank[r].n; rightpos++) {
				right = g->u.rank[r].v[rightpos];
				if (bothdowncandidates(left,right) == FALSE) break;
			}
			if (rightpos - leftpos > 1)
				mergevirtual(g,r,leftpos,rightpos-1,DOWN);
		}
	}
	/* this is the corresponding upward pass */
	while (r > 0) {
		for (leftpos = 0; leftpos < g->u.rank[r].n; leftpos++) {
			left = g->u.rank[r].v[leftpos];
			if (upcandidate(left) == FALSE) continue;
			for (rightpos = leftpos + 1; rightpos < g->u.rank[r].n; rightpos++) {
				right = g->u.rank[r].v[rightpos];
				if (bothupcandidates(left,right) == FALSE) break;
			}
			if (rightpos - leftpos > 1)
				mergevirtual(g,r,leftpos,rightpos-1,UP);
		}
		r--;
	}
	for (c = 1; c <= g->u.n_cluster; c++)
		rebuild_vlists(g->u.clust[c]);
}

infuse(g,n)
graph_t	*g;
node_t	*n;
{
	node_t	*lead;

	lead = g->u.rankleader[n->u.rank];
	if ((lead == NULL) || (lead->u.order > n->u.order))
		g->u.rankleader[n->u.rank] = n;
}

rebuild_vlists(g)
graph_t	*g;
{
	int		c,i,r,maxi;
	node_t	*n,*lead;
	edge_t	*e,*rep;

	for (r = g->u.minrank; r <= g->u.maxrank; r++)
		g->u.rankleader[r] = NULL;

	for (n = agfstnode(g); n; n = agnxtnode(g,n)) {
		infuse(g,n);
		for (e = agfstout(g,n); e; e = agnxtout(g,e)) {
			for (rep = e; rep->u.to_virt; rep = rep->u.to_virt);
			while (rep->head->u.rank < e->head->u.rank) {
				infuse(g,rep->head);
				rep = rep->head->u.out.list[0];
			}
		}
	}

	for (r = g->u.minrank; r <= g->u.maxrank; r++) {
		lead = g->u.rankleader[r];
		if(g->root->u.rank[r].v[lead->u.order] != lead)
			abort();
		g->u.rank[r].v = g->root->u.rank[r].v + g->u.rankleader[r]->u.order;
		maxi = -1;
		for (i = 0; i < g->u.rank[r].n; i++) {
			if ((n = g->u.rank[r].v[i]) == NULL) break;
			if (n->u.node_type == NORMAL) {
				if (agcontains(g,n)) maxi = i;
				else break;
			}
			else {
				edge_t	*e;
				for (e = n->u.in.list[0]; e && e->u.to_orig; e = e->u.to_orig);
				if (e && (agcontains(g,e->tail)) && agcontains(g,e->head))
					maxi = i;
			}
		}
		if (maxi == -1)
			fprintf(stderr,"warning: degenerate concentrated rank %s,%d\n",g->name,r);
		g->u.rank[r].n = maxi + 1;
	}

	for (c = 1; c <= g->u.n_cluster; c++)
		rebuild_vlists(g->u.clust[c]);
}
