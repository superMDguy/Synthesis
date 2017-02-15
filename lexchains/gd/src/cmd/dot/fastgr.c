/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dot.h"

/*
 * operations on the fast internal graph.
 */

static edge_t*
ffe(u,uL,v,vL)
node_t	*u,*v;
elist	uL,vL;
{
	int		i;
	edge_t	*e;

	if (uL.size < vL.size) {
		for (i = 0; e = uL.list[i]; i++)
			if (e->head == v) break;
	}
	else {
		for (i = 0; e = vL.list[i]; i++)
			if (e->tail == u) break;
	}
	return e;
}

edge_t*
find_fast_edge(u,v)
node_t	*u,*v;
{
	return ffe(u,u->u.out,v,v->u.in);
}

node_t*
find_fast_node(g,n)
graph_t		*g;
node_t		*n;
{
	node_t		*v;
	for (v = g->u.nlist; v; v = v->u.next)
		if (v == n) break;
	return v;
}

edge_t*
find_flat_edge(u,v)
node_t	*u,*v;
{
	return ffe(u,u->u.flat_out,v,v->u.flat_in);
}

safe_list_append(e,L)
edge_t		*e;
elist		*L;
{
	int		i;
	edge_t	*f;

	for (i = 0; f = L->list[i]; i++) if (e == f) return;
	elist_append(e,(*L));
}

edge_t*
fast_edge(e)
edge_t	*e;
{
#ifdef DEBUG
	int		i;
	edge_t	*f;
	for (i = 0; f = e->tail->u.out.list[i]; i++) {
		if (e == f) {fprintf(stderr,"duplicate fast edge\n"); return;}
		assert (e->head != f->head);
	}
	for (i = 0; f = e->head->u.in.list[i]; i++) {
		if (e == f) {fprintf(stderr,"duplicate fast edge\n"); return;}
		assert (e->tail != f->tail);
	}
#endif
	elist_append(e,e->tail->u.out);
	elist_append(e,e->head->u.in);
	return e;
}

zapinlist(p, e)
elist	*p;
edge_t	*e;
{
	int		i,j;
	for (i = 0; p->list[i] && (p->list[i] != e); i++);
	assert (p->list[i] != NULL);
	for (j = i+1; p->list[j]; j++);
	p->list[i] = p->list[j - 1];
	p->list[j - 1] = NULL;
	p->size--;
}

/* disconnects e from graph */
delete_fast_edge(e)
edge_t	*e;
{
	zapinlist(&(e->tail->u.out),e);
	zapinlist(&(e->head->u.in),e);
}

safe_delete_fast_edge(e)
edge_t	*e;
{
	int		i;
	edge_t	*f;
	for (i = 0; f = e->tail->u.out.list[i]; i++)
		if (f == e) zapinlist(&(e->tail->u.out),e);
	for (i = 0; f = e->head->u.in.list[i]; i++)
		if (f == e) zapinlist(&(e->head->u.in),e);
}

other_edge(e)
edge_t	*e;
{
	elist_append(e,e->tail->u.other);
}

safe_other_edge(e)
edge_t	*e;
{
	safe_list_append(e,&(e->tail->u.other));
}

delete_other_edge(e)
edge_t	*e;
{
	zapinlist(&(e->tail->u.other),e);
}

/* orig might be an input edge, reverse of an input edge, or virtual edge */
edge_t*
new_virtual_edge(u,v,orig)
node_t		*u,*v;
edge_t		*orig;
{
	edge_t		*e;

	e = NEW(edge_t);
	e->tail = u;
	e->head = v;
	e->u.edge_type = VIRTUAL;

	if (orig) {
		e->u.count = orig->u.count;
		e->u.xpenalty = orig->u.xpenalty;
		e->u.weight = orig->u.weight;
		e->u.minlen = orig->u.minlen;
		if (e->tail == orig->tail) e->u.tail_port = orig->u.tail_port;
		else if (e->tail == orig->head) e->u.tail_port = orig->u.head_port;
		if (e->head == orig->head) e->u.head_port = orig->u.head_port;
		else if (e->head == orig->tail) e->u.head_port = orig->u.tail_port;

		if (orig->u.to_virt == NULL) orig->u.to_virt = e;
		e->u.to_orig = orig;
	}
	else e->u.minlen = e->u.count = e->u.xpenalty = e->u.weight = 1;
	return e;
}

edge_t*
virtual_edge(u,v,orig)
node_t		*u,*v;
edge_t		*orig;
{
	return fast_edge(new_virtual_edge(u,v,orig));
}

fast_node(g,n)
graph_t		*g;
node_t		*n;
{

#ifdef DEBUG
	assert (find_fast_node(g,n) == NULL);
#endif
	n->u.next = g->u.nlist;
	if (n->u.next) n->u.next->u.prev = n;
	g->u.nlist = n;
	n->u.prev = NULL;
	assert (n != n->u.next);
}

fast_nodeapp(u,v)
node_t	*u,*v;
{
	assert (u != v);
	assert (v->u.next == NULL);
	v->u.next = u->u.next;
	if (u->u.next) u->u.next->u.prev = v;
	v->u.prev = u;
	u->u.next = v;
}

delete_fast_node(g,n)
graph_t		*g;
node_t		*n;
{
	assert(find_fast_node(g,n));
	if (n->u.next) n->u.next->u.prev = n->u.prev;
	if (n->u.prev) n->u.prev->u.next = n->u.next;
	else g->u.nlist = n->u.next;
}

node_t*
virtual_node(g)
graph_t		*g;
{
	node_t		*n;

	n = NEW(node_t);
	n->name = "virtual";
	n->graph = g;
	n->u.node_type = VIRTUAL;
	n->u.lw = n->u.rw = 1;
	n->u.ht = 1;
	n->u.UF_size = 1;
	alloc_elist(4,n->u.in);
	alloc_elist(4,n->u.out);
	fast_node(g,n);
	g->u.n_nodes++;
	return n;
}

flat_edge(g,e)
graph_t		*g;
edge_t		*e;
{
	elist_append(e,e->tail->u.flat_out);
	elist_append(e,e->head->u.flat_in);
	g->root->u.has_flat_edges = g->u.has_flat_edges = TRUE;
}

delete_flat_edge(e)
edge_t	*e;
{
	zapinlist(&(e->tail->u.flat_out),e);
	zapinlist(&(e->head->u.flat_in),e);
}

#ifdef DEBUG
static char*
NAME(n)
node_t *n;
{
	static char buf[20];
	if (n->u.node_type == NORMAL) return n->name;
	sprintf(buf,"V%x",n);
	return buf;
}

fastgr(g)
graph_t	*g;
{
	int			i,j;
	node_t		*n,*w;
	edge_t		*e,*f;

	for (n = g->u.nlist; n; n = n->u.next) {
		fprintf(stderr,"%s %d: (",NAME(n), n->u.rank);
		for (i = 0; e = n->u.out.list[i]; i++) {
			fprintf(stderr," %s:%d",NAME(e->head),e->u.count);
			w = e->head;
			if (g == g->root) {
				for (j = 0; f = w->u.in.list[j]; j++) if (e == f) break;
				assert (f != NULL);
			}
		}
		fprintf(stderr," ) (");
		for (i = 0; e = n->u.in.list[i]; i++) {
			fprintf(stderr," %s:%d",NAME(e->tail),e->u.count);
			w = e->tail;
			if (g == g->root) {
				for (j = 0; f = w->u.out.list[j]; j++) if (e == f) break;
				assert (f != NULL);
			}
		}
		fprintf(stderr," )\n");
	}
}
#endif

merge_oneway(e,rep)
edge_t		*e,*rep;
{
	if (rep == e->u.to_virt) {fprintf(stderr,"warning, merge_oneway glitch\n"); return;}
	assert(e->u.to_virt == NULL);
	e->u.to_virt = rep;
	basic_merge(e,rep);
}

basic_merge(e,rep)
edge_t		*e,*rep;
{
	if (rep->u.minlen < e->u.minlen)
		rep->u.minlen = e->u.minlen;
	while (rep) {
		rep->u.count += e->u.count;
		rep->u.xpenalty += e->u.xpenalty;
		rep->u.weight += e->u.weight;
		rep = rep->u.to_virt;
	}
}

static void unrep(rep,e)
edge_t	*rep,*e;
{
	rep->u.count -= e->u.count;
	rep->u.xpenalty -= e->u.xpenalty;
	rep->u.weight -= e->u.weight;
}

unmerge_oneway(e)
edge_t		*e;
{
	edge_t	*rep,*nextrep;
	for (rep = e->u.to_virt; rep; rep = nextrep) {
		unrep(rep,e);
		nextrep = rep->u.to_virt;
		if (rep->u.count == 0) safe_delete_fast_edge(rep);	/* free(rep)? */

			/* unmerge from a virtual edge chain */
		while ((rep->u.edge_type == VIRTUAL)
		&& (rep->head->u.node_type == VIRTUAL)
		&& (rep->head->u.out.size == 1)) {
			rep = rep->head->u.out.list[0];
			unrep(rep,e);
		}
	}
	e->u.to_virt = NULL;
}

is_fast_node(g,v)
graph_t		*g;
node_t		*v;
{
	node_t		*n;

	for (n = g->u.nlist; n; n = n->u.next)
		if (v == n) return TRUE;
	return FALSE;
}
