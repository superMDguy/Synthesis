/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/*
 * Written by Stephen North.  3/1/93 release.
 * libgraph functions for edges.
 */

#include "libgraph.h"

static void			install_edge();

static void graphs_of_e(e, g_e, g)
     Agedge_t *e;
     Dict_t *g_e;
     Agraph_t *g;
{
	Agedge_t		*sub;

	if (dtsearch(g->inedges,e) == NULL) return;
	if (dtsearch(g_e,g->meta_node)) return;
	dtinsert(g_e,g->meta_node);
	for (sub = agfstin(g->meta_node->graph,g->meta_node); sub; 
		sub = agnxtin(g->meta_node->graph,sub))
			graphs_of_e(e,g_e,agusergraph(sub->head));
}

Agedge_t *agfindedge(g, tail, head)
     Agraph_t *g;
     Agnode_t *tail;
     Agnode_t *head;
{
	Agedge_t	key,*e;

	key.tail = tail;
	key.head = head;
	key.attr = NULL;
	e = (Agedge_t*)dtnext(g->inedges,&key);
	if (e && ((e->tail != tail) || (e->head != head))) e = NULL;
	return e;
}

static void
install_edge(g, e)
     Agraph_t *g;
     Agedge_t *e;
{
	Agraph_t		*meta;
	Agedge_t	*f;

	if (dtsearch(g->inedges,e)) return;
	agINSnode(g,e->tail);
	agINSnode(g,e->head);
	dtinsert(g->outedges,e);
	dtinsert(g->inedges,e);
	f = (Agedge_t *) dtprev(g->outedges,e);
	if (f && (f->tail == e->tail) && (f->head == e->head) && (e->printkey == NOPRINT)) e->printkey = MULTIPLE;
	if (AG_IS_METAGRAPH(g) == FALSE) {
		meta = g->meta_node->graph;
		for (f = agfstin(meta,g->meta_node); f; f = agnxtin(meta,f)) {
			install_edge(agusergraph(f->tail),e);
		}
	}
}

void agINSedge(g, e)
     Agraph_t *g;
     Agedge_t *e;
{
	if (e->printkey == MULTIPLE) e->printkey = MUSTPRINT;
	install_edge(g,e);
}


static int printedge(p)
void		*p;
{
	Agedge_t	*e = (Agedge_t*)p;
	fprintf(stderr,"\t%x %s,%s\n",e,e->tail->name,e->head->name);
}

Agedge_t * agfstedge(g, n)
     Agraph_t *g;
     Agnode_t *n;
{
	Agedge_t	*e;

	e = NULL;
	if ((g != NULL) && (n != NULL)) {
		e = agfstout(g,n);
		if (e == NULL) e = agfstin(g,n);
	}
	return e;
}

Agedge_t * agnxtedge(g, e, n)
     Agraph_t *g;
     Agedge_t *e;
     Agnode_t *n;
{
	Agedge_t	*f;

	f = NULL;
	if ((g != NULL) && (e != NULL) && (n != NULL)) {
		if (e->tail == n) {
			f = (Agedge_t*)dtnext(g->outedges,e);
			if ((f != NULL) && (f->tail == n)) return f;
			f = agfstin(g,n);
			while (f && (f->head == f->tail) && (f->head == n))
				f = (Agedge_t*)dtnext(g->inedges,f);
		}
		else {
			if (e->head != n) return NULL;
			else f = (Agedge_t*) dtnext(g->inedges,e);
		}
		while (f && (f->head == f->tail) && (f->head == n))
			f = (Agedge_t*)dtnext(g->inedges,f);
		if (f && (f->head != n)) f = NULL;
	}
	return f;
}

Agedge_t * agfstout(g, n)
     Agraph_t *g;
     Agnode_t *n;
{
	Agedge_t		*f,key;

	f = NULL;
	if ((g != NULL) && (n != NULL)) {
		key.tail = n;
		key.head = NULL;
		key.attr = NULL;
		f = (Agedge_t*) dtnext(g->outedges,&key);
		if (f && (f->tail != n)) f = NULL;
	}
	return f;
}

Agedge_t * agnxtout(g, e)
     Agraph_t *g;
     Agedge_t *e;
{
	Agedge_t		*f;
	f = (Agedge_t*) dtnext(g->outedges,e);
	if (f && (f->tail != e->tail)) f = NULL;
	return f;
}

Agedge_t *agfstin(g, n)
     Agraph_t *g;
     Agnode_t *n;
{
	Agedge_t		*f,key;

	f = NULL;
	if ((g != NULL) && (n != NULL)) {
		key.head = n;
		key.tail = NULL;
		key.attr = NULL;
		f = (Agedge_t*) dtnext(g->inedges,&key);
		if (f && (f->head != n)) f = NULL;
	}
	return f;
}

Agedge_t *
agnxtin(g, e)
     Agraph_t *g;
     Agedge_t *e;
{
	Agedge_t	*f;

	f = (Agedge_t*) dtnext(g->inedges,e);
	if (f && (f->head != e->head)) f = NULL;
	return f;
}

int agedgecnt(g)
     Agraph_t *g;
 { return g->inedges->nobj; }

Agedge_t *
agNEWedge(subg, tail, head, proto)
     Agraph_t *subg;
     Agnode_t *tail;
     Agnode_t *head;
     Agedge_t *proto;
{
	int				i;
	Agedge_t		*e;

	e =	(Agedge_t*) calloc(1,AG.edge_nbytes);
	e->tag = TAG_EDGE;
	e->tail = tail;
	e->head = head;
	e->id = subg->univ->max_edge_id++;

	e->attr = N_NEW(subg->univ->edgeattr->dict->nobj,char*);
	for (i = 0; i < subg->univ->edgeattr->dict->nobj; i++) e->attr[i] =
	  agstrdup(proto? proto->attr[i] : subg->univ->edgeattr->list[i]->value);
	return e;
}

Agedge_t *agedge(g, tail, head)
     Agraph_t *g;
     Agnode_t *tail;
     Agnode_t *head;
{
	Agedge_t		*e;
	char			*keystr,key[SMALLBUF],printkey = NOPRINT;
	static int		ctr;
	
	if ((AG_IS_DIRECTED(g) == FALSE) && (tail > head)) {
		Agnode_t* temp; temp = head; head = tail; tail = temp;
	}
	keystr = g->proto->e->attr[KEYX];	/* temporarily set aside */
	e = NULL;
	g->proto->e->head = head;
	g->proto->e->tail = tail;
	if (AG_IS_STRICT(g)) {
		g->proto->e->attr[KEYX] = "";	
		e = (Agedge_t*)dtsearch(g->root->inedges,g->proto->e);
		if (e && (e->tail == tail) && (e->head == head)) {
			install_edge(g,e);
		}
	}
	else {
		if (keystr[0]) {
			e = (Agedge_t*)dtsearch(g->root->inedges,g->proto->e);
			if (e) agINSedge(g,e);
			else printkey = MUSTPRINT;
		}
		else {
			sprintf(key,"%d",ctr++);
			g->proto->e->attr[KEYX] = key;
		}
	}
	if (e == NULL) {
		e = agNEWedge(g,tail,head,g->proto->e);
		install_edge(g,e);
		g->proto->e->head = g->proto->e->tail = g->proto->n;
		e->printkey = printkey;
	}
	g->proto->e->attr[KEYX] = keystr;
	return e;
}

void agFREEedge(e)
     Agedge_t *e;
{
	int			i;
	Agdict_t	*dict = agdictof(e);

	dict = dict;
	TAG_OF(e) = -1;
	for (i = 0; i < e->tail->graph->univ->edgeattr->dict->nobj; i++)
		agstrfree(e->attr[i]);
	free(e->attr);
	free(e);
}

void agDELedge(g, e)
     Agraph_t *g;
     Agedge_t *e;
{
	Agraph_t*	meta;
	Agraph_t*	g0;
	Agedge_t*	f;

	if (dtsearch(g->inedges,e) == NULL) {
		fprintf(stderr,"...was not found\n");
		dtwalk(g->inedges,printedge);
		return;
	}
	if (AG_IS_METAGRAPH(g) == FALSE) {
		meta = g->meta_node->graph;
		for (f = agfstout(meta,g->meta_node);
		  f; f = agnxtout(meta,f)) {
			g0 = agusergraph(f->head);
			if (dtsearch(g0->inedges,e)) agDELedge(g0,e);
		}
	}
	dtdelete(g->inedges,e);
	dtdelete(g->outedges,e);
	if (g == g->root) agFREEedge(e);
}

