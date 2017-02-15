/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/*
 * Written by Stephen North.  3/1/93 release.
 * libgraph functions for nodes.
 */

#include "libgraph.h"

int agnodecmp(dict, n0, n1, disc)
	Dict_t *dict;
     Agnode_t *n0;
     Agnode_t *n1;
	Dtdisc_t *disc;
{
	return strcmp(n0->name,n1->name);
}

int agfastnode(dict, n0, n1, disc)
	Dict_t *dict;
     Agnode_t *n0;
     Agnode_t *n1;
	Dtdisc_t *disc;
{
	return (n0->id - n1->id);
}

Agnode_t *agfindnode(g, name)
     Agraph_t *g;
     char *name;
{
	Agnode_t		key,*rv;

	key.name = name;
	rv =  (Agnode_t*)dtsearch(g->univ->node_dict,&key);
	if (rv && (g != g->root)) rv = (Agnode_t*)dtsearch(g->nodes,rv);
	return rv;
}

Agnode_t *agidnode(g, index)
     Agraph_t *g;
     int index;
{
	Agnode_t	key,*rv;
	key.id = index;
	rv =  (Agnode_t*)dtsearch(g->nodes,&key);
	return rv;
}

Agnode_t *agnode(g, name)
     Agraph_t *g;
     char *name;
{
	Agnode_t		*n;
	if ((n = agfindnode(g->root,name)) == NULL) {
		n = agNEWnode(g,name,g->proto->n);
		dtinsert(g->univ->node_dict,n);
	}
	agINSnode(g,n);
	return n;
}

void agINSnode(g, n)
     Agraph_t *g;
     Agnode_t *n; 
{
	Agraph_t		*meta;
	Agedge_t		*e;

	if (agidnode(g,n->id)) return;
	dtinsert(g->nodes,n);
	if (AG_IS_METAGRAPH(g) == FALSE) {
		meta = g->meta_node->graph;
		for (e = agfstin(meta,g->meta_node); e; e = agnxtin(meta,e))
			agINSnode(agusergraph(e->tail),n);
	}
}

void
agDELnode(g, n)
     Agraph_t *g;
     Agnode_t *n;
{
	Agedge_t		*e,*f;
	Agraph_t		*meta,*h;

	for (e = agfstedge(g,n); e; e = f) {
		f = agnxtedge(g,e,n);
		agDELedge(g,e);
	}

	if (AG_IS_METAGRAPH(g) == FALSE) {
		meta = g->meta_node->graph;
		for (e = agfstout(meta,g->meta_node); e; e = agnxtout(meta,e)) {
			h = agusergraph(e->head);
			if (dtsearch(h->nodes,n)) agDELnode(h,n);
		}
	}
	dtdelete(g->nodes,n);
	if (g == g->root) agFREEnode(n);
}

Agnode_t *agfstnode(g)
     Agraph_t *g;
{
	return (Agnode_t*)dtfirst(g->nodes);
}

Agnode_t * agnxtnode(g, n)
     Agraph_t *g;
     Agnode_t *n;
{
	return (Agnode_t*)dtnext(g->nodes,n);
}

int agnodecnt(g)
     Agraph_t *g;
{
	return g->nodes->nobj;
}

Agnode_t *agNEWnode(subg, name, proto)
     Agraph_t *subg;
     char *name;
     Agnode_t *proto;
{
	int			i;
	Agnode_t		*n;

	n			=	(Agnode_t*) calloc(1,AG.node_nbytes);
	n->tag		=	TAG_NODE;
	n->name		=	agstrdup(name);
	n->id		=	subg->univ->max_node_id++;
	n->graph	=	subg->root;
	n->attr 	=	N_NEW(subg->univ->nodeattr->dict->nobj,char*);
	for (i = 0; i < subg->univ->nodeattr->dict->nobj; i++)
	  n->attr[i] = agstrdup(proto? proto->attr[i] :
		  subg->univ->nodeattr->list[i]->value);
	return n;
}

void agFREEnode(n)
     Agnode_t *n;
{
	int			i;
	Agdict_t	*dict = agdictof(n);

	dict = dict;
	dtdelete(n->graph->univ->node_dict,n);
	TAG_OF(n) = -1;
	agstrfree(n->name);
	if (AG_IS_METAGRAPH(n->graph) == FALSE)
		for (i = 0; i < n->graph->univ->nodeattr->dict->nobj; i++)
			agstrfree(n->attr[i]);
	free(n->attr);
	free(n);
}
