/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/*
 * Written by Stephen North.  3/1/93 release.
 * libgraph functions for graphs.
 */

#include "libgraph.h"

Dtdisc_t agNamedisc =
	{-1, 0, agnomake, NULL_FN(void), agnodecmp, NULL_FN(unsigned long)};
Dtdisc_t agNodedisc =
	{-1, 0, agnomake, NULL_FN(void), agfastnode, NULL_FN(unsigned long)};
Dtdisc_t agIndisc =
	{-1, 0, agnomake, NULL_FN(void), agcmpin, NULL_FN(unsigned long)};
Dtdisc_t agOutdisc =
	{-1, 0, agnomake, NULL_FN(void), agcmpout, NULL_FN(unsigned long)};

#ifdef DEBUG
static int
myinedgecmp(dict, e0, e1, disc)
Dict_t		*dict;
Agedge_t	*e0,*e1;
Dtdisc_t	*disc;
{
	int rv = myinedgecmp(e0,e1);
	printf("compare (%s,%s:%s),(%s,%s:%s) = %d\n",
		e0->head?e0->head->name:"nil",
		e0->tail?e0->tail->name:"nil",
		e0->key? e0->key : "nil",
		e1->head?e1->head->name:"nil",
		e1->tail?e1->tail->name:"nil",
		e1->key? e1->key : "nil",
		rv);
	return rv;
}
#endif

static agcmpedge(e0, e1, flag)
     Agedge_t *e0;
     Agedge_t *e1;
     int flag;
{
	Agnode_t	*maj0,*maj1,*min0,*min1;
	char		*key0,*key1;

	if (flag) {
		maj0 = e0->tail; maj1 = e1->tail;
		min0 = e0->head; min1 = e1->head;
	}
	else {
		maj0 = e0->head; maj1 = e1->head;
		min0 = e0->tail; min1 = e1->tail;
	}
	key0 = e0->attr? e0->attr[KEYX] : NULL;
	key1 = e1->attr? e1->attr[KEYX] : NULL;

	if (maj0 != maj1) {
		if (maj0 && maj1) return maj0->id - maj1->id;
		else return (maj0? 1 : -1);
	}

	if (min0 != min1) {
		if (min0 && min1) return min0->id - min1->id;
		else return (min0? 1 : -1);
	}
	if (key0 == NULL) return (key1? -1 : 0);
	if (key1 == NULL) return 1;
	return strcmp(key0,key1);
}

agcmpin(dict, e0, e1, disc)
	Dict_t *dict;
     Agedge_t *e0;
     Agedge_t *e1;
	Dtdisc_t *disc;
{
	return agcmpedge(e0,e1,FALSE);
}

agcmpout(dict, e0, e1, disc)
	Dict_t *dict;
     Agedge_t *e0;
     Agedge_t *e1;
	Dtdisc_t *disc;
{
	return agcmpedge(e0,e1,TRUE);
}

static Agdata_t *agnewdata()
{
	Agdata_t		*rv;

	rv = NEW(Agdata_t);
	rv->node_dict	= dtopen(&agNamedisc,Dttree);
	rv->globattr	= agNEWdict("graph");
	rv->nodeattr	= agNEWdict("node");
	rv->edgeattr	= agNEWdict("edge");
	if (AG.proto_g) {
		agcopydict(rv->globattr,AG.proto_g->univ->globattr);
		agcopydict(rv->nodeattr,AG.proto_g->univ->nodeattr);
		agcopydict(rv->edgeattr,AG.proto_g->univ->edgeattr);
	}
	return rv;
}

static void agfreedata(g)
     Agraph_t *g;
{
	agFREEdict(g,g->univ->globattr);
	agFREEdict(g,g->univ->nodeattr);
	agFREEdict(g,g->univ->edgeattr);
	dtclose(g->univ->node_dict);
	free(g->univ);
}

static void dup_proto(g, proto)
     Agraph_t *g;
     Agproto_t *proto;
{
	Agnode_t	*n = NULL;
	Agedge_t	*e = NULL;
	Agproto_t	*s = NEW(Agproto_t);

	s->prev = g->proto;
	if (proto) {n = proto->n; e = proto->e;}
	s->n = agNEWnode(g,"\001proto",n);
	s->e = agNEWedge(g,s->n,s->n,e);
	g->proto = s;
}

void agpushproto(g)
     Agraph_t *g;
{
	dup_proto(g,g->proto);
}

void agpopproto(g)
     Agraph_t *g;
{
	Agproto_t		*s = g->proto;
	if (s != NULL) {
		g->proto = s->prev;
		s->e->tail = s->e->head = s->n;
		agFREEedge(s->e);
		agFREEnode(s->n);
		free(s);
	}
}

static Agraph_t *agNEWgraph(name, parent, kind)
     char *name;
     Agraph_t *parent;
     int kind;
{
	int				i;
	Agraph_t		*g;

	if (AG.init_called == FALSE) {
		fprintf(stderr,"libag fatal error -- aginit() was not called\n");
		exit(1);
	}
	g			= (Agraph_t*) calloc(1,AG.graph_nbytes);
	g->tag		= TAG_GRAPH;
	g->kind		= kind;
	g->nodes	= dtopen(&agNodedisc,Dttree);
	g->inedges	= dtopen(&agIndisc,Dttree);
	g->outedges	= dtopen(&agOutdisc,Dttree);

	if (parent == NULL) {
		g->univ		= agnewdata();
		g->root		= g;
		g->attr	= N_NEW(g->univ->globattr->dict->nobj,char*);
		for (i = 0; i < g->univ->globattr->dict->nobj; i++)
			g->attr[i] = agstrdup(AG.proto_g->attr[i]);
	}
	else {
		g->univ		= parent->univ;
		g->root 	= parent->root;
		g->attr 	= N_NEW(parent->univ->globattr->dict->nobj,char*);
		for (i = 0; i < parent->univ->globattr->dict->nobj; i++)
			g->attr[i] = agstrdup(parent->attr[i]);
	}

	g->meta_node 	= NULL;
	g->name			= agstrdup(name);
	g->proto		= NULL;

	if (parent) dup_proto(g,parent->proto);
	else agpushproto(g);
	return g;
}

static int reach0(m, from, to)
     Dict_t *m;
     Agnode_t *from;
     Agnode_t *to;
{
	Agedge_t		*e;

	if (from == to) return TRUE;
	if (agfindedge(from->graph->root,from,to)) return TRUE;
	dtinsert(m,from);
	for (e = agfstout(from->graph,from); e; e = agnxtout(from->graph,e))
		if ((dtsearch(m,e->head) == NULL) && reach0(m,e->head,to))
			return TRUE;
	return FALSE;
}

static int reach(from, to)
     Agnode_t *from;
     Agnode_t *to;
{
	Dict_t		*m;
	int			rv;

	m = dtopen(&agNodedisc,Dttree);
	rv = reach0(m,from,to);
	dtclose(m);
	return rv;
}

Agraph_t *agusergraph(n)
     Agnode_t *n;
{
	return (n->graph->meta_node ? NULL : (Agraph_t*)(n->attr[0]));
}

Agraph_t *agopen(name, kind)
     char *name;
     int kind;
{
	Agraph_t *g,*meta;

	g = agNEWgraph(name,NULL,kind);
	meta = agNEWgraph(name,NULL,AGMETAGRAPH);
	agnodeattr(meta, "agusergraph", NULL);
	g->meta_node = agnode(meta,name);
	g->meta_node->attr[0] = (char*) g;
	return g;
}

Agraph_t *agsubg(g, name)
     Agraph_t *g;
     char *name;
{
	Agraph_t	*subg,*meta;
	Agnode_t	*n;

	meta = g->meta_node->graph;
	n = agfindnode(meta,name);
	if (n) subg = agusergraph(n);
	else {
		subg = agNEWgraph(name,g,g->kind);
		n = agnode(meta,name);
		subg->meta_node = n;
		n->attr[0] = (char*) subg;
	}
	agINSgraph(g,subg);
	return subg;
}

Agraph_t *agfindsubg(g, name)
     Agraph_t *g;
     char *name;
{
	Agnode_t	*n;

	if (g->meta_node) {
		n = agfindnode(g->meta_node->graph,name);
		if (n) return agusergraph(n);
	}
	return NULL;
}

void agINSgraph(g, subg)
     Agraph_t *g;
     Agraph_t *subg;
{
	Agnode_t	*h,*t;
	t = g->meta_node;
	h = subg->meta_node;
	if (t && h && (reach(h,t) == FALSE))
		agedge(t->graph,t,h);
}

void agclose(g)
     Agraph_t *g;
{
	Agedge_t	*e,*f;
	Agnode_t	*n,*nn;
	Agraph_t	*meta;
	int			i,flag,is_meta;

	if ((g == NULL) || (TAG_OF(g) != TAG_GRAPH)) return;
	is_meta = AG_IS_METAGRAPH(g);
	if (is_meta == FALSE) {
		meta = g->meta_node->graph;
		/* recursively remove its subgraphs */
		do {	/* better semantics would be to find strong component */
			flag = FALSE;
			for (e = agfstout(meta,g->meta_node); e; e = f) {
				f = agnxtout(meta,e);
				if (agnxtin(meta,agfstin(meta,e->head)) == NULL) {
					agclose(agusergraph(e->head));
					flag = TRUE;
				}
			}
		} while (flag);
	}
	while (g->proto) agpopproto(g);
	if (is_meta == FALSE)
		for (i = 0; i < g->univ->globattr->dict->nobj; i++)
			agstrfree(g->attr[i]);
	free(g->attr);
	if (g == g->root) {
		for (n = agfstnode(g); n; n = nn) {
			nn = agnxtnode(g,n);
			agDELnode(g,n);
		}
		if (is_meta == FALSE) agclose(g->meta_node->graph);
		agfreedata(g);
	}
	else {
		if (is_meta == FALSE) agdelete(meta,g->meta_node);
	}
	/*
	dtclose(g->nodes);
	dtclose(g->inedges);
	dtclose(g->outedges);
	agstrfree(g->name);
	*/
	TAG_OF(g) = -1;
	free(g);
}

int agcontains(g, obj)
     Agraph_t *g;
     void *obj;
{
	switch(TAG_OF(obj)) {
		case TAG_NODE: return(agidnode(g,((Agnode_t*)obj)->id) != NULL);
		case TAG_EDGE: return(dtsearch(g->inedges,(Agedge_t*)obj) != NULL);
		case TAG_GRAPH: return(reach(g->meta_node,((Agraph_t*)obj)->meta_node));
	}
	return FALSE;
}

void aginsert(g, obj)
     Agraph_t *g;
     void *obj; 
{
	switch (TAG_OF(obj)) {
		case TAG_NODE: 	agINSnode(g,obj); break;
		case TAG_EDGE:	agINSedge(g,obj); break;
		case TAG_GRAPH:	agINSgraph(g,obj); break;
	}
}

void agdelete(g, obj)
     Agraph_t *g;
     void *obj; 
{
	switch (TAG_OF(obj)) {
		case TAG_NODE: 	agDELnode(g,obj); break;
		case TAG_EDGE:	agDELedge(g,obj); break;
		case TAG_GRAPH:	agclose(obj); break;
	}
}

int agnnodes(g)
     Agraph_t *g;
 { return g->nodes->nobj;}
int agnedges(g)
     Agraph_t *g;
 { return g->outedges->nobj;}
