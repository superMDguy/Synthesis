/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/*
 * Written by Stephen North.  3/1/93 release.
 * libgraph functions for managing attributes and their symbol tables.
 */

#define EXTERN
#include "libgraph.h"

Agdict_t *agdictof(obj)
     void *obj;
{
	Agdict_t		*d = NULL;

	switch (TAG_OF(obj)) {
		case TAG_GRAPH:	d = ((Agraph_t*)obj)->univ->globattr; break;
		case TAG_NODE:	d = ((Agnode_t*)obj)->graph->univ->nodeattr; break;
		case TAG_EDGE:	d = ((Agedge_t*)obj)->tail->graph->univ->edgeattr; break;
	}
	return d;
}

Agsym_t *agNEWsym(dict, name, value)
     Agdict_t *dict;
     char *name;
     char *value;
{
	Agsym_t		*a;
	int			i;

	a			=	NEW(Agsym_t);
	a->name		=	agstrdup(name);
	a->value	=	agstrdup(value);
	a->printed	=	TRUE;
	i = a->index	=	dict->dict->nobj;
	dict->list	=	ALLOC(i + 2, dict->list, Agsym_t*);
	dict->list[i++] = a;
	dict->list[i++] = NULL;
	dtinsert(dict->dict,a);
	return a;
}

static void obj_init_attr(obj, attr)
     void *obj;
     Agsym_t *attr;
{
	int			i;
	Agraph_t	*gobj;	/* generic object */

	gobj = (Agraph_t*) obj;
	i = attr->index;
	gobj->attr = ALLOC(i + 1, gobj->attr, char*);
	gobj->attr[i] = agstrdup(attr->value);
}

static void add_graph_attr(g, attr)
     Agraph_t *g;
     Agsym_t *attr;
{
	Agnode_t		*n;

	if (g->meta_node) {
		for (n = agfstnode(g->meta_node->graph); n;
			n = agnxtnode(g->meta_node->graph,n))
				obj_init_attr(agusergraph(n),attr);
	}
	else obj_init_attr(g,attr);
}

static void add_node_attr(g, attr)
     Agraph_t *g;
     Agsym_t *attr;
{
	Agnode_t		*n;
	Agproto_t		*proto;

	for (n = agfstnode(g); n; n = agnxtnode(g,n)) obj_init_attr(n,attr);
	if (g->meta_node) {
		for (n = agfstnode(g->meta_node->graph); n;
			 n = agnxtnode(g->meta_node->graph,n))
				for (proto = agusergraph(n)->proto; proto; proto = proto->prev)
					obj_init_attr(proto->n,attr);
	}
	else for (proto = g->proto; proto; proto = proto->prev)
		obj_init_attr(proto->n,attr);
}

static void add_edge_attr(g, attr)
     Agraph_t *g;
     Agsym_t *attr;
{
	Agnode_t		*n;
	Agedge_t		*e;
	Agproto_t		*proto;

	for (n = agfstnode(g); n; n = agnxtnode(g,n))
		for (e = agfstout(g,n); e; e = agnxtout(g,e))
			obj_init_attr(e,attr);
	if (g->meta_node) {
		for (n = agfstnode(g->meta_node->graph); n;
			n = agnxtnode(g->meta_node->graph,n))
				for (proto = agusergraph(n)->proto; proto; proto = proto->prev)
					obj_init_attr(proto->e,attr);
	}
	else for (proto = g->proto; proto; proto = proto->prev)
		obj_init_attr(proto->e,attr);
}

static Agsym_t *dcl_attr(obj, name, value)
     void *obj;
     char *name;
     char *value;
{
    Agsym_t   *rv;
	
	rv = agfindattr(obj, name);
    if (rv) {
        if (strcmp(rv->value,value)) {
			fprintf(stderr,"%s %s %s\n",name,value,rv->value);
            agerror("%s may not be redefined",name);
        }
		return rv;
    }
    rv = agNEWsym(agdictof(obj),name,value);
    if (rv) {
        switch (TAG_OF(obj)) {
            case TAG_GRAPH: add_graph_attr((Agraph_t*)obj,rv);			break;
            case TAG_NODE : add_node_attr(((Agnode_t*)obj)->graph,rv);	break;
            case TAG_EDGE : add_edge_attr(((Agedge_t*)obj)->head->graph,rv);break;
        }
    }
    return rv;
}

static void initproto()
{
	Agsym_t		*a;
	Agraph_t	*g;
	g = AG.proto_g = agopen("ProtoGraph",AGRAPH);
	a = dcl_attr(g->proto->e,KEY_ID,"");
	if (a->index != KEYX) abort();
	a = dcl_attr(g->proto->e,TAIL_ID,"");
	if (a->index != TAILX) abort();
	a->printed = FALSE;
	a = dcl_attr(g->proto->e,HEAD_ID,"");
	if (a->index != HEADX) abort();
	a->printed = FALSE;
}

Agsym_t *agraphattr(g, name, value)
     Agraph_t *g;
     char *name;
     char *value; 
{
	if (g == NULL) g = AG.proto_g;
	if (g != g->root) return NULL;
	return dcl_attr(g,name,value);
}

Agsym_t *agnodeattr (g, name, value)
     Agraph_t *g;
     char *name;
     char *value; 
{
	if (g == NULL) g = AG.proto_g;
	if (g != g->root) return NULL;
	return dcl_attr(g->proto->n,name,value);
}

Agsym_t * agedgeattr(g, name, value)
     Agraph_t *g;
     char *name;
     char *value; 
{
	if (g == NULL) g = AG.proto_g;
	if (g != g->root) return NULL;
	return dcl_attr(g->proto->e,name,value);
}

/* attribute dictionaries */

#ifdef NOTDEF
static int agcmpsym(Agsym_t *a0, Agsym_t *a1)
{
	return strcmp(a0->name,a1->name);
}
#endif

static void agfreesym(dict, ptr, disc)
	Dict_t *dict;
	void *ptr;
	Dtdisc_t *disc;
{
	Agsym_t		 *a;
	a = (Agsym_t*)ptr;
	agstrfree(a->name);
	agstrfree(a->value);
	free(a);
}

Void_t* agnomake(dict, ptr, disc)
	Dict_t *dict;
	void *ptr;
	Dtdisc_t *disc;
{
	return (void *) ptr;
}

void agFREEdict(g, attrdict)
     Agraph_t *g;
     Agdict_t *attrdict;
{
	g = g;
	dtclose(attrdict->dict);
	if (attrdict->list) free(attrdict->list);
	free(attrdict);
}

Agdict_t * agNEWdict(name)
     char *name;
{
	Agdict_t	*dict;
	static Dtdisc_t symdisc = {0, -1, agnomake, agfreesym, NULL_FN(int), NULL_FN(unsigned long)};

	dict	= NEW(Agdict_t);
	dict->name = name;
	dict->dict = dtopen(&symdisc,Dttree);
	dict->list = NULL;
	return dict;
}

void agcopydict(to_dict, from_dict)
     Agdict_t *to_dict;
     Agdict_t *from_dict;
{
	int			i;
	Agsym_t		*a,*b;

	for (i = 0; i < from_dict->dict->nobj; i++) {
		a = from_dict->list[i];
		b = agNEWsym(to_dict,a->name,a->value);
		b->printed = a->printed;
#ifdef DOS
		/* Microsoft C is a thing of wonder. */
fprintf(stderr,"", a->name, a->value);
#endif
	}
}

Agsym_t *agfindattr(obj, name)
     void *obj;
     char *name;
{
	Agsym_t		*rv, key;
	Agdict_t	*dict	= agdictof(obj);

	key.name = name;
	rv = (Agsym_t*) dtsearch(dict->dict,&key);
	return rv;
}

	/* this is normally called by the aginit() macro */
void aginitlib(gs, ns, es)
     int gs;
     int ns;
     int es;
{
	if (AG.proto_g == NULL) {
		AG.graph_nbytes = gs;
		AG.node_nbytes = ns;
		AG.edge_nbytes = es;
		AG.init_called = TRUE;
		initproto();
	}
	else
		if ((AG.graph_nbytes!=gs)||(AG.node_nbytes!=ns)||(AG.edge_nbytes!=es))
			fprintf(stderr,"aginit() called multiply with inconsistent args\n");
}

char *agget(obj, attr)
     void *obj;
     char *attr;
{
	return agxget(obj,agindex(obj,attr));
}

void agset(obj, attr, value)
     void *obj;
     char *attr;
     char *value;
{
	agxset(obj,agindex(obj,attr),value);
}

int agindex(obj, name)
     void *obj;
     char *name;
{
	Agsym_t		*a;
	int			rv = -1;
	
	a = agfindattr(obj,name);
	if (a) rv = a->index;
	return rv;
}

char *agxget(obj, index)
     void *obj;
     int index;
{
	if (index >= 0) return ((Agraph_t*)obj)->attr[index];
	return NULL;
}

void agxset(obj, index, buf)
     void *obj;
     int index;
     char *buf;
{
	char	**p;
	if (index >= 0) {
		p = ((Agraph_t*)obj)->attr;
		agstrfree(p[index]);
		p[index] = agstrdup(buf);
	}
}
