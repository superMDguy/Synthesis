/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dot.h"

initialize(argc,argv)
int		argc;
char	**argv;
{
	char		*rest,c;
	int			i,nfiles;

	aginit();
	nfiles = 0;
	for (i = 1; i < argc; i++)
		if (argv[i][0] != '-') nfiles++;
	Files = N_NEW(nfiles + 1, char *);
	nfiles = 0;
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			rest = &(argv[i][2]);
			switch (c = argv[i][1]) {
				case 'G':
					global_def(rest,agraphattr);
					break;
				case 'N':
					global_def(rest,agnodeattr);
					break;
				case 'E':
					global_def(rest,agedgeattr);
					break;
				case 'v':
					Verbose = TRUE;
					break;
				case 'T':
					Output_lang = lang_select(rest);
					break;
				case 'l':
					use_library(rest[0]?rest:(*argv[i+1]!='-'?argv[++i]:NULL));
					break;
				case 'V':
					fprintf(stderr,"%s\n",Version);
					break;
				case 'o':
					Output_file = file_select(rest[0]?rest:argv[++i]);
					break;
				default:
					fprintf(stderr,"dot: option -%c unrecognized\n",c);
			}
		}
		else
#if !SERVER
		Files[nfiles++] = argv[i];
#else
		fprintf(stderr,"dot server: input file arg %s ignored\n",argv[i]);
#endif
	}
	if (Output_file == NULL) Output_file = stdout;
	/* set persistent attributes here */ 
	agnodeattr(NULL,"label",NODENAME_ESC);
}

global_def(dcl,dclfun)
char	*dcl;
attrsym_t *((*dclfun)());
{
	char		*p,*rhs = "";
	if (p = strchr(dcl,'=')) { *p++ = '\0'; rhs = p; }
	(void) dclfun(NULL,dcl,rhs);
}

FILE *
next_input_file()
{
	static int ctr = 0;
	FILE	*rv = NULL;

	if (Files[0] == NULL) {
		if (ctr++ == 0) rv = stdin;
	}
	else {
		rv = NULL;
		while (Files[ctr]) {
			if (rv = fopen(Files[ctr++],"r")) break;
			else fprintf(stderr,"dot: can't open %s\n",Files[ctr-1]);
		}
	}
	return rv;
}

graph_t* 
next_input_graph()
{
	graph_t		*g;
	static FILE	*fp;

	if (fp == NULL) fp = next_input_file();
	g = NULL;

	while (fp != NULL) {
#if !SERVER
		if (g = agread(fp)) break;
#else
		while (g = agread(fp)) {
			int n,m;
			n = agnnodes(g);
			m = agnedges(g);
			if ((n > SERVER_NN) || (m > SERVER_NE))
				fprintf(stderr,"dot server: free samples limited to %d nodes, %d edges (you sent %d, %d)\n",SERVER_NN,SERVER_NE,n,m);
			else break;
		}
#endif

		fp = next_input_file();
	}
	return g;
}

dot_init(g)
graph_t	*g;
{
	node_t		*n;
	edge_t		*e;

		/* initialize the graph */
		init_ugraph(g);

		/* initialize nodes */
		N_height = agfindattr(g->proto->n,"height");
		N_width = agfindattr(g->proto->n,"width");
		N_shape = agfindattr(g->proto->n,"shape");
		N_color = agfindattr(g->proto->n,"color");
		N_style = agfindattr(g->proto->n,"style");
		N_fontsize = agfindattr(g->proto->n,"fontsize");
		N_fontname = agfindattr(g->proto->n,"fontname");
		N_fontcolor = agfindattr(g->proto->n,"fontcolor");
		N_label = agfindattr(g->proto->n,"label");
		N_showboxes = agfindattr(g->proto->n,"showboxes");
			/* attribs for polygon shapes */
		N_sides = agfindattr(g->proto->n,"sides");
		N_peripheries = agfindattr(g->proto->n,"peripheries");
		N_skew = agfindattr(g->proto->n,"skew");
		N_orientation = agfindattr(g->proto->n,"orientation");
		N_distortion = agfindattr(g->proto->n,"distortion");
		N_fixed = agfindattr(g->proto->n,"fixedsize");
		N_layer = agfindattr(g->proto->n,"layer");
		N_group = agfindattr(g->proto->n,"group");
		N_comment = agfindattr(g->proto->n,"comment");
		for (n = agfstnode(g); n; n = agnxtnode(g,n)) init_node(n);

		/* initialize edges */
		E_weight = agfindattr(g->proto->e,"weight");
		E_color = agfindattr(g->proto->e,"color");
		E_fontsize = agfindattr(g->proto->e,"fontsize");
		E_fontname = agfindattr(g->proto->e,"fontname");
		E_fontcolor = agfindattr(g->proto->e,"fontcolor");
		E_label = agfindattr(g->proto->e,"label");
		E_minlen = agfindattr(g->proto->e,"minlen");
		E_showboxes = agfindattr(g->proto->e,"showboxes");
		E_style = agfindattr(g->proto->e,"style");
		E_decorate = agfindattr(g->proto->e,"decorate");
		E_arrowsz = agfindattr(g->proto->e,"arrowsize");
		E_constr = agfindattr(g->proto->e,"constraint");
		E_layer = agfindattr(g->proto->e,"layer");
		E_comment = agfindattr(g->proto->e,"comment");
		for (n = agfstnode(g); n; n = agnxtnode(g,n)) {
			for (e = agfstout(g,n); e; e = agnxtout(g,e)) init_edge(e);
		}
}

init_ugraph(g)
graph_t		*g;
{
	char		*p;
	int			i;
	double		xf,yf;
	static char *rankname[] = {"local","global","none",NULL};
	static int	rankcode[] =  {LOCAL, GLOBAL, NOCLUST, LOCAL};
	
	g->u.drawing = NEW(layout_t);
	do_graph_label(g);
	g->u.drawing->quantum = late_float(g,agfindattr(g,"quantum"),0.0,0.0);
	g->u.left_to_right = ((p = agget(g,"rankdir")) && streq(p,"LR"));
	xf = late_float(g,agfindattr(g,"nodesep"),DEFAULT_NODESEP,MIN_NODESEP);
	g->u.nodesep = POINTS(xf);

	p = late_string(g,agfindattr(g,"ranksep"),NULL);
	if (p) {
			if (sscanf(p,"%lf",&xf) == 0) xf = DEFAULT_RANKSEP;
			else {if (xf < MIN_RANKSEP) xf = MIN_RANKSEP;}
			if (strstr(p,"equally")) g->u.exact_ranksep = TRUE;
	}
	else xf = DEFAULT_RANKSEP;
	g->u.ranksep = POINTS(xf);

	g->u.showboxes = late_int(g,agfindattr(g,"showboxes"),0,0);

	/* margin */
	g->u.drawing->margin.x = g->u.drawing->margin.y = DEFAULT_MARGIN;
	if (p = agget(g,"margin")) {
		i = sscanf(p,"%lf,%lf",&xf,&yf);
		if (i > 0) g->u.drawing->margin.x = g->u.drawing->margin.y = POINTS(xf);
		if (i > 1) g->u.drawing->margin.y = POINTS(yf);
	}
	if (p = agget(g,"size")) {
		i = sscanf(p,"%lf,%lf",&xf,&yf);
		if ((i > 1) && (xf > 0) && (yf > 0)) {
			g->u.drawing->size.x = POINTS(xf);
			g->u.drawing->size.y = POINTS(yf);
		}
	}
	if (p = agget(g,"page")) {
		i = sscanf(p,"%lf,%lf",&xf,&yf);
		if ((i > 1) && (xf > 0) && (yf > 0)) {
			g->u.drawing->page.x = POINTS(xf);
			g->u.drawing->page.y = POINTS(yf);
		}
	}

	g->u.drawing->centered = mapbool(agget(g,"center"));
	if (p = agget(g,"rotate")) g->u.drawing->landscape = (atoi(p) == 90);
	else {		/* today we learned the importance of backward compatibilty */
		if (p = agget(g,"orientation"))
			g->u.drawing->landscape = ((p[0] == 'l') || (p[0] == 'L'));
	}

	p = agget(g,"clusterrank");
	CL_type = maptoken(p,rankname,rankcode);
	p = agget(g,"concentrate");
	Concentrate = mapbool(p);
}

init_node(n)
node_t	*n;
{
	char	*str;

	n->u.width = late_float(n,N_width,DEFAULT_NODEWIDTH,MIN_NODEWIDTH);
	n->u.height = late_float(n,N_height,DEFAULT_NODEHEIGHT,MIN_NODEHEIGHT);
	if (N_label == NULL) str = n->name;
	else {
		str = agxget(n,N_label->index);
		if (strcmp(str,NODENAME_ESC) == 0) str = n->name;
	}
	n->u.label = make_label(str,
		late_float(n,N_fontsize,DEFAULT_FONTSIZE,MIN_FONTSIZE),
		late_nnstring(n,N_fontname,DEFAULT_FONTNAME), 
		late_nnstring(n,N_fontcolor,DEFAULT_COLOR));
	n->u.shape = bind_shape(late_nnstring(n,N_shape,DEFAULT_NODESHAPE));
	n->u.showboxes = late_int(n,N_showboxes,0,0);
	n->u.shape->initfn(n);
	nodesize(n,n->graph->u.left_to_right);
	alloc_elist(4,n->u.in);	alloc_elist(4,n->u.out);
	alloc_elist(2,n->u.flat_in); alloc_elist(2,n->u.flat_out);
	alloc_elist(2,n->u.other);
	n->u.UF_size = 1;
}

init_edge(e)
edge_t	*e;
{
	char	*p;
	char	*tailgroup,*headgroup;

	e->u.weight = late_float(e,E_weight,1.0,0.0);
	tailgroup = late_string(e->tail,N_group,"");
	headgroup = late_string(e->head,N_group,"");
	e->u.count = e->u.xpenalty = 1;
	if (tailgroup[0] && (tailgroup == headgroup))
		{ e->u.xpenalty = CL_CROSS;  e->u.weight *= 100; }

	if (E_label && (p = agxget(e,E_label->index)) && (p[0])) {
		e->u.label = make_label(agxget(e,E_label->index),
			late_float(e,E_fontsize,DEFAULT_FONTSIZE,MIN_FONTSIZE),
			late_nnstring(e,E_fontname,DEFAULT_FONTNAME),
			late_nnstring(e,E_fontcolor,DEFAULT_COLOR));
		e->tail->graph->u.has_edge_labels = TRUE;
	}

	e->u.showboxes = late_int(e,E_showboxes,0,0);
	e->u.minlen = late_int(e,E_minlen,1,0);
	p = agget(e,"tailport");
	if (p[0]) e->tail->u.has_port = TRUE;
	e->u.tail_port = e->tail->u.shape->portfn(e->tail,p);
	p = agget(e,"headport");
	if (p[0]) e->head->u.has_port = TRUE;
	e->u.head_port = e->head->u.shape->portfn(e->head,p);
}

nodesize(n,flip)
node_t	*n;
boolean	flip;
{
	float		x,y;
	int			ps;

	if (flip == FALSE) { x = n->u.width; y = n->u.height; }
	else { y = n->u.width; x = n->u.height; }
	ps = POINTS(x)/2;
	if (ps < 1) ps = 1;
	n->u.lw = n->u.rw = ps;
	n->u.ht = POINTS(y);
}

cleanup_node(n)
node_t	*n;
{
	free_list(n->u.in);
	free_list(n->u.out);
	free_list(n->u.flat_out);
	free_list(n->u.flat_in);
	free_list(n->u.other);
	memset(&(n->u),0,sizeof(Agnodeinfo_t));
}

free_splines(e)
edge_t	*e;
{
	int		i;
	if (e->u.spl) {
		for (i = 0; i < e->u.spl->size; i++) free(e->u.spl->list[i].list);
		free(e->u.spl->list);
		free(e->u.spl);
	}
	e->u.spl = NULL;
}

cleanup_edge(e)
edge_t	*e;
{
	free_splines(e);
	free(e->u.label);
	memset(&(e->u),0,sizeof(Agedgeinfo_t));

}

cleanup_graph(g)
graph_t	*g;
{
	int		i;
	free_list(g->u.comp);
	if ((g == g->root) && g->u.rank) {
		for (i = g->u.minrank; i <= g->u.maxrank; i++)
			free(g->u.rank[i].v);
		free(g->u.rank+g->u.minrank);
	}
	memset(&(g->u),0,sizeof(Agraphinfo_t));
}

dot_cleanup(g)
graph_t *g;
{
	node_t  *n;
	edge_t  *e;

    for (n = agfstnode(g); n; n = agnxtnode(g, n)) {
        for (e = agfstedge(g, n); e; e = agnxtedge(g, e, n)) {
			cleanup_edge(e);
        }
        cleanup_node(n);
	}
    cleanup_graph(g);
}

dot_close(g)
graph_t	*g;
{
	cleanup_graph(g);
	agclose(g);
}

terminate()
{
	emit_eof();
	exit(agerrors());
}

extern codegen_t	PS_CodeGen,HPGL_CodeGen,MIF_CodeGen;
extern codegen_t	GIF_CodeGen,ISMAP_CodeGen;

static struct cg_s {
		codegen_t	*cg;
		char		*name;
		int			id;
	}
	gens[] = {
		{&PS_CodeGen,"ps",POSTSCRIPT},
		{&HPGL_CodeGen,"hpgl",HPGL},
		{&HPGL_CodeGen,"pcl",PCL},
		{&MIF_CodeGen,"mif",MIF},
		{&GIF_CodeGen,"gif",GIF},
		{&ISMAP_CodeGen,"ismap",ISMAP},
		{(codegen_t*)0,"dot",ATTRIBUTED_DOT},
		{(codegen_t*)0,"canon",CANONICAL_DOT},
		{(codegen_t*)0,"plain",PLAIN},
		{(codegen_t*)0,(char*)0,0}
	};

lang_select(str)
char	*str;
{
	struct 	cg_s *p;
	for (p = gens; p->name; p++) {
		if (strcmp(str,p->name) == 0) {
			CodeGen = p->cg;
			return p->id;
		}
	}
	fprintf(stderr,"warning, language %s not recognized, use one of:\n",str);
	for (p = gens; p->name; p++) fprintf(stderr," %s",p->name);
	fprintf(stderr,"\n");
	return ATTRIBUTED_DOT;
}

FILE *
file_select(str)
char	*str;
{
	FILE 	*rv;
#if !SERVER
	rv = fopen(str,"w");
	if (rv == NULL) { perror(str); exit(1); }
#else
	rv = NULL;
#endif
	return rv;
}

use_library(name)
char	*name;
{
	static int	cnt = 0;
	if (name) {
		Lib = ALLOC(cnt+2,Lib,char*);
		Lib[cnt++] = name;
		Lib[cnt] = NULL;
	}
}
