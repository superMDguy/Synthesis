/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"neato.h"

static int cleanup();	/* forward */

initialize(argc,argv)
int		argc;
char	**argv;
{
	char		*rest,c;
	int			i,nfiles;

	aginit();
	nfiles = 0;
	CmdName = argv[0];
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			rest = &(argv[i][2]);
			switch (c = argv[i][1]) {
				case 'E': global_def(rest,agedgeattr); break;
				case 'G': global_def(rest,agraphattr); break;
				case 'l':
					use_library(rest[0]?rest:(*argv[i+1]!='-'?argv[++i]:NULL));
					break;
				case 'N': global_def(rest,agnodeattr); break;
				case 'o': Output_file = file_select(rest[0]?rest:argv[++i]);break;
				case 'T': Output_lang = lang_select(rest); break;
				case 'V': fprintf(stderr,"%s\n",Version); break;
				case 'v': Verbose = TRUE; break;
				case 'x': Reduce = TRUE; break;
				default:
					fprintf(stderr,"%s: option -%c unrecognized\n",CmdName,c);
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

getfloats2pt(g,name,result)
graph_t		*g;
char		*name;
point		*result;
{
	char		*p;
	int			i;
	double		xf,yf;

	if (p = agget(g,name)) {
		i = sscanf(p,"%lf,%lf",&xf,&yf);
		if ((i > 1) && (xf > 0) && (yf > 0)) {
			result->x = POINTS(xf);
			result->y = POINTS(yf);
		}
	}
}

getfloat(g,name,result)
graph_t		*g;
char		*name;
double		*result;
{
	char		*p;
	double		f;

	if (p = agget(g,name)) {
		if (sscanf(p,"%lf",&f) >= 1) *result = f;
	}
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
			else fprintf(stderr,"%s: can't open %s\n",CmdName,Files[ctr-1]);
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
				fprintf("%s server: free samples limited to %d nodes, %d edges (you sent %d, %d)\n",SERVER_NN,SERVER_NE,n,m);
			else break;
		}
#endif

		fp = next_input_file();
	}
	return g;
}


graph_t	*
input_graph()
{
	graph_t		*g;
	node_t		*n;
	edge_t		*e;
	static graph_t	*prev;

	if (g = next_input_graph()) {

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
			/* attribs for polygon shapes */
		N_sides = agfindattr(g->proto->n,"sides");
		N_peripheries = agfindattr(g->proto->n,"peripheries");
		N_skew = agfindattr(g->proto->n,"skew");
		N_orientation = agfindattr(g->proto->n,"orientation");
		N_distortion = agfindattr(g->proto->n,"distortion");
		for (n = agfstnode(g); n; n = agnxtnode(g,n)) init_node(n);

		/* initialize edges */
		E_weight = agfindattr(g->proto->e,"w");
		E_color = agfindattr(g->proto->e,"color");
		E_fontsize = agfindattr(g->proto->e,"fontsize");
		E_fontname = agfindattr(g->proto->e,"fontname");
		E_fontcolor = agfindattr(g->proto->e,"fontcolor");
		E_label = agfindattr(g->proto->e,"label");
		E_style = agfindattr(g->proto->e,"style");
		E_decorate = agfindattr(g->proto->e,"decorate");
		for (n = agfstnode(g); n; n = agnxtnode(g,n)) {
			for (e = agfstout(g,n); e; e = agnxtout(g,e)) init_edge(e);
		}
	}

	if (g && prev) cleanup(prev);
	prev = g;
	return g;
}

init_ugraph(g)
graph_t		*g;
{
	char		*p;
	int			i;
	double		xf,yf;
	
	g->u.drawing = NEW(layout_t);
	do_graph_label(g);
	g->u.drawing->quantum = late_float(g,agfindattr(g,"quantum"),0.0,0.0);

	/* margin */
	g->u.drawing->margin.x = g->u.drawing->margin.y = DEFAULT_MARGIN;
	if (p = agget(g,"margin")) {
		i = sscanf(p,"%lf,%lf",&xf,&yf);
		if (i > 0) g->u.drawing->margin.x = g->u.drawing->margin.y = POINTS(xf);
		if (i > 1) g->u.drawing->margin.y = POINTS(yf);
	}
	Epsilon = .0001 * agnnodes(g);
	getfloats2pt(g,"size",&(g->u.drawing->size));
	getfloats2pt(g,"page",&(g->u.drawing->page));
	getfloat(g,"epsilon",&Epsilon);
	getfloat(g,"nodesep",&Nodesep);
	getfloat(g,"nodefactor",&Nodefactor);
}

init_node(n)
node_t	*n;
{
	char	*str;

	n->u.width = late_float(n,N_width,DEFAULT_NODEWIDTH,MIN_NODEWIDTH);
	n->u.height = late_float(n,N_height,DEFAULT_NODEHEIGHT,MIN_NODEWIDTH);
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
	n->u.shape->initfn(n); /* ### need to quantize ? */
	nodesize(n,n->graph->u.left_to_right);
}

init_edge(e)
edge_t	*e;
{
	char	*p;

	e->u.factor = late_float(e,E_weight,1.0,1.0);

	if (E_label && (p = agxget(e,E_label->index)) && (p[0])) {
		e->u.label = make_label(agxget(e,E_label->index),
			late_float(e,E_fontsize,DEFAULT_FONTSIZE,MIN_FONTSIZE),
			late_nnstring(e,E_fontname,DEFAULT_FONTNAME),
			late_nnstring(e,E_fontcolor,DEFAULT_COLOR));
	}

	init_port(e->tail,e,agget(e,"tailport"));
	init_port(e->head,e,agget(e,"headport"));
}

init_port(n,e,name)
node_t		*n;
edge_t		*e;
char		*name;
{
	port_t	port;

	if (name == NULL) return FALSE;
	port = n->u.shape->portfn(n,name);
#ifdef NOTDEF
	if (n->graph->u.left_to_right) port.p = invflip_pt(port.p);
#endif 
	port.order = 0;
	if (e->tail == n) e->u.tail_port = port; else e->u.head_port = port;
	return TRUE;
}

nodesize(n,flip)
node_t	*n;
boolean	flip;
{
	int		w;

	w = n->u.xsize = POINTS(n->u.width);
	n->u.lw  = n->u.rw = w / 2;
	n->u.ht = n->u.ysize = POINTS(n->u.height);
}

cleanup_node(n)
node_t	*n;
{
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
}

cleanup_graph(g)
graph_t	*g;
{
}

static cleanup(g)
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
	Lib = ALLOC(cnt+2,Lib,char*);
	Lib[cnt++] = name;
	Lib[cnt] = NULL;
}

do_graph_label(g)
graph_t	*g;
{
	char	*p;
	if (p = agget(g,"label")) {
		g->u.label = make_label(p,
			late_float(g,agfindattr(g,"fontsize"),DEFAULT_FONTSIZE,MIN_FONTSIZE),
			late_nnstring(g,agfindattr(g,"fontname"),DEFAULT_FONTNAME),
			late_nnstring(g,agfindattr(g,"fontcolor"),DEFAULT_COLOR));
	}
}
