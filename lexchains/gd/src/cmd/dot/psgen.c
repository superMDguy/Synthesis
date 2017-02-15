/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dot.h"
#include	"ps.h"

#define	NONE	0
#define	NODE	1
#define	EDGE	2

static	FILE	*Outfile;
static	int		Obj,N_pages,Cur_page;
static 	point	Pages;
static	box		PB;
static int		onetime = TRUE;

static char *Outline_and_Fill = "gsave 0 setgray stroke grestore fill\n";
static char	*Stroke = "stroke\n";
static char	*Newpath_Moveto = "newpath %d %d moveto\n";
static char	**U_lib;

typedef struct grcontext_t {
	char	*color,*font;
	double	size;
} grcontext_t;

#define STACKSIZE 8
static grcontext_t S[STACKSIZE];
static SP = 0;

static
ps_reset()
{
	onetime = TRUE;
}

static
ps_begin_job(ofp,g,lib,user,vers,pages)
FILE		*ofp;
graph_t		*g;
char		**lib,*user,*vers;
point		pages;
{
	Outfile = ofp;
	Pages = pages;
	U_lib = lib;
		/* wrong when drawing more than one than one graph - use (atend) */
	N_pages = pages.x * pages.y;
	Cur_page = 0;
	fprintf(Outfile,"%%!PS-Adobe-2.0\n");
	fprintf(Outfile,"%%%%Creator: %s\n",vers);
	fprintf(Outfile,"%%%%For: %s\n",user);
	fprintf(Outfile,"%%%%Title: %s\n",g->name);
	fprintf(Outfile,"%%%%Pages: (atend)\n");
		/* remainder is emitted by first begin_graph */
}

static 
ps_end_job()
{
	fprintf(Outfile,"%%%%Trailer\n");
	fprintf(Outfile,"%%%%Pages: %d\n",Cur_page);
	fprintf(Outfile,"end\nrestore\n");
	fprintf(Outfile,"%%%%EOF\n");
}

static ps_comment(obj, sym)
void		*obj;
attrsym_t	*sym;
{
	char	*str;
	str = late_string(obj,sym,"");
	if (str[0]) fprintf(Outfile,"%% %s\n",str);
}

static
ps_begin_graph(g,bb,pb)
graph_t		*g;
box			bb;				/* drawable region assumed for all pages */
point		pb;				/* device page box (w.r.t. origin) */
{
	PB = bb;
	if (onetime) {
		fprintf(Outfile,"%%%%BoundingBox: %d %d %d %d\n",
			bb.LL.x,bb.LL.y,bb.UR.x+1,bb.UR.y+1);
		ps_comment(g,agfindattr(g,"comment"));
		fprintf(Outfile,"%%%%EndComments\n");
		cat_libfile(Outfile,U_lib,ps_lib);
		onetime = FALSE;
	}
}

static
ps_end_graph(g)
graph_t		*g;
{
}

static ps_begin_page(page,scale,rot,offset)
point		page;
double		scale;
int			rot;
point		offset;
{
	int		page_number;
	point	sz;

	Cur_page++;
	sz = sub_points(PB.UR,PB.LL);
    fprintf(Outfile,"%%%%Page: %d (atend)\n",Cur_page);
    fprintf(Outfile,"%%%%PageBoundingBox: %d %d %d %d\n",
		PB.LL.x,PB.LL.y,PB.UR.x+1,PB.UR.y+1);
    fprintf(Outfile,"gsave\n%d %d %d %d boxprim clip newpath\n",
		PB.LL.x-1, PB.LL.y-1, sz.x + 2, sz.y + 2);
	fprintf(Outfile,"%d %d translate\n",PB.LL.x,PB.LL.y);
	if (rot) fprintf(Outfile,"gsave %d %d translate %d rotate\n",
		PB.UR.x-PB.LL.x,0,rot);
	fprintf(Outfile,"%d %d %d beginpage\n",page.x,page.y,N_pages);
	if (rot) fprintf(Outfile,"grestore\n");
	if (scale != 1.0) fprintf(Outfile,"%.4lf set_scale\n",scale);
	fprintf(Outfile,"%d %d translate %d rotate\n",offset.x,offset.y,rot);
	assert(SP == 0);
	S[SP].font = S[SP].color = ""; S[SP].size = 0.0;
}

static ps_end_page()
{
	fprintf(Outfile,"endpage\ngrestore\n");
	fprintf(Outfile,"%%%%PageTrailer\n");
	assert(SP == 0);
}

static ps_begin_node(n)
node_t		*n;
{
	Obj = NODE;
	fprintf(Outfile,"\n%%\t%s\n",n->name);
	ps_comment(n,N_comment);
}

static ps_end_node (n)
node_t		*n;
{
	Obj = NONE;
}

static ps_begin_edge (e)
edge_t		*e;
{
	Obj = EDGE;
	fprintf(Outfile,"\n%%\t%s -> %s\n",e->tail->name,e->head->name);
	ps_comment(e,E_comment);
}

static ps_end_edge (e)
{
	Obj = NONE;
}

static ps_begin_context()
{
	fprintf(Outfile,"gsave 10 dict begin\n");
	if (SP == STACKSIZE - 1) fprintf(stderr,"warning: psgen stk ovfl\n");
	else {SP++; S[SP] = S[SP-1];}
}

static ps_end_context()
{
	if (SP == 0) fprintf(stderr,"warning: psgen stk undfl\n");
	else SP--;
	fprintf(Outfile,"end grestore\n");
}

static ps_set_font(name,size)
char		*name;
double		size;
{
	if (strcmp(S[SP].font,name) || (size != S[SP].size)) {
		fprintf(Outfile,"%.2lf /%s set_font\n",size,name);
		S[SP].font = name;
		S[SP].size = size;
	}
}

static ps_arrowhead(p,theta,scale)
point		p;
double		theta,scale;
{
	fprintf(Outfile,"%d %d %.2lf %.2lf %.2lf arrowhead\n",
		p.x,p.y,theta,scale*ARROW_LENGTH,scale*ARROW_WIDTH);
}

static ps_set_color(name)
char	*name;
{
	static char *op[] = {"graph","node","edge","sethsb"};
	char	buf[SMALLBUF];

	if (strcmp(name,S[SP].color))
		fprintf(Outfile,"%s %scolor\n",colorxlate(name,buf),op[Obj]);
	S[SP].color = name;
}

static ps_set_style(s)
char	**s;
{
	char	*line,*p;

	while (p = line = *s++) {
		while (*p) p++; p++;
		while (*p) {
			fprintf(Outfile,"%s ",p);
			while (*p) p++; p++;
		}
		fprintf(Outfile,"%s\n",line);
	}
}

static char *
ps_string(s,auxbuf)
char	*s,*auxbuf;
{
	char			*p = auxbuf;
	*p++ = LPAREN;
	while (*s)  {
		if ((*s == LPAREN) || (*s == RPAREN)) *p++ = '\\';
		*p++ = *s++;
	}
	*p++ = RPAREN;
	*p = '\0';
	return auxbuf;
}

static ps_textline(p,str,width,fontsz,align)
point		p;
char		*str;
int			width;
double		fontsz,align;
{
	char	buf[BUFSIZ];

	ps_string(str,buf);
	fprintf(Outfile,"%d %d moveto %s %d %.2lf %.2lf alignedtext\n",
		p.x,p.y,buf,width,fontsz,align);
}

static ps_bezier(A,n)
point		*A;
int			n;
{
	int		j;
	fprintf(Outfile,Newpath_Moveto,A[0].x,A[0].y);
	for (j = 1; j < n; j += 3)
		fprintf(Outfile,"%d %d %d %d %d %d curveto\n",
			A[j].x,A[j].y,A[j+1].x,A[j+1].y,A[j+2].x,A[j+2].y);
	fprintf(Outfile,Stroke);
}

static ps_polygon(A,n,filled)
point		*A;
int			n,filled;
{
	int		j;
	fprintf(Outfile,Newpath_Moveto,A[0].x,A[0].y);
	for (j = 1; j < n; j++) fprintf(Outfile,"%d %d lineto\n",A[j].x,A[j].y);
	fprintf(Outfile,"closepath\n");
	fprintf(Outfile, filled? Outline_and_Fill : Stroke);
}

static ps_ellipse(p,rx,ry,filled)
point		p;
int			rx,ry;
int			filled;
{
	fprintf(Outfile,"%d %d %d %d ellipse_path\n",p.x,p.y,rx,ry);
	fprintf(Outfile, filled? Outline_and_Fill : Stroke);
}

static ps_polyline(A,n)
point		*A;
int			n;
{
	int		j;

	fprintf(Outfile,Newpath_Moveto,A[0].x,A[0].y);
	for (j = 1; j < n; j ++) fprintf(Outfile,"%d %d lineto\n",A[j].x,A[j].y);
	fprintf(Outfile,Stroke);
}

static ps_user_shape(name,A,sides,filled)
char		*name;
point		*A;
int			sides,filled;
{
	int		j;
	fprintf(Outfile,"[ ");
	for (j = 0; j < sides; j++) fprintf(Outfile,"%d %d ",A[j].x,A[j].y);
	fprintf(Outfile,"%d %d ",A[0].x,A[0].y);
	fprintf(Outfile,"]  %d %s %s\n",sides,(filled?"true":"false"),name);
}

codegen_t	PS_CodeGen = {
	ps_reset,
	ps_begin_job, ps_end_job,
	ps_begin_graph, ps_end_graph,
	ps_begin_page, ps_end_page,
	ps_begin_node, ps_end_node,
	ps_begin_edge, ps_end_edge,
	ps_begin_context, ps_end_context,
	ps_set_font, ps_textline,
	ps_set_color, ps_set_style,
	ps_ellipse, ps_polygon,
	ps_bezier, ps_polyline,
	ps_arrowhead, ps_user_shape
};
