/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dot.h"

#define	NONE	0
#define	NODE	1
#define	EDGE	2

#define PIC_COORDS_PER_LINE (16)		/* to avoid stdio BUF overflow */

static	FILE	*Outfile;
static	int		Obj;
static	box		PB;
static  int		BezierSubdivision = 10;
static	float	Scale;

static char	**U_lib,*User,*Vers;
static char EscComment[] = ".\" ";	/* troff comment */

typedef struct grcontext_t {
	char	*color,*font;
	double	size;
} grcontext_t;

#define STACKSIZE 8
static grcontext_t S[STACKSIZE];
static SP = 0;

static char		picgen_msghdr[] = "dot picgen: ";
static void unsupported(s) char *s;
{ fprintf(stderr,"%s%s unsupported\n",picgen_msghdr,s); }
static void warn(s) char *s;
{ fprintf(stderr,"%s%s warning\n",picgen_msghdr,s); }


/* troff font mapping */
typedef struct {
	char	trname[3],*psname;
} fontinfo;

static fontinfo fonttab[] = {
	{"AB",  "AvantGarde-Demi"},
	{"AI",  "AvantGarde-BookOblique"},
	{"AR",  "AvantGarde-Book"},
	{"AX",  "AvantGarde-DemiOblique"},
	{"B ",  "Times-Bold"},
	{"BI",  "Times-BoldItalic"},
	{"CB",  "Courier-Bold"},
	{"CO",  "Courier"},
	{"CX",  "Courier-BoldOblique"},
	{"H ",  "Helvetica"},
	{"HB",  "Helvetica-Bold"},
	{"HI",  "Helvetica-Oblique"},
	{"HX",  "Helvetica-BoldOblique"},
	{"Hb",  "Helvetica-Narrow-Bold"},
	{"Hi",  "Helvetica-Narrow-Oblique"},
	{"Hr",  "Helvetica-Narrow"},
	{"Hx",  "Helvetica-Narrow-BoldOblique"},
	{"I ",  "Times-Italic"},
	{"KB",  "Bookman-Demi"},
	{"KI",  "Bookman-LightItalic"},
	{"KR",  "Bookman-Light"},
	{"KX",  "Bookman-DemiItalic"},
	{"NB",  "NewCenturySchlbk-Bold"},
	{"NI",  "NewCenturySchlbk-Italic"},
	{"NR",  "NewCenturySchlbk-Roman"},
	{"NX",  "NewCenturySchlbk-BoldItalic"},
	{"PA",  "Palatino-Roman"},
	{"PB",  "Palatino-Bold"},
	{"PI",  "Palatino-Italic"},
	{"PX",  "Palatino-BoldItalic"},
	{"R ",  "Times-Roman"},
	{"S ",  "Symbol"},
	{"ZD",  "ZapfDingbats"},
	{"\000\000", (char*)0}
};

static char *picfontname(psname) char *psname;
{
	char	*rv;
	fontinfo *p;
	for (p = fonttab; p->psname; p++)
		if (streq(p->psname,psname)) break;
	if (p->psname) rv = p->trname;
	else {
		rv = "R";
		fprintf(stderr,"%s %s is not a troff font\n",picgen_msghdr,psname);
	}
	return rv;
}

static char *pic_fcoord(buf, pf) char *buf; pointf pf;
{
	sprintf(buf,"(%.3lf,%.3lf)",pf.x,pf.y);
	return buf;
}

static char *pic_coord(buf, p) char *buf; point p;
{
	return pic_fcoord(buf,cvt2ptf(p));
}

static
pic_begin_job(ofp,g,lib,user,vers,pages)
FILE		*ofp;
graph_t		*g;
char		**lib,*user,*vers;
point		pages;
{
	Outfile = ofp;
	U_lib = lib;
	Vers = vers;
	User = user;
	if (pages.x * pages.y > 1) unsupported("pagination");
}

static 
pic_end_job()
{
}

static
pic_begin_graph(g,bb,pb)
graph_t		*g;
box			bb;				/* drawable region assumed for all pages */
point		pb;				/* device page box (w.r.t. origin) */
{
	static  int onetime = TRUE;

	g = g;
	PB = bb;
	fprintf(Outfile,"%s %s drawn for %s by %s\n",EscComment,g->name,User,Vers);
	fprintf(Outfile,".PS\n");
	if (onetime) {
		/*cat_libfile(Outfile,U_lib,pic_lib);*/
		onetime = FALSE;
	}
}

static
pic_end_graph()
{
	fprintf(Outfile,".PE\n");
}

static pic_begin_page(page,scale,rot,offset)
point		page;
double		scale;
int			rot;
point		offset;
{
	if (rot) unsupported("rotation");
	Scale = scale;
	fprintf(Outfile,"scale = %.3lf\n",1.0/scale);
}

static pic_end_page()
{
	assert(SP == 0);
}

static pic_begin_node(n)
node_t		*n;
{
	Obj = NODE;
	fprintf(Outfile,"%s\t%s\n",EscComment,n->name);
}

static pic_end_node ()
{
	Obj = NONE;
}

static pic_begin_edge (e)
edge_t		*e;
{
	Obj = EDGE;
	fprintf(Outfile,"%s\t%s -> %s\n",EscComment,e->tail->name,e->head->name);
}

static pic_end_edge ()
{
	Obj = NONE;
}

static pic_begin_context()
{
	if (SP == STACKSIZE - 1) warn("stk ovfl");
	else {SP++; S[SP] = S[SP-1];}
}

static pic_end_context()
{
	if (SP == 0) warn("stk undfl");
	else SP--;
	/* have to do something else here */
}

static pic_set_font(name,size)
char		*name;
double		size;
{
	if (strcmp(S[SP].color,name) || (size != S[SP].size)) {
		S[SP].font = name;
		S[SP].size = size;
		fprintf(Outfile,".ps %d\n",(int)(size * Scale));
		fprintf(Outfile,".ft %s\n",picfontname(name));
	}
}

#ifdef NOTDEF
static pic_arrowhead(p,angle,scale)
point		p;
double		angle,scale;
{
	pointf	pf0,pf1;
	double	len,theta;

	theta = RADIANS(angle);
	len = PS2INCH(scale * ARROW_LENGTH);
	pf0 = cvt2ptf(p);
	pf1.x = pf0.x + len * cos(theta);
	pf1.y = pf0.y + len * sin(theta);
	fprintf(Outfile,"line -> from (%.3lf,%.3lf) to (%.3lf,%.3lf);\n",
		pf1.x,pf1.y,pf0.x,pf0.y);
}
#endif

static pic_set_color(name)
char	*name;
{
	S[SP].color = name;
}

static pic_set_style(s)
char	**s;
{
}

static char *
pic_string(s,auxbuf,fontname,fontsize)
char	*s,*auxbuf,*fontname;
double  fontsize;
{
	char			*p,*trfont,buf[SMALLBUF];
	int				sz;

	sz = (int)(fontsize*Scale);
	sprintf(auxbuf,"\"\\s%d",sz);
	for (p = auxbuf; *p; p++);
	while (*s)  {
		if (*s == '\\') *p++ = '\\';
		*p++ = *s++;
	}
	sprintf(p,"\\s0\"");
	return auxbuf;
}

static pic_textline(p,str,width,fontsz,align)
point		p;
char		*str;
int			width;
double		fontsz,align;
{
	char	buf[BUFSIZ];
	pointf	pf;

	pf = cvt2ptf(p);
	pf.y -= fontsz / (5.0 * POINTS_PER_INCH);
	assert(S[SP].size == fontsz);	/* this is dumb */
	/* need to deal with alignment!?! */
	pic_string(str,buf,S[SP].font,S[SP].size);
	fprintf(Outfile,"%s at (%.3lf,%.3lf);\n",buf,pf.x,pf.y);
}

static pic_bezier(A,n,arrow_at_start,arrow_at_end)
point		*A;
int			n,arrow_at_start,arrow_at_end;
{
	pointf	V[4],p;
	int		i,j,step;
	char	*arrow0,*arrow1;
	char	buf[SMALLBUF];

	arrow0 = arrow1 = "";
	if (n <= 4) {	/* degenerate case */
		if (arrow_at_start) {
			if (arrow_at_end) arrow0 = "<-> ";
			else arrow0 = "<- ";
		}
		else {
			if (arrow_at_end) arrow0 = "-> ";
			else /* arrow0 = "" */ ;
		}
	}
	else {
		if (arrow_at_start) arrow0 = "<- ";
		if (arrow_at_end) arrow1 = "-> ";
	}
			
	V[3] = cvt2ptf(A[0]);	/* initial cond */
	for (i = 0; i+3 < n; i += 3) {
		V[0] = V[3];
		for (j = 1; j <= 3; j++)
			V[j] = cvt2ptf(A[i+j]);

		if (i == 0) fprintf(Outfile,"spline %sfrom",arrow0);
		else if (i + 4 >= n) fprintf(Outfile,"spline %sto",arrow1);
		else fprintf(Outfile,"spline to");

		p = Bezier(V, 3, 0.0, NULL, NULL);
		fprintf(Outfile," %s",pic_fcoord(buf,p));
		for (step = 1; step <= BezierSubdivision; step++) {
			p = Bezier(V, 3, (float)step/BezierSubdivision, NULL, NULL);
			fprintf(Outfile," to %s",pic_fcoord(buf,p));
		}
		fprintf(Outfile,";\n");
	}
}

static pic_ellipse(p,rx,ry,filled)
point		p;
int			rx,ry;
int			filled;
{
	pointf	pf;
	pf = cvt2ptf(p);
	fprintf(Outfile,"ellipse wid %.3lf ht %.3lf at (%.3lf,%.3lf);\n",
		PS2INCH(2*rx), PS2INCH(2*ry), pf.x, pf.y);
}

static void point_list_out(A,n,close) point *A; int n; int close;
{
	int		j;
	char	buf[SMALLBUF];

	fprintf(Outfile,"line from");
	for (j = 0; j < n; j ++) {
		fprintf(Outfile," %s",pic_coord(buf,A[j]));
		if (j < n - 1) {
			if (((j + 1) % PIC_COORDS_PER_LINE) == 0)
				fprintf(Outfile,";\nline from last pt to");
			else
				fprintf(Outfile," to");
		}
	}
	if (close) fprintf(Outfile," to %s",pic_coord(buf,A[0]));
	fprintf(Outfile,";\n");
}

static pic_polygon(A,n,filled)
point		*A;
int			n,filled;
{
	if (filled) unsupported("shape fill");
	point_list_out(A,n,TRUE);
}

static pic_polyline(A,n)
point		*A;
int			n;
{
	point_list_out(A,n,FALSE);
}

static pic_user_shape(name,A,sides,filled)
char		*name;
point		*A;
int			sides,filled;
{
	unsupported("user shapes");
}

codegen_t	PIC_CodeGen = {
	pic_begin_job, pic_end_job,
	pic_begin_graph, pic_end_graph,
	pic_begin_page, pic_end_page,
	pic_begin_node, pic_end_node,
	pic_begin_edge, pic_end_edge,
	pic_begin_context, pic_end_context,
	pic_set_font, pic_textline,
	pic_set_color, pic_set_style,
	pic_ellipse, pic_polygon,
	pic_bezier, pic_polyline,
	0, pic_user_shape
};
