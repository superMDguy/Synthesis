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

/* ISMAP font modifiers */
#define REGULAR 0
#define BOLD	1
#define ITALIC	2

/* ISMAP patterns */
#define P_SOLID	0
#define P_NONE  15
#define P_DOTTED 4		/* i wasn't sure about this */
#define P_DASHED 11		/* or this */

/* ISMAP bold line constant */
#define WIDTH_NORMAL 1
#define WIDTH_BOLD 3

static FILE    *Outfile;
static int      Obj, N_pages;
static point    Pages;
static double   Scale;
static int      Rot;
static box      PB;
static int      onetime = TRUE;

typedef struct context_t {
    char            color_ix, *fontfam, fontopt, font_was_set;
    char            pen, fill, penwidth, style_was_set;
    float           fontsz;
}               context_t;

#define MAXNEST 4
static context_t cstk[MAXNEST];
static int      SP;

static char    *FillStr = "<Fill 3>";
static char    *NoFillStr = "<Fill 15>";

static 
ismap_reset()
{
    onetime = TRUE;
}

static 
init_ismap()
{
    SP = 0;
    cstk[0].color_ix = 0;	/* ISMAP color index 0-7 */
    cstk[0].fontfam = "Times";	/* font family name */
    cstk[0].fontopt = REGULAR;	/* modifier: REGULAR, BOLD or ITALIC */
    cstk[0].pen = P_SOLID;	/* pen pattern style, default is sold */
    cstk[0].fill = P_NONE;
    cstk[0].penwidth = WIDTH_NORMAL;
}

static          pointf
ismappt(p)
    pointf          p;
{
    pointf          tmp, rv;
    tmp.x = p.x * Scale;
    tmp.y = Scale * p.y;
    if (Rot == 0) {
	rv.x = tmp.x;
	rv.y = PB.UR.y - PB.LL.y - tmp.y;
    } else {
	rv.x = PB.UR.x - PB.LL.x - tmp.y;
	rv.y = tmp.x;
    }
    return rv;
}

static 
ismap_font(cp)
    context_t      *cp;
{
}

static 
ismap_color(i)
    int             i;
{
}

static 
ismap_style(cp)
    context_t      *cp;
{
}

static
ismap_begin_job(ofp, g, lib, user, vers, pages)
    FILE           *ofp;
    graph_t        *g;
    char          **lib, *user, *vers;
    point           pages;
{
    Outfile = ofp;
    Pages = pages;
    N_pages = pages.x * pages.y;
}

static
ismap_end_job()
{
}

static
ismap_begin_graph(g, bb, pb)
    graph_t        *g;
    box             bb;		/* drawable region assumed for all pages */
    point           pb;		/* device page box (w.r.t. origin) */
{
    char	   *s;

    g = g;
    PB = bb;
    if (onetime) {
	init_ismap();
	onetime = FALSE;
    }
    if ((s = agget(g, "URL")) && strlen(s)) fprintf(Outfile,"default %s\n",s);
}

static
ismap_end_graph()
{
}

static 
ismap_begin_page(page, scale, rot, offset)
    point           page;
    double          scale;
    int             rot;
    point           offset;
{
    int             page_number;
    point           sz;

    Scale = scale;
    Rot = rot;
    page_number = page.x + page.y * Pages.x + 1;
    sz = sub_points(PB.UR, PB.LL);
}

static 
ismap_end_page()
{
}

static 
ismap_begin_node(n)
    node_t         *n;
{
    char	   *s,*s1,*s2;
    char	    buf[2048];
    pointf	    p,p1,p2;

    Obj = NODE;
    if ((s = agget(n, "URL")) && strlen(s)) {
	p.x = n->u.coord.x - n->u.lw;
	p.y = n->u.coord.y - (n->u.ht/2);
	p1 = ismappt(p);
	p.x = n->u.coord.x + n->u.rw;
	p.y = n->u.coord.y + (n->u.ht/2);
	p2 = ismappt(p);

	s = strdup(s);
        if ((s2 = strstr(s,NODENAME_ESC))) {
	    s1 = n->name;
	    *s2 = '\0';
	    s2 += 2;
        } else {
	    s1 = s2 = "";
        }

        fprintf(Outfile,"rectangle (%d,%d) (%d,%d) %s%s%s\n",
	    ROUND(p1.x),ROUND(p1.y),ROUND(p2.x),ROUND(p2.y),s,s1,s2);

	free(s);
    }
}

static 
ismap_end_node()
{
    Obj = NONE;
}

static 
ismap_begin_edge(e)
    edge_t         *e;
{
    Obj = EDGE;
}

static 
ismap_end_edge()
{
    Obj = NONE;
}

static 
ismap_begin_context()
{
    assert(SP + 1 < MAXNEST);
    cstk[SP + 1] = cstk[SP];
    SP++;
}

static 
ismap_end_context()
{
    int             c, psp = SP - 1;
    assert(SP > 0);
    if (cstk[SP].color_ix != (c = cstk[psp].color_ix))
	ismap_color(c);
    if (cstk[SP].font_was_set)
	ismap_font(&(cstk[psp]));
    if (cstk[SP].style_was_set)
	ismap_style(&(cstk[psp]));
    /* free(cstk[psp].fontfam); */
    SP = psp;
}

static 
ismap_set_font(name, size)
    char           *name;
    double          size;
{
}

static 
ismap_arrowhead(p, theta, scale)
    point           p;
    double          theta, scale;
{
}


static 
ismap_set_color(name)
    char           *name;
{
}

static 
ismap_set_style(s)
    char          **s;
{
}

static 
ismap_textline(p, str, width, fontsz, align)
    point           p;
    char           *str;
    int             width;
    double          fontsz, align;
{
}

static 
ismap_bezier(A, n)
    point          *A;
    int             n;
{
}

static 
ismap_polygon(A, n, filled)
    point          *A;
    int             n, filled;
{
    pointf          p, p0, p1, p3;
    int             i;

    p0.x = A[0].x; p0.y = A[0].y;
    p0 = ismappt(p0);
    p.x = p0.x; p.y = p0.y;
    for (i = 1; i < n; i++) {
	p1.x = p.x; p1.y = p.y;
	p.x = A[i].x; p.y = A[i].y;
	p = ismappt(p);
/*	gdImageLine(im, ROUND(p1.x), ROUND(p1.y), 
	    ROUND(p.x), ROUND(p.y), black); */
    }
/*    gdImageLine(im, ROUND(p.x), ROUND(p.y),
	ROUND(p0.x), ROUND(p0.y), black); */
}

static 
ismap_ellipse(p, rx, ry, filled)
    point           p;
    int             rx, ry;
    int             filled;
{
    pointf          mp;

    mp.x = p.x; mp.y = p.y;
    mp = ismappt(mp);
/*    gdImageArc(im, ROUND(mp.x), ROUND(mp.y),
	ROUND(Scale*(rx + rx)), ROUND(Scale*(ry + ry)), 0, 360, black); */
}

static 
ismap_polyline(A, n)
    point          *A;
    int             n;
{
    pointf          p, p1;
    int             i;

    p.x = A[0].x;
    p.y = A[0].y;
    p = ismappt(p);
    for (i = 1; i < n; i++) {
	p1.x = A[i].x;
	p1.y = A[i].y;
	p1 = ismappt(p1);
/*	gdImageLine(im, ROUND(p.x), ROUND(p.y), ROUND(p1.x), ROUND(p1.y), black); */
	p.x = p1.x;
	p.y = p1.y;
    }
}

static 
ismap_user_shape(name, A, n, filled)
    point          *A;
    int             n, filled;
{
    static boolean  onetime = TRUE;
    if (onetime) {
	fprintf(stderr, "custom shapes not available with this driver\n");
	onetime = FALSE;
    }
    ismap_polygon(A, n, filled);
}




codegen_t       ISMAP_CodeGen = {
    ismap_reset,
    ismap_begin_job, ismap_end_job,
    ismap_begin_graph, ismap_end_graph,
    ismap_begin_page, ismap_end_page,
    ismap_begin_node, ismap_end_node,
    ismap_begin_edge, ismap_end_edge,
    ismap_begin_context, ismap_end_context,
    ismap_set_font, ismap_textline,
    ismap_set_color, ismap_set_style,
    ismap_ellipse, ismap_polygon, ismap_bezier,
    ismap_polyline, ismap_arrowhead, ismap_user_shape,
};
