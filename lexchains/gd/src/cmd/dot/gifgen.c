/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dot.h"
#include	"gd.h"
#include	"gdfontl.h"
#include	"gdfonts.h"

#define	NONE	0
#define	NODE	1
#define	EDGE	2

#define BEZIERSUBDIVISION 10

/* font modifiers */
#define REGULAR 0
#define BOLD	1
#define ITALIC	2

/* patterns */
#define P_SOLID	0
#define P_NONE  15
#define P_DOTTED 4		/* i wasn't sure about this */
#define P_DASHED 11		/* or this */

/* bold line constant */
#define WIDTH_NORMAL 1
#define WIDTH_BOLD 3

typedef struct {
  unsigned char r, g, b;
} Color;

static FILE    *Outfile;
static int      Obj, N_pages;
static point    Pages;
static double   Scale;
static int      Rot;
static box      PB;
static int      onetime = TRUE;

static gdImagePtr im;

typedef struct context_t {
    char            color_ix, *fontfam, fontopt, font_was_set;
    char            pen, fill, penwidth;
    float           fontsz;
}               context_t;

#define MAXNEST 4
static context_t cstk[MAXNEST];
static int      SP;

static 
gif_reset()
{
    onetime = TRUE;
}

static 
init_gif()
{
    int transparent;

    SP = 0;
    if ((transparent = gdImageGetTransparent(im)) == -1) {
        transparent = gdImageColorAllocate(im, 255, 255, 255);
        gdImageColorTransparent(im, transparent);
    }
    cstk[0].color_ix = transparent; /* set background transparent */
    cstk[0].fontfam = "Times";	/* font family name */
    cstk[0].fontopt = REGULAR;	/* modifier: REGULAR, BOLD or ITALIC */
    cstk[0].pen = P_SOLID;	/* pen pattern style, default is solid */
    cstk[0].fill = P_NONE;
    cstk[0].penwidth = WIDTH_NORMAL;


}

static          pointf
gifpt(p)
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
gif_font(cp)
    context_t      *cp;
{
    char           *fw, *fa;

    fw = fa = "Regular";
    switch (cp->fontopt) {
    case BOLD:
	fw = "Bold";
	break;
    case ITALIC:
	fa = "Italic";
	break;
    }
}

static
gif_begin_job(ofp, g, lib, user, vers, pages)
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
gif_end_job()
{
}

static
gif_begin_graph(g, bb, pb)
    graph_t        *g;
    box             bb;		/* drawable region assumed for all pages */
    point           pb;		/* device page box (w.r.t. origin) */
{
    g = g;
    PB = bb;
    im = gdImageCreate((PB.UR.x - PB.LL.x + 2), (PB.UR.y - PB.LL.y + 2));
    if (onetime) {
	init_gif();
	onetime = FALSE;
    }
}

static
gd_begin_graph(g, bb, pb)
    graph_t        *g;
    box             bb;		/* drawable region assumed for all pages */
    point           pb;		/* device page box (w.r.t. origin) */
{
    g = g;
    PB = bb;
    im = *(gdImagePtr *)Output_file;
    if (onetime) {
	init_gif();
	onetime = FALSE;
    }
}

static
gif_end_graph()
{
    gdImageGif(im, Outfile);
    gdImageDestroy(im);
}

static
gd_end_graph()
{
}

static 
gif_begin_page(page, scale, rot, offset)
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
gif_end_page()
{
}

static 
gif_begin_node(n)
    node_t         *n;
{
    Obj = NODE;
}

static 
gif_end_node()
{
    Obj = NONE;
}

static 
gif_begin_edge(e)
    edge_t         *e;
{
    Obj = EDGE;
}

static 
gif_end_edge()
{
    Obj = NONE;
}

static 
gif_begin_context()
{
    assert(SP + 1 < MAXNEST);
    cstk[SP + 1] = cstk[SP];
    SP++;
}

static 
gif_end_context()
{
    int             c, psp = SP - 1;
    assert(SP > 0);
    if (cstk[SP].font_was_set)
	gif_font(&(cstk[psp]));
    /* free(cstk[psp].fontfam); */
    SP = psp;
}

static 
gif_set_font(name, size)
    char           *name;
    double          size;
{
    char           *p, *q, buf[SMALLBUF];
    context_t      *cp;

    cp = &(cstk[SP]);
    cp->font_was_set = TRUE;
    cp->fontsz = Scale * size;
    p = strdup(name);
    if (q = strchr(p, '-')) {
	*q++ = 0;
	canoncolor(q, buf);
	if (streq(buf, "italic"))
	    cp->fontopt = ITALIC;
	else if (streq(q, "bold"))
	    cp->fontopt = BOLD;
    }
    cp->fontfam = p;
    gif_font(&cstk[SP]);
}

static 
gif_arrowhead(p, theta, scale)
    point           p;
    double          theta, scale;
{
    pointf          p0, p1, p2, p3;
    double          v;
    gdPoint	    points[3];

    if (cstk[SP].pen != P_NONE) {
        p0.x = p.x; p0.y = p.y;
        p0 = gifpt(p0);
	points[0].x = ROUND(p0.x); points[0].y = ROUND(p0.y);
        v = cos(RADIANS(theta+15)) * ARROW_LENGTH; p1.x = v + p.x;
        v = sin(RADIANS(theta+15)) * ARROW_LENGTH; p1.y = v + p.y;
        p1 = gifpt(p1);
	points[1].x = ROUND(p1.x); points[1].y = ROUND(p1.y);
        v = cos(RADIANS(theta-15)) * ARROW_LENGTH; p2.x = v + p.x;
        v = sin(RADIANS(theta-15)) * ARROW_LENGTH; p2.y = v + p.y;
        p2 = gifpt(p2);
	points[2].x = ROUND(p2.x); points[2].y = ROUND(p2.y);
        gdImageFilledPolygon(im, points, 3, cstk[SP].color_ix);
    }
}

static gif_set_color(name)
char    *name;
{
    double r,g,b;
    double h,s,v;
    int    R,G,B, color;
    char   result[SMALLBUF];
 
    colorxlate(name,result);
    if ((sscanf(result,"%lf %lf %lf", &h, &s, &v)) != 3) {
      fprintf(stderr, "Unknown color %s; using black\n", name);
      h = s = v = 0.0;
    }
    hsv2rgb(&r,&g,&b,h,s,v);
    R = (int)(r*255);
    G = (int)(g*255);
    B = (int)(b*255);

    if ((color=gdImageColorExact(im,R,G,B)) < 0)
      if ((color=gdImageColorAllocate(im,R,G,B)) < 0)
        color=gdImageColorClosest(im,R,G,B);
    cstk[SP].color_ix = color;
}

static 
gif_set_style(s)
    char          **s;
{
    char           *line;
    context_t      *cp;

    cp = &(cstk[SP]);
    while (line = *s++) {
	if (streq(line, "solid")) {	/* no-op */
	} else if (streq(line, "dashed"))
	    cp->pen = P_DASHED;
/*
 *	else if (streq(line, "dotted"))
 *	    cp->pen = P_DOTTED;
 *	else if (streq(line, "bold"))
 *	    cp->penwidth = WIDTH_BOLD;
 */
	else if (streq(line, "invis"))
	    cp->pen = P_NONE;
	else if (streq(line, "filled"))
	    cp->fill = P_SOLID;
	else if (streq(line, "unfilled")) {	/* no-op */
	} else {
	    fprintf(stderr, "gif_set_style: unsupported style %s - ignoring\n",
		    line);
	}
    }
}

static char    *
gif_string(s, auxbuf)
    char           *s, *auxbuf;
{
    char           *p = auxbuf, esc;
    while (*s) {
	esc = 0;
	switch (*s) {
	case '\t':
	    esc = 't';
	    break;
	case '>':
	case '\'':
	case '`':
	case '\\':
	    esc = *s;
	    break;
	}
	if (esc) {
	    *p++ = '\\';
	    *p++ = esc;
	} else
	    *p++ = *s;
	s++;
    }
    *p = '\0';
    return auxbuf;
}

static 
gif_textline(p, str, width, fontsz, align)
    point           p;
    char           *str;
    int             width;
    double          fontsz, align;
{
    double		    len;
    char            buf[BUFSIZ], *astr;
    pointf          mp;

    gif_string(str,buf);
    len = strlen(buf) * fontsz/2;
    if (align == -.5) mp.x = p.x - (len)/2;
    else {if (align < 0) mp.x = p.x; else mp.x = p.x - len;}
    mp.x = mp.x + 5; mp.y = p.y + fontsz/2 - 2;
    mp = gifpt(mp);
    gdImageString(im, gdFontSmall, ROUND(mp.x), ROUND(mp.y),
	gif_string(str,buf), cstk[SP].color_ix);
}

static 
gif_bezier(A, n)
    point          *A;
    int             n;
{
    pointf          p0, p1, V[4];
    int             i, j, step;

    if (cstk[SP].pen != P_NONE) {
		V[3].x = A[0].x; V[3].y = A[0].y;
		for (i = 0; i+3 < n; i += 3) {
			V[0] = V[3];
			for (j = 1; j <= 3; j++) {
				V[j].x  = A[i+j].x; V[j].y = A[i+j].y;
			}
			p0 = gifpt(V[0]); 
			for (step = 1; step <= BEZIERSUBDIVISION; step++) {
				p1 = gifpt(Bezier(V, 3, (float)step/BEZIERSUBDIVISION, NULL, NULL));
				if (cstk[SP].pen == P_DASHED) {
					gdImageDashedLine(im, ROUND(p0.x), ROUND(p0.y),
					ROUND(p1.x), ROUND(p1.y), cstk[SP].color_ix);
				}
				else {
					gdImageLine(im, ROUND(p0.x), ROUND(p0.y),
					ROUND(p1.x), ROUND(p1.y), cstk[SP].color_ix);
				}
				p0 = p1;
			}
		}
    }
}

static 
gif_polygon(A, n, filled)
    point          *A;
    int             n, filled;
{
    pointf          p, p0, p1, p3;
    int             i;
    gdPoint	points[500]; /* Naughty.  Should allocate n */

    for (i = 0; i < n; i++) {
	p.x = A[i].x; p.y = A[i].y;
	p = gifpt(p);
	points[i].x = ROUND(p.x); points[i].y = ROUND(p.y);
    }
    if (filled) {
        gdImageFilledPolygon(im, points, n, cstk[SP].color_ix);
    } else {
        gdImagePolygon(im, points, n, cstk[SP].color_ix);
    }
}

static 
gif_ellipse(p, rx, ry, filled)
    point           p;
    int             rx, ry;
    int             filled;
{
    pointf          mp;

    if (cstk[SP].pen != P_NONE) {
        mp.x = p.x; mp.y = p.y;
        mp = gifpt(mp);
        if (filled) {
            gdImageFilledArc(im, ROUND(mp.x), ROUND(mp.y),
	        ROUND(Scale*(rx + rx)), ROUND(Scale*(ry + ry)),
		0, 360, cstk[SP].color_ix);
        } else {
            gdImageArc(im, ROUND(mp.x), ROUND(mp.y),
	        ROUND(Scale*(rx + rx)), ROUND(Scale*(ry + ry)),
		0, 360, cstk[SP].color_ix);
	}
    }
}

static 
gif_polyline(A, n)
    point          *A;
    int             n;
{
    pointf          p, p1;
    int             i;

    if (cstk[SP].pen != P_NONE) {
        p.x = A[0].x;
        p.y = A[0].y;
        p = gifpt(p);
        for (i = 1; i < n; i++) {
	    p1.x = A[i].x;
	    p1.y = A[i].y;
	    p1 = gifpt(p1);
	    if (cstk[SP].pen == P_DASHED) {
	        gdImageDashedLine(im, ROUND(p.x), ROUND(p.y),
		    ROUND(p1.x), ROUND(p1.y), cstk[SP].color_ix);
            } else {
		gdImageLine(im, ROUND(p.x), ROUND(p.y),
		    ROUND(p1.x), ROUND(p1.y), cstk[SP].color_ix);
	    }
	    p.x = p1.x;
	    p.y = p1.y;
	}
    }
}

static 
gif_user_shape(name, A, n, filled)
    point          *A;
    int             n, filled;
{
    static boolean  onetime = TRUE;
    if (onetime) {
	fprintf(stderr, "custom shapes not available with this driver\n");
	onetime = FALSE;
    }
    gif_polygon(A, n, filled);
}




codegen_t       GIF_CodeGen = {
    gif_reset,
    gif_begin_job, gif_end_job,
    gif_begin_graph, gif_end_graph,
    gif_begin_page, gif_end_page,
    gif_begin_node, gif_end_node,
    gif_begin_edge, gif_end_edge,
    gif_begin_context, gif_end_context,
    gif_set_font, gif_textline,
    gif_set_color, gif_set_style,
    gif_ellipse, gif_polygon, gif_bezier,
    gif_polyline, gif_arrowhead, gif_user_shape,
};

codegen_t       GD_CodeGen = {
    gif_reset,
    gif_begin_job, gif_end_job,
    gd_begin_graph, gd_end_graph,  /* use open GD structure */
    gif_begin_page, gif_end_page,
    gif_begin_node, gif_end_node,
    gif_begin_edge, gif_end_edge,
    gif_begin_context, gif_end_context,
    gif_set_font, gif_textline,
    gif_set_color, gif_set_style,
    gif_ellipse, gif_polygon, gif_bezier,
    gif_polyline, gif_arrowhead, gif_user_shape,
};
