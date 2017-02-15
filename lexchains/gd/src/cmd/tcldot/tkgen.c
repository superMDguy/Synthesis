/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/*
 * * tkgen.c generate canvas commands to display a graph
 */

#include	"dot.h"
#include	<tcl.h>

#define	NONE	0
#define	NODE	1
#define	EDGE	2

/* font modifiers */
#define REGULAR 0
#define BOLD    1
#define ITALIC  2
 
/* patterns */
#define P_SOLID 0
#define P_NONE  15
#define P_DOTTED 4              /* i wasn't sure about this */
#define P_DASHED 11             /* or this */

/* bold line constant */
#define WIDTH_NORMAL 1
#define WIDTH_BOLD 3
 
static FILE    *Outfile;
static int      Obj, Firstperiphery, N_pages, ObjHandle;
static point    Pages;
static double   Scale;
static int      Rot;
static box      PB;
static int      onetime = TRUE;
static char     buffer[256];
static char     fontname[SMALLBUF] = "-Adobe-Times-Medium-R-Normal--*-140*";

#define ANONCHAR	'_'
#define ISEMPTYSTR(s)	(((s) == NULL) || (*(s) == '\0'))
#define NULL_FN(t)	(t(*)())0
#define void		char
#define ulong		unsigned long

typedef struct context_t {
    char            color[SMALLBUF], *fontfam, fontopt, font_was_set;
    char            pen, fill, penwidth;
    float           fontsz;
}               context_t;

#define MAXNEST 4
static context_t cstk[MAXNEST];
static int      SP;

static char    *op[] = {"graph", "node", "edge", "setrgb"};

static 
init_tk()
{
    SP = 0;
    cstk[0].color[0] = '\0';
    cstk[0].fontfam = "Times";
    cstk[0].fontopt = 0;
}

static void
tkpt(p)
    point           p;
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
    sprintf(buffer, " %.1lfp %.1lfp", rv.x, rv.y);
    Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
}

static void
tkptarray(A, n)
    point          *A;
    int             n;
{
    int             i;

    for (i = 0; i < n; i++)
	tkpt(A[i]);
}

static 
tk_font(cp)
    context_t      *cp;
{
    char           *fw, *fa;

    fw = "Medium";
    fa = "R";
    switch (cp->fontopt) {
    case BOLD:
	fw = "Bold";
	break;
    case ITALIC:
	fa = "I";
	break;
    }
    sprintf(fontname, "-Adobe-%s-%s-%s-Normal--*-%d*",
	    cp->fontfam, fw, fa, ROUND(10. * cp->fontsz));
}

static 
tk_reset()
{
    onetime = TRUE;
}

static
tk_begin_job(ofp, g, lib, user, vers, pages)
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
tk_end_job()
{
}

static
tk_begin_graph(g, bb)
    graph_t        *g;
    box             bb;		/* drawable region assumed for all pages */
{
    Agsym_t        *a;

    g = g;
    PB = bb;

    sprintf(buffer,"%dp %dp %dp %dp",0,0,PB.UR.x-PB.LL.x+1,PB.UR.y-PB.LL.y+1);
    if (!(a = agfindattr(g, "bb")))
        a = agraphattr(g, "bb", "");
    agxset(g, a->index, buffer);

    if (onetime) {
	init_tk();
	onetime = FALSE;
    }
}

static
tk_end_graph()
{
}

static 
tk_begin_page(page, scale, rot, offset)
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
tk_end_page()
{
}

static 
tk_begin_node(n)
    node_t         *n;
{
    Obj = NODE;
    ObjHandle = n->handle;
    Firstperiphery = TRUE;
}

static 
tk_end_node()
{
    Obj = NONE;
}

static 
tk_begin_edge(e)
    edge_t         *e;
{
    Obj = EDGE;
    ObjHandle = e->handle;
}

static 
tk_end_edge()
{
    Obj = NONE;
}

static 
tk_begin_context()
{
    /*assert(SP + 1 < MAXNEST);*/	/* unknown problem with SGI cc */
    cstk[SP + 1] = cstk[SP];
    SP++;
}

static 
tk_end_context()
{
    int             c, psp = SP - 1;
    /* assert(SP > 0); */			/* same as above */
    if (cstk[SP].font_was_set)
	tk_font(&(cstk[psp]));
    SP = psp;
}

static 
tk_set_font(name, size)
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
	canoncolor(q, buf);   /* makes lower case alpha */
	if (strcmp(buf, "italic") == 0)
	    cp->fontopt = ITALIC;
	else if (strcmp(q, "bold") == 0)
	    cp->fontopt = BOLD;
    }
    cp->fontfam = p;
    tk_font(&cstk[SP]);
}

static 
tk_arrowhead(p, theta)
    point           p;
    double          theta;
{
    point           A[2];
    double          v;
    v = cos(RADIANS(theta)) * 10.0;
    A[0].x = ROUND(v) + p.x;
    v = sin(RADIANS(theta)) * 10.0;
    A[0].y = ROUND(v) + p.y;
    A[1] = p;
    if (cstk[SP].pen != P_NONE) {
        Tcl_AppendResult((Tcl_Interp *) Outfile, "$c create line", (char *) NULL);
        tkptarray(A, 2);
        if (cstk[SP].color[0]) {
            sprintf(buffer, " -fill %s", cstk[SP].color);
            Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
	}
        sprintf(buffer, " -arrow last -tags 1edge%d\n", ObjHandle);
        Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
    }
}


static 
tk_set_color(name)
    char           *name;
{
	canoncolor(name, &cstk[SP].color);
}

static 
tk_set_style(s)
    char          **s;
{
    char           *line;
    context_t      *cp;
 
    cp = &(cstk[SP]);
    while (line = *s++) {
        if (streq(line, "solid")) { }     /* no-op */
/*
 *      else if (streq(line, "dashed"))
 *          cp->pen = P_DASHED;
 *      else if (streq(line, "dotted"))
 *          cp->pen = P_DOTTED;
 *      else if (streq(line, "bold"))
 *          cp->penwidth = WIDTH_BOLD;
 */
        else if (streq(line, "invis"))
            cp->pen = P_NONE;
        else if (streq(line, "filled"))
            cp->fill = P_SOLID;
        else if (streq(line, "unfilled")) { }     /* no-op */
        else
            fprintf(stderr, "tk_set_style: unsupported style %s - ignoring\n", line);
    }
}

static 
tk_textline(p, str, width, fontsz, align)
    point           p;
    char           *str;
    int             width;
    double          fontsz, align;
{
    char            buf[BUFSIZ], *astr;

    if (align == -.5)
	astr = "center";
    else {
	if (align < 0)
	    astr = "right";
	else
	    astr = "left";
    }
    Tcl_AppendResult((Tcl_Interp *) Outfile, "$c create text", (char *) NULL);
    tkpt(p);
    Tcl_AppendResult((Tcl_Interp *) Outfile, " -justify ", astr, " -text", (char *) NULL);
    Tcl_AppendElement((Tcl_Interp *) Outfile, str);
    if (cstk[SP].color[0]) {
        sprintf(buffer, " -fill %s", cstk[SP].color);
        Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
    }
    sprintf(buffer, " -font %s -tags 0%s%d\n", fontname, op[Obj], ObjHandle);
    Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
}

static 
tk_bezier(A, n)
    point          *A;
    int             n;
{
    if (cstk[SP].pen != P_NONE) {
        Tcl_AppendResult((Tcl_Interp *) Outfile, "$c create line", (char *) NULL);
        tkptarray(A, n);
        if (cstk[SP].color[0]) {
            sprintf(buffer, " -fill %s", cstk[SP].color);
            Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
        }
        sprintf(buffer, " -smooth true -tags 1edge%d\n", ObjHandle);
        Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
    }
}

static 
tk_polygon(A, n, filled)
    point          *A;
    int             n, filled;
{
    if (cstk[SP].pen != P_NONE) {
        if (Firstperiphery && filled) {
            Tcl_AppendResult((Tcl_Interp *) Outfile, "$c create polygon", (char *) NULL);
	    tkptarray(A, n);
	    if (cstk[SP].color[0]) {
	        sprintf(buffer, " -fill %s", cstk[SP].color);
                Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
	    }
        } else {
            Tcl_AppendResult((Tcl_Interp *) Outfile, "$c create line", (char *) NULL);
            tkptarray(A, n);
            tkptarray(A, 1);
            if (cstk[SP].color[0]) {
                sprintf(buffer, " -fill %s", cstk[SP].color);
                Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
	    }
        }
        sprintf(buffer, " -tags 1%s%d\n", op[Obj], ObjHandle);
        Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
    }
    Firstperiphery = FALSE;
}

static 
tk_ellipse(p, rx, ry, filled)
    point           p;
    int             rx, ry;
    int             filled;
{
    point           A[2];

    if (cstk[SP].pen != P_NONE) {
        A[0].x = p.x - rx;
        A[0].y = p.y - ry;
        A[1].x = p.x + rx;
        A[1].y = p.y + ry;
        Tcl_AppendResult((Tcl_Interp *) Outfile, "$c create oval", (char *) NULL);
        tkptarray(A, 2);
        if (Firstperiphery && filled && cstk[SP].color[0]) {
	    sprintf(buffer, " -fill %s", cstk[SP].color);
            Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
        }
        if (cstk[SP].color[0]) {
	    sprintf(buffer, " -outline %s", cstk[SP].color);
            Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
        }
        sprintf(buffer, " -tags 1node%d\n", ObjHandle);
        Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
    }
    Firstperiphery = FALSE;
}

static 
tk_polyline(A, n)
    point          *A;
    int             n;
{
    if (cstk[SP].pen != P_NONE) {
        Tcl_AppendResult((Tcl_Interp *) Outfile, "$c create line", (char *) NULL);
        tkptarray(A, n);
        if (cstk[SP].color[0]) {
            sprintf(buffer, " -fill %s", cstk[SP].color);
            Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
        }
        sprintf(buffer, " -tags 1edge%d\n", ObjHandle);
        Tcl_AppendResult((Tcl_Interp *) Outfile, buffer, (char *) NULL);
    }
}

static
tk_user_shape(name, A, n, filled)
    point          *A;
    int             n, filled;
{
    static boolean  onetime = TRUE;
    if (onetime) {
        fprintf(stderr, "custom shapes not available with this driver\n");
        onetime = FALSE;
    }
    tk_polygon(A, n, filled);
}


codegen_t       TK_CodeGen = {
    tk_reset,
    tk_begin_job, tk_end_job,
    tk_begin_graph, tk_end_graph,
    tk_begin_page, tk_end_page,
    tk_begin_node, tk_end_node,
    tk_begin_edge, tk_end_edge,
    tk_begin_context, tk_end_context,
    tk_set_font, tk_textline,
    tk_set_color, tk_set_style,
    tk_ellipse, tk_polygon,
    tk_bezier, tk_polyline,
    tk_arrowhead, tk_user_shape,
};
