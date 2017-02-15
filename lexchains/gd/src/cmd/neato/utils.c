/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"neato.h"

point pointof(x, y)
int x, y;
{
	point rv;
	rv.x = x, rv.y = y;
	return rv;
}

point
cvt2pt(p)
pointf	p;
{
	point	rv;
	rv.x = POINTS(p.x);
	rv.y = POINTS(p.y);
	return rv;
}

pointf
cvt2ptf(p)
point	p;
{
	pointf	rv;
	rv.x = PS2INCH(p.x);
	rv.y = PS2INCH(p.y);
	return rv;
}


point add_points(p0,p1)
point	p0,p1;
{
	p0.x += p1.x;
	p0.y += p1.y;
	return p0;
}

point sub_points(p0,p1)
point	p0,p1;
{
	p0.x -= p1.x;
	p0.y -= p1.y;
	return p0;
}

box mkbox(p0,p1)
point	p0,p1;
{
	box		rv;

	if (p0.x < p1.x)	{	rv.LL.x = p0.x; rv.UR.x = p1.x; }
	else				{	rv.LL.x = p1.x; rv.UR.x = p0.x; }
	if (p0.y < p1.y)	{	rv.LL.y = p0.y; rv.UR.y = p1.y; }
	else				{	rv.LL.y = p1.y; rv.UR.y = p0.y; }
	return rv;
}

double atan2pt(p1,p0)
point	p1,p0;
{
	return atan2((double)(p1.y - p0.y),(double)(p1.x - p0.x));
}

/* from Glassner's Graphics Gems */
#define W_DEGREE 5

/*
 *  Bezier : 
 *	Evaluate a Bezier curve at a particular parameter value
 *      Fill in control points for resulting sub-curves if "Left" and
 *	"Right" are non-null.
 * 
 */
pointf Bezier (V, degree, t, Left, Right)
	int degree;		/* Degree of bezier curve	*/
	pointf *V;		/* Control pts			*/
	double t;		/* Parameter value		*/
	pointf *Left;		/* RETURN left half ctl pts	*/
	pointf *Right;		/* RETURN right half ctl pts	*/
{
	int i, j;		/* Index variables	*/
	pointf Vtemp[W_DEGREE + 1][W_DEGREE + 1];

	/* Copy control points	*/
	for (j =0; j <= degree; j++) {
		Vtemp[0][j] = V[j];
	}

	/* Triangle computation	*/
	for (i = 1; i <= degree; i++) {	
		for (j =0 ; j <= degree - i; j++) {
	    	Vtemp[i][j].x =
	      		(1.0 - t) * Vtemp[i-1][j].x + t * Vtemp[i-1][j+1].x;
	    	Vtemp[i][j].y =
	      		(1.0 - t) * Vtemp[i-1][j].y + t * Vtemp[i-1][j+1].y;
		}
	}
	
	if (Left != NULL)
		for (j = 0; j <= degree; j++)
	    		Left[j] = Vtemp[j][0];
	if (Right != NULL)
		for (j = 0; j <= degree; j++)
	    		Right[j] = Vtemp[degree-j][j];

	return (Vtemp[degree][0]);
}

textlabel_t	*
make_label(str,fontsize,fontname,fontcolor)
char	*str,*fontname,*fontcolor;
double	fontsize;
{
	pointf	dummy;
	textlabel_t	*rv = NEW(textlabel_t);
	rv->text = str;
	rv->fontname = fontname;
	rv->fontcolor = fontcolor;
	rv->fontsize = fontsize;
	dummy = label_size(rv,str,TRUE);
	return rv;
}

char *
zrealloc(ptr,size,elt,osize)
void	*ptr;
int		size,elt,osize;
{
	char	*p = (char*)realloc(ptr,size*elt);
	if (osize < size) memset(p+(osize*elt),'\0',(size-osize)*elt);
	return p;
}

/* portability hacks */
#ifndef HAS_STRDUP
char *strdup(s) char *s; { return strcpy(malloc(strlen(s)+1),s); }
#endif

#ifndef HAS_HYPOT
double hypot(x,y) double x,y; {return sqrt(x*x + y*y); }
#endif

#ifndef HAS_STRCASECMP
strcasecmp(s0,s1)
#ifdef __STDC__
const char	*s0,*s1;
#else
char	*s0,*s1;
#endif
{
	char		c0,c1;
	do {
		c0 = *s0++;
		c1 = *s1++;
		if (isupper(c0)) c0 = tolower(c0);
		if (isupper(c1)) c1 = tolower(c1);
		if (c0 != c1) break;
	} while (c0 && c1);
	return c0 - c1;
}
strncasecmp(s0,s1,n)
#ifdef __STDC__
const char	*s0,*s1;
const size_t	n;
#else
char	*s0,*s1;
int		n;
#endif
{
	char		c0,c1;
	int			m = n;
	while (m--) {
		c0 = *s0++;
		c1 = *s1++;
		if (isupper(c0)) c0 = tolower(c0);
		if (isupper(c1)) c1 = tolower(c1);
		if (c0 != c1) break;
	}
	return c0 - c1;
}
#endif


#ifdef DEBUG
edge_t	*
debug_getedge(g,s0,s1)
graph_t		*g;
{
	node_t	*n0,*n1;
	n0 = getnode(g,s0);
	n1 = getnode(g,s1);
	if (n0 && n1) return getedge(g,n0,n1);
	else return NULL;
}
#endif

#ifndef DOS
#include	<pwd.h>
#endif

char *
username(buf)
char	*buf;
{
	char	*user = NULL;
#ifndef DOS
	struct passwd	*p;
	p = (struct passwd *) getpwuid(getuid());
	if (p) {sprintf(buf,"(%s) %s",p->pw_name,p->pw_gecos); user = buf;}
#endif
	if (user == NULL) user = "Bill Gates";
	return user;
}

cat_libfile(ofp,arglib,stdlib)
FILE		*ofp;
char		**arglib,**stdlib;
{
	FILE	*fp;
	char	*p,**s,buf[BUFSIZ];
	int		i,use_stdlib = TRUE;

	if (arglib) for (i = 0; p = arglib[i]; i++)
		if (*p == '\0') use_stdlib = FALSE;
	if (use_stdlib) for (s = stdlib; *s; s++) {fputs(*s,ofp); fputc('\n',ofp);}
#if !SERVER
	if (arglib) for (i = 0; p = arglib[i]; i++) {
		if (p[0] && (fp = fopen(p,"r"))) {
			while (fgets(buf,sizeof(buf),fp)) fputs(buf,ofp);
		}
		else fprintf(stderr,"warning: can't open library file %s\n", p);
	}
#endif
}

rect_overlap(b0,b1)
box		b0,b1;
{
	if ((b0.UR.x < b1.LL.x) || (b1.UR.x < b0.LL.x) 
		|| (b0.UR.y < b1.LL.y) || (b1.UR.y < b0.LL.y)) return FALSE;
	return TRUE;
}

point spline_at_y(spl,y)
	splines *spl;
	int y;
{
	int i,j;
	double low, high, d, t;
	pointf c[4], pt2;
	point pt;
	static bezier bz;
	static splines *mem = NULL;

	if (mem != spl) {
		mem = spl;
		for (i = 0; i < spl->size; i++) {
			bz = spl->list[i];
			if (BETWEEN (bz.list[bz.size-1].y, y, bz.list[0].y))
				break;
		}
	}
	if (y > bz.list[0].y)
		pt = bz.list[0];
	else if (y < bz.list[bz.size-1].y)
		pt = bz.list[bz.size - 1];
	else {
		for (i = 0; i < bz.size; i += 3) {
			for (j = 0; j < 3; j++) {
				if ((bz.list[i+j].y <= y) && (y <= bz.list[i+j+1].y))
					break;
				if ((bz.list[i+j].y >= y) && (y >= bz.list[i+j+1].y))
					break;
			}
			if (j < 3)
				break;
		}
		assert (i < bz.size);
		for (j = 0; j < 4; j++) {
			c[j].x = bz.list[i + j].x;
			c[j].y = bz.list[i + j].y;
			/* make the spline be monotonic in Y, awful but it works for now */
			if ((j > 0) && (c[j].y > c[j - 1].y))
				c[j].y = c[j - 1].y;
		}
		low = 0.0; high = 1.0;
		do {
			t = (low + high) / 2.0;
			pt2 = Bezier (c, 3, t, NULL, NULL);
			d = pt2.y - y;
			if (ABS(d) <= 1)
				break;
			if (d < 0)
				high = t;
			else
				low = t;
		} while (1);
		pt.x = pt2.x;
		pt.y = pt2.y;
	}
	pt.y = y;
	return pt;
}

/* returns index of an attribute if bound, else -1 */
late_attr(obj,name)
void		*obj;
char		*name;
{
	attrsym_t	*a;
	if (a = agfindattr(obj,name)) return a->index;
	else return -1;
}

int late_int(obj,attr,def,low)
void		*obj;
attrsym_t	*attr;
int			def,low;
{
	char	*p;
	int		rv;
	if (attr == NULL) return def;
	p = agxget(obj,attr->index);
	if (p[0]  == '\0') return def;
	if ((rv = atoi(p)) < low) rv = low;
	return rv;
}

double late_float(obj,attr,def,low)
void		*obj;
attrsym_t	*attr;
double		def,low;
{
	char		*p;
	double		rv;
	if (attr == NULL) return def;
	p = agxget(obj,attr->index);
	if (p[0]  == '\0') return def;
	if ((rv = atof(p)) < low) rv = low;
	return rv;
}

char * late_string(obj,attr,def)
void		*obj;
attrsym_t	*attr;
char		*def;
{
	if (attr == NULL) return def;
	return agxget(obj,attr->index);
}

char * late_nnstring(obj,attr,def)
void		*obj;
attrsym_t	*attr;
char		*def;
{
	char	*rv = late_string(obj,attr,def);
	if (rv[0] == '\0') rv = def;
	return rv;
}

point
coord(n)
node_t	*n;
{
	pointf		pf;
	pf.x = n->u.pos[0];
	pf.y = n->u.pos[1];
	return cvt2pt(pf);
}

/* counts occurences of 'c' in string 'p' */
strccnt(p,c)
char	*p,c;
{
	int		rv = 0;
	while (*p) if (*p++ == c) rv++;
	return 	rv;
}

int round(f)
double  f;
{
	int		rv;
	if (f > 0) rv = (int)(f + .5) ;
	else rv = (int)(f - .5);
	return rv;
}

mapbool(p)
char	*p;
{
	if (p == NULL) return FALSE;
	if (!strcasecmp(p,"false")) return FALSE;
	if (!strcasecmp(p,"true")) return TRUE;
	return atoi(p);
}
