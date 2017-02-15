/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dot.h"

char *zmalloc(nbytes)
int	nbytes;
{
	char	*rv = (char*)malloc(nbytes);
	if (rv == NULL) {fprintf(stderr,"out of memory\n"); abort();}
	memset(rv,0,nbytes);
	return rv;
}

/*
 *  a queue of nodes
 */
queue *
new_queue(sz)
int		sz;
{
	queue		*q = NEW(queue);

	if (sz <= 1) sz = 2;
	q->head = q->tail = q->store = N_NEW(sz,node_t*);
	q->limit = q->store + sz;
	return q;
}

void
free_queue(q)
queue	*q;
{
	free(q->store);
	free(q);
}

void
enqueue(q,n)
queue	*q;
node_t	*n;
{
	*(q->tail++) = n;
	if (q->tail >= q->limit) q->tail = q->store;
}

node_t *
dequeue(q)
queue	*q;
{
	node_t	*n;
	if (q->head == q->tail) n = NULL;
	else {
		n = *(q->head++);
		if (q->head >= q->limit) q->head = q->store;
	}
	return n;
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

float late_float(obj,attr,def,low)
void		*obj;
attrsym_t	*attr;
float		def,low;
{
	char		*p;
	float		rv;
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

/* counts occurences of 'c' in string 'p' */
strccnt(p,c)
char	*p,c;
{
	int		rv = 0;
	while (*p) if (*p++ == c) rv++;
	return 	rv;
}

/* union-find */
node_t	*
UF_find(n)
node_t	*n;
{
	while (n->u.UF_parent && (n->u.UF_parent != n)) {
		if (n->u.UF_parent->u.UF_parent) n->u.UF_parent = n->u.UF_parent->u.UF_parent;
		n = n->u.UF_parent;
	}
	return n;
}

node_t	*
UF_union(u,v)
node_t	*u,*v;
{
	if (u == v) return u;
	if (u->u.UF_parent == NULL) {u->u.UF_parent = u; u->u.UF_size = 1;}
	else u = UF_find(u);
	if (v->u.UF_parent == NULL) {v->u.UF_parent = v; v->u.UF_size = 1;}
	else v = UF_find(v);
	if (u->id > v->id) { u->u.UF_parent = v; v->u.UF_size += u->u.UF_size;}
	else {v->u.UF_parent = u; u->u.UF_size += v->u.UF_size; v = u;}
	return v;
}

UF_remove(u,v)
node_t	*u,*v;
{
	assert(u->u.UF_size == 1);
	u->u.UF_parent = u;
	v->u.UF_size -= u->u.UF_size;
}
UF_singleton(u)
node_t		*u;
{
	u->u.UF_size = 1;
	u->u.UF_parent = NULL;
	u->u.ranktype = NORMAL;
}
UF_setname(u,v)
node_t	*u,*v;
{
	assert(u == UF_find(u));
	u->u.UF_parent = v;
	v->u.UF_size += u->u.UF_size;
}

boolean spline_merge(n)
node_t	*n;
{
	return ((n->u.node_type == VIRTUAL) && ((n->u.in.size > 1) || (n->u.out.size > 1)));
}

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

box boxof (llx, lly, urx, ury)
int llx, lly, urx, ury;
{
	box b;

	b.LL.x = llx, b.LL.y = lly;
	b.UR.x = urx, b.UR.y = ury;
	return b;
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
float	fontsize;
{
	textlabel_t	*rv = NEW(textlabel_t);
	rv->text = str;
	rv->fontname = fontname;
	rv->fontcolor = fontcolor;
	rv->fontsize = fontsize;
	label_size(rv,str,TRUE);
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
size_t	n;
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

char *strstr(s0,s1)
#ifdef __STDC__
const char	*s0,*s1;
#else
char	*s0,*s1;
#endif
{
    int n = strlen(s1);
    char *rs = (*s0) ? NULL : s1;

    while (*s0)
    {   if (strncmp(s1,s0,n) == 0L)
    {   rs = s0;
        break;
    }
    ++s0;
    }
    return(rs);
}
#endif

#ifdef DEBUG
edge_t	*
debug_getedge(g,s0,s1)
graph_t		*g;
char 		*s0,*s1;
{
	node_t	*n0,*n1;
	n0 = agfindnode(g,s0);
	n1 = agfindnode(g,s1);
	if (n0 && n1) return agfindedge(g,n0,n1);
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
int round(f)
double  f;
{
	int		rv;
	if (f > 0) rv = (int)(f + .5) ;
	else rv = (int)(f - .5);
	return rv;
}

maptoken(p,name,val)
char	*p,**name;
int		*val;
{
	int		i;
	char	*q;

	for (i = 0; q = name[i]; i++)
		if (p && streq(p,q)) break;
	return val[i];
}

mapbool(p)
char	*p;
{
	if (p == NULL) return FALSE;
	if (!strcasecmp(p,"false")) return FALSE;
	if (!strcasecmp(p,"true")) return TRUE;
	return atoi(p);
}
