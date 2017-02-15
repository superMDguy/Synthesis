/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#define NOT(v)		(!(v))
#ifndef FALSE
#define	FALSE		0
#endif
#ifndef TRUE
#define TRUE		NOT(FALSE)
#endif
#define NEW(t)		 (t*)zmalloc(sizeof(t))
#define N_NEW(n,t)	 (t*)zmalloc((n)*sizeof(t))
#define ALLOC(size,ptr,type) (ptr? (type*)realloc(ptr,(size)*sizeof(type)):(type*)malloc((size)*sizeof(type)))
#define ZALLOC(size,ptr,type,osize) (ptr? (type*)zrealloc(ptr,size,sizeof(type),osize):(type*)calloc((size),sizeof(type)))
#define MIN(a,b)	((a)<(b)?(a):(b))
#define MAX(a,b)	((a)>(b)?(a):(b))
#define ABS(a)		((a) >= 0 ? (a) : -(a))
#ifndef MAXINT
#define	MAXINT		((int)(~(unsigned)0 >> 1))
#endif
#ifndef MAXSHORT
#define	MAXSHORT	(0x7fff)
#endif
#define BETWEEN(a,b,c)	(((a) <= (b)) && ((b) <= (c)))
#define ROUND(f)		(round(f))
#define RADIANS(deg)	((deg)/180.0 * PI)
#define DEGREES(rad)	((rad)/PI * 180.0)
#define DIST(x1,y1,x2,y2) (sqrt(((x1) - (x2))*((x1) - (x2)) + ((y1) - (y2))*((y1) - (y2))))
#define POINTS_PER_INCH	72.0
#define POINTS(f_inch)	(ROUND(f_inch*POINTS_PER_INCH))
#define PS2INCH(ps)		((ps)/POINTS_PER_INCH)

#define is_virtual(n)	((n)->u.node_type == VIRTUAL)
#define streq(s,t)      (!strcmp((s),(t)))
