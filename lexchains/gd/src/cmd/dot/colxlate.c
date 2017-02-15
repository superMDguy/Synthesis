/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include "dot.h"

#ifndef NOCOLORNAMES
#include "colortbl.h"

char *
canoncolor(orig,out)
char	*orig,*out;
{
	char	c,*p = out;
	while (c = *orig++) {
		if (isalnum(c) == FALSE) continue;
		if (isupper(c)) c = tolower(c);
		*out++ = c;
	}
	*out = c;
	return p;
}

static int
colorcmpf(p0,p1)
#ifdef __STDC__
const void *p0,*p1;
#else
void *p0,*p1;
#endif
{
	int		i = (((hsbcolor_t*)p0)->name[0] - ((hsbcolor_t*)p1)->name[0]);
	return (i ? i : strcmp(((hsbcolor_t*)p0)->name,((hsbcolor_t*)p1)->name));
}

char *
colorxlate(str,buf)
char	*str,*buf;
{
	static	hsbcolor_t	*last;
	char				*p,canon[SMALLBUF];
	hsbcolor_t			fake;

	if ((last == NULL)||(last->name[0] != str[0])||(strcmp(last->name,str))) {
		fake.name = canoncolor(str,canon);
		last = (hsbcolor_t*) bsearch((void*)&fake,(void*)color_lib,sizeof(color_lib)/sizeof(hsbcolor_t),sizeof(fake),colorcmpf);
	}
	if (last == NULL) {
		if (isdigit(canon[0]) == FALSE) {
			fprintf(stderr,"warning: %s is not a known color\n",str);
			strcpy(buf,str);
		}
		else for (p = buf; *p = *str++; p++) if (*p == ',') *p = ' ';
	}
	else sprintf(buf,"%.3f %.3f %.3f",((double)last->h)/255,((double)last->s)/255,((double)last->b)/255);
	return buf;
}
#else
char * colorxlate(str) char (*str) {return str;}
#endif

void hsv2rgb(r,g,b,h,s,v)
double *r,*g,*b;
double h,s,v;
{
	int i;
	double f,p,q,t;

	if (s <= 0.0) {	/* achromatic */
		*r = v;
		*g = v;
		*b = v;
	}
	else {
		if (h >= 1.0) h = 0.0;
		h = 6.0 * h;
		i = (int)h;
		f = h - (double)i;
		p = v * (1 - s);
		q = v * (1 - (s * f));
		t = v * ( 1 - (s * (1 - f)));
		switch (i) {
			case 0: *r = v; *g = t; *b = p; break;
			case 1: *r = q; *g = v; *b = p; break;
			case 2: *r = p; *g = v; *b = t; break;
			case 3: *r = p; *g = q; *b = v; break;
			case 4: *r = t; *g = p; *b = v; break;
			case 5: *r = v; *g = p; *b = q; break;
		}
	}
}
