/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/*
 * Written by Stephen North.  3/1/93 release.
 * reference counted strings.
 */

#include	"libgraph.h"

typedef struct refstr_t {
	Dtlink_t		link;
	ulong 			refcnt;
	char			s[1];
} refstr_t;

#ifdef offsetof
#undef offsetof
#endif
#define offsetof(typ,fld)  ((int)(&(((typ*)0)->fld)))

static Dtdisc_t Refstrdisc = {offsetof(refstr_t,s[0]), 0,
	NULL_FN(Void_t*), NULL_FN(void), NULL_FN(int), NULL_FN(ulong)};
static Dict_t*	StringDict;

static int refstrprint(r)
     refstr_t *r;
 {
	fprintf(stderr,"%s\n",r->s); return 0;
}

#ifdef DEBUG
agrefstrdump()
{
	dtwalk(StringDict,refstrprint);
}
#endif

static void initialize_strings()
{
	StringDict	= dtopen(&Refstrdisc,Dttree);
}

char *agstrdup(s)
     char *s;
{
	refstr_t		*key,*r;

	if (StringDict == NULL) initialize_strings();
	if (s == NULL) return s;

	key = (refstr_t*)(s - offsetof(refstr_t,s[0]));
	r = (refstr_t*) dtsearch(StringDict,key);
	if (r) r->refcnt++;
	else {
		r = (refstr_t*) malloc(sizeof(refstr_t)+strlen(s));
		r->refcnt = 1;
		strcpy(r->s,s);
		dtinsert(StringDict,r);
	}
	return r->s;
}

void agstrfree(s)
     char *s;
{
	refstr_t		*key,*r;

	if ((StringDict == NULL) || (s == NULL)) return;
	key = (refstr_t*)(s - offsetof(refstr_t,s[0]));
	r = (refstr_t*) dtsearch(StringDict,key);

	if (r) {
		r->refcnt--;
		if (r->refcnt <= 0) dtdelete(StringDict,r);
	}
	else fprintf(stderr,"agstrfree lost %s\n",s);
}
