/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"
static char     *Version = "\n@(#)Version 07/07/93-dict-kpv\n";

/* 	Make a new dictionary
**
**	Written by Kiem-Phong Vo (12/10/92)
*/

Dtdisc_t	_Dtnil =
	{ -1, 0, NIL(Dtmake_f), NIL(Dtfree_f), NIL(Dtcompar_f), NIL(Dthash_f) };

#if __STD_C
Dict_t* dtopen(Dtdisc_t* disc, Dtmethod_t methodf)
#else
Dict_t*	dtopen(disc, methodf)
Dtdisc_t*	disc;
Dtmethod_t	methodf;
#endif
{
	reg Dict_t*	dict = (Dict_t*)Version;	/* shut-up unuse warning */

	if(!methodf)
		return NIL(Dict_t*);

	/* allocate space for dictionary */
	if(!(dict = (Dict_t*) malloc(sizeof(Dict_t))))
		return NIL(Dict_t*);

	dict->methodf = dict->searchf = methodf;
	dict->disc = disc;
	dict->nobj = 0;
	dict->type = 0;
	dict->nview = 0;
	dict->here = NIL(Dtlink_t*);
	dict->prev = NIL(Dtlink_t*);
	dict->nslot = 0;
	dict->slot = NIL(Dtlink_t**);
	dict->view = NIL(Dict_t*);
	dict->walk = NIL(Dict_t*);
	dict->flat = NIL(Dict_t*);

	if(disc && disc->makef)
		dict->type |= DT_MAKEF;

	return dict;
}
