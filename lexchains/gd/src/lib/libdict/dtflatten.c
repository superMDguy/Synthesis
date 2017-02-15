/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"


/*	Flatten a dictionary into a linked list.
**	This may be used when many traversals are likely
**
**	Written by Kiem-Phong Vo (12/10/92).
*/

#if __STD_C
static void _dtflat(reg Dict_t* d, reg Dict_t* dict)
#else
static void _dtflat(d, dict)
reg Dict_t*	d;	/* dictionary to be flattened */
reg Dict_t*	dict;	/* top view dictionary */
#endif
{
	reg Dtlink_t	*t, *open, *seen, *lo, *ls;

	/* construct list from bottom up */
	if(d->view)
		_dtflat(d->view,dict);

	if(d->flat)
		_dtrestore(d);

	d->flat = dict;
	d->walk = NIL(Dict_t*);

	/* make a big list of everything */
	open = lo = NIL(Dtlink_t*);
	if(ISHASH(d))
	{	reg Dtlink_t	**slot, **eslot;

		for(eslot = (slot = d->slot) + NSLOT(d); slot < eslot; ++slot)
		{	if(!(t = *slot))
				continue;

			lo = lo ? (lo->right = t) : (open = t);
			for(; lo->right; lo = lo->right)
				;

			*slot = NIL(Dtlink_t*);
		}
	}
	else if((open = d->here) )
	{	/* make all nodes have no left children */
		while((t = open->left) )
			RROTATE(open,t);
		lo = open;
		while((ls = lo->right) )
		{	while((t = ls->left) )
				RROTATE(ls,t);
			lo->right = ls;
			lo = ls;
		}
	}

	/* partition into 2 lists */
	seen = ls = t = NIL(Dtlink_t*);
	if(d != dict)
	{	reg Void_t*	obj;
		reg Dict_t*	p;
		reg Dtmake_f	makef;

		makef = d->disc ? d->disc->makef : NIL(Dtmake_f);

		for(lo = open; lo; )
		{	obj = OBJ(lo,makef);
			for(p = dict; p != d; p = p->view)
				if((*(p->methodf))(p,obj,DT_SEARCH) )
					goto is_cover;

			t = lo;
			goto next;

		is_cover:
			if(seen)
			{	ls->right = lo;
				ls = lo;
			}
			else	seen = ls = lo;

			if(t)
				t->right = lo->right;
			else	open = lo->right;

		next:
			lo = lo->right;
		}
	}

	d->prev = seen;
	if(seen)
		ls->right = NIL(Dtlink_t*);

	if((lo = open) )
	{	for(; lo->right; lo = lo->right)
			;
		if(d->view)
			lo->right = d->view->here;
	}
	else if(d->view)
		open = d->view->here;

	d->here = open;
}

#if __STD_C
Dtlink_t* dtflatten(Dict_t* dict)
#else
Dtlink_t* dtflatten(dict)
Dict_t	*dict;
#endif
{	if(dict->flat && dict->flat != dict)
		_dtrestore(dict);

	if(!dict->flat)
		_dtflat(dict,dict);

	return dict->here;
}
