/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"

/*
**	Change searching method.
**
**	Written by Kiem-Phong Vo (12/08/92)
*/

#if __STD_C
Dtmethod_t dtmethod(reg Dict_t* dict, Dtmethod_t methodf, reg int size)
#else
Dtmethod_t dtmethod(dict, methodf, size)
reg Dict_t*	dict;
reg Dtmethod_t	methodf;
reg int		size;
#endif
{
	reg Dtlink_t	*root, *t;
	reg int		s;
	reg Dtmethod_t	oldmeth = dict->methodf;

	if(dict->flat)
		_dtrestore(dict);

	if(methodf == oldmeth)
		return oldmeth;

	if(methodf == Dthash )
	{	/* user requested a specific size */
		if(size > 0 && NSLOT(dict) != size)
		{	free((Void_t*)dict->slot);
			dict->nslot = 0;
			dict->slot = (Dtlink_t**)malloc(size*sizeof(Dtlink_t*));
			if(!dict->slot)
				return NIL(Dtmethod_t);

			dict->nslot = -size;
		}

		for(s = NSLOT(dict)-1; s >= 0; --s)
			dict->slot[s] = NIL(Dtlink_t*);

		dict->type |= DT_HASH;
		dict->prev = NIL(Dtlink_t*);
		dict->methodf = methodf;
		if(!dict->view)
			dict->searchf = methodf;

		dict->nobj = 0;
		root = dict->here;
		dict->here = NIL(Dtlink_t*);

		while(root)
		{	while((t = root->left) )
				RROTATE(root,t);
			t = root->right;
			(void)(*methodf)(dict,(Void_t*)root,DT_RENEW);
			root = t;
		}
	}
	else if(methodf == Dttree)
	{	/* reinsert all objects into a single search tree */
		dict->nobj = 0;
		dict->here = NIL(Dtlink_t*);
		dict->prev = NIL(Dtlink_t*);
		dict->type &= ~DT_HASH;
		dict->methodf = methodf;
		if(!dict->view)
			dict->searchf = methodf;

		for(s = NSLOT(dict)-1; s >= 0; --s)
		{	root = dict->slot[s];
			while(root)
			{	t = root->right;
				(void)(*methodf)(dict,(Void_t*)root,DT_RENEW);
				root = t;
			}
		}
	
		if(dict->slot)
			free((Void_t*)dict->slot);
		dict->slot = NIL(Dtlink_t**);
		dict->nslot = 0;
	}
	else	return NIL(Dtmethod_t);

	return oldmeth;
}
