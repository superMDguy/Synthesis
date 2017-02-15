/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"

/*
**	Change the discipline structure.
**	Duplicates caused by the new functions will be removed.
**	dict :	dictionary
**	disc :	discipline
**
**	Written by Kiem-Phong Vo (12/10/92)
*/

#if __STD_C
Dtdisc_t* dtdisc(Dict_t* dict, Dtdisc_t* disc)
#else
Dtdisc_t* dtdisc(dict,disc)
Dict_t*		dict;
Dtdisc_t*	disc;
#endif
{
	reg Dtlink_t	*root, *t, *next;
	reg int		s, rehash;
	reg Dtdisc_t*	old = dict->disc;

	if(!disc)
		return old;

	if(disc->makef != old->makef || disc->freef != old->freef)
		return NIL(Dtdisc_t*);

	if(dict->view || dict->nview > 0)
		return NIL(Dtdisc_t*);

	dict->walk = NIL(Dict_t*);
	dict->disc = disc;
	dict->nobj = 0;

	if(disc->key != old->key || disc->size != old->size ||
	   disc->hashf != old->hashf)
		rehash = 1;
	else	rehash = 0;

	if(ISHASH(dict))
	{	/* collect all elements into a list */
		if((s = NSLOT(dict)-1) == 0)
		{	root = dict->slot[0];
			dict->slot[0] = NIL(Dtlink_t*);
		}
		else
		{	root = NIL(Dtlink_t*);
			for(; s >= 0; --s)
			{	t = dict->slot[s];
				dict->slot[s] = NIL(Dtlink_t*);
				while(t)
				{	next = t->right;
					t->right = root;
					root = t;
					t = next;
				}
			}
		}

		/* reinsert them */
		dict->here = dict->prev = NIL(Dtlink_t*);
		while(root)
		{	next = root->right;
			if(rehash)
			{	reg Void_t*	obj = OBJ(root,old->makef);
				reg ulong	hsh;
				HASH(dict,hsh,obj,disc);
				root->hash = hsh;
			}
			(void)(*(dict->methodf))(dict,(Void_t*)root,DT_REORDER);
			root = next;
		}
	}
	else
	{	root = dict->here;
		dict->here = NIL(Dtlink_t*);
		while(root)
		{	while((t = root->left) )
				RROTATE(root,t);
			next = root->right;
			(void)(*(dict->methodf))(dict,(Void_t*)root,DT_REORDER);
			root = next;
		}
	}

	return old;
}
