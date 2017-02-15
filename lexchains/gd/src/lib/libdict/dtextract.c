/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"

/*	Extract objects of a dictionary.
**
**	Written by Kiem-Phong Vo (12/10/93).
*/

#if __STD_C
Dtlink_t* _dtextract(reg Dict_t* dict)
#else
Dtlink_t* _dtextract(dict)
reg Dict_t*	dict;
#endif
{
	if(dict->flat)
		_dtrestore(dict);

	dict->walk = dict->flat = NIL(Dict_t*);

	if(ISHASH(dict)) /* make a big list of everything */
	{	reg Dtlink_t	*le, *t, **slot, **eslot, *list;

		list = le = NIL(Dtlink_t*);
		for(eslot = (slot = dict->slot) + NSLOT(dict); slot < eslot; ++slot)
		{	if(!(t = *slot))
				continue;

			le = le ? (le->right = t) : (list = t);
			for(; le->right; le = le->right)
				;

			*slot = NIL(Dtlink_t*);
		}
		dict->here = list;
	}

	return dict->here;
}
