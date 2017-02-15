/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"

/*	Restore dictionaries that have been flattened.
**
**	Written by Kiem-Phong Vo (AT&T Bell Laboratories, 12/10/92)
*/

#if __STD_C
void _dtrestore(reg Dict_t* dict)
#else
void _dtrestore(dict)
reg Dict_t*	dict;
#endif
{
	reg Dtlink_t	*t, *r, *head, *last;
	reg ulong	hsh;
	reg int		ind, n, i, nobj, type;

	if(!(dict = dict->flat) )
		return;

	for(; dict; dict = dict->view)
	{	dict->flat = NIL(Dict_t*);

		if(ISHASH(dict))
		{	n = dict->nslot;
			nobj = 0;

			for(type = 0; type < 2; ++type)
			{	head = type == 0 ? dict->here : dict->prev;
				last = (type == 0 && dict->view) ? dict->view->here :
						NIL(Dtlink_t*);
					
				while(head != last)
				{	hsh = head->hash;
					ind = (int)HINDEX(n,hsh);
					nobj += 1;

					/* get all that go into same slot */
					for(r = head; r->right; nobj += 1, r = r->right) 
					{	hsh = r->right->hash;
						if((i = (int)HINDEX(n,hsh)) != ind)
							break;
					}

					t = r->right;
					r->right = dict->slot[ind];
					dict->slot[ind] = head;
					head = t;
				}
			}

			dict->nobj = nobj;
			dict->here = dict->prev = NIL(Dtlink_t*);
		}
		else
		{	head = dict->here;
			last = dict->view ? dict->view->here : NIL(Dtlink_t*);
			if(head == last)
			{	head = NIL(Dtlink_t*);
				nobj = 0;
			}
			else
			{	nobj = 1;
				for(t = head; t->right != last; nobj += 1, t = t->right)
					;
				t->right = NIL(Dtlink_t*);
			}
			dict->here = head;
			dict->nobj = nobj;

			if(dict->here)
			{	for(t = dict->prev; t; )
				{	r = t->right;
					(*(dict->methodf))(dict,(Void_t*)t,DT_RENEW);
					t = r;
				}
			}
			else
			{	for(t = dict->prev; t; t = t->right)
					nobj += 1;
				dict->here = dict->prev;
				dict->nobj = nobj;
			}
		}

		dict->prev = NIL(Dtlink_t*);
	}
}
