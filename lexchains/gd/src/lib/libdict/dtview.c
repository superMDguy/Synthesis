/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"

/*	Set a view path from dict to view.
**	This is allowed only if the two dictionaries are of the same type.
**
**	Written by Kiem-Phong Vo (12/10/92)
*/


#if __STD_C
static Void_t* _dvsearch(Dict_t* dict, reg Void_t* obj, reg int type)
#else
static Void_t* _dvsearch(dict,obj,type)
Dict_t*		dict;
reg Void_t*	obj;
reg int		type;
#endif
{
	reg Dict_t	*d, *p;
	reg Void_t*	o;

	/* these operations only happen at the top level */
	if(type&(DT_INSERT|DT_DELETE))
		return (*(dict->methodf))(dict,obj,type);

	if(type&DT_SEARCH)
	{	if(obj) for(d = dict; d; d = d->view)
		{	if((o = (*(d->methodf))(d,obj,DT_SEARCH)) )
			{	dict->walk = d;
				return o;
			}
		}

		dict->walk = NIL(Dict_t*);
		return NIL(Void_t*);
	}

	if(type&DT_NEXT)
	{	if(!obj || !dict->walk)
			dict->walk = dict;
		type = DT_NEXT;
	}
	else if(type&DT_PREV)
	{	if(!obj || !dict->walk)
		{	for(d = dict; d->view; d = d->view)
				;
			dict->walk = d;
		}
		type = DT_PREV;
	}
	else	return NIL(Void_t*);

	for(d = dict->walk; d; )
	{	/* try to find adjacent element */	
		o = (*(d->methodf))(d,obj,type) ;
		while(o)
		{	for(p = dict; ; p = p->view)
			{	/* this object is uncovered */	
				if(p == d)
					return o;

				/* it is covered */
				if((*(p->methodf))(p,o,DT_SEARCH) )
					break;
			}

			/* try next one */
			o = (*(d->methodf))(d,o,type);
		}

		if(type == DT_NEXT)
			d = d->view;
		else
		{	if(dict->walk == dict)
				p = NIL(Dict_t*);
			else for(p = dict; p; p = p->view)
				if(p->view == d)
					break;
			d = p;
		}

		dict->walk = d;
		obj = NIL(Void_t*);
	}

	return NIL(Void_t*);
}

#if __STD_C
Dict_t* dtview(reg Dict_t* dict, reg Dict_t* view)
#else
Dict_t* dtview(dict,view)
reg Dict_t	*dict, *view;
#endif
{
	reg Dict_t	*d;

	if(dict->flat)
		_dtrestore(dict);

	/* no more viewing lower dictionary */
	if((d = dict->view) )
		d->nview -= 1;
	dict->view = NIL(Dict_t*);
	dict->walk = NIL(Dict_t*);

	if(!view)
	{	dict->searchf = dict->methodf;
		return d;
	}

	if(view->flat)
		_dtrestore(view);

	/* make sure they are of the same type */
#define KEY(m)		(m ? m->key : -1)
#define SIZE(m)		(m ? m->size : 0)
#define MAKEF(m)	(m ? m->makef : NIL(Dtmake_f))
	if(dict->disc != view->disc &&
	   (KEY(dict->disc) != KEY(view->disc) ||
	    SIZE(dict->disc) != SIZE(view->disc) ||
	    MAKEF(dict->disc) != MAKEF(view->disc)) )
		return NIL(Dict_t*);

	/* make sure there won't be a cycle */
	for(d = view; d; d = d->view)
		if(d == dict)
			return NIL(Dict_t*);

	/* ok */
	dict->view = view;
	dict->searchf = _dvsearch;
	view->nview += 1;

	return view;
}
