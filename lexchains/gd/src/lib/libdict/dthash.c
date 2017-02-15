/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"

/*
**	Search for an element in a hash table.
**	dict:	dictionary
**	obj:	what to look for
**	type:	type of search
**		DT_INSERT, DT_DELETE,
**		DT_RENEW, DT_REORDER,
**		DT_SEARCH, DT_NEXT, DT_PREV
**
**      Written by Kiem-Phong Vo (12/10/92)
*/

/* resize the hash table */
#if __STD_C
static void _hresize(Dict_t* dict, int nobj)
#else
static void _hresize(dict,nobj)
Dict_t	*dict;
int	nobj;
#endif
{
	reg Dtlink_t	*t, *next, **slot;
	reg int		n, s, nslot;

	if((nslot = dict->nslot) == 0)
		nslot = HSLOT;
	while(nobj > HLOAD(nslot))
		nslot = HRESIZE(nslot);

	if(!(slot = (Dtlink_t**) malloc(nslot*sizeof(Dtlink_t*))))
		return;
	for(n = nslot-1; n >= 0; --n)
		slot[n] = NIL(Dtlink_t*);

	for(n = NSLOT(dict)-1; n >= 0; --n)
	{	/* rehash */
		t = dict->slot[n];
		while(t)
		{	next = t->right;
			s = (int)HINDEX(nslot,t->hash);
			t->right = slot[s];
			slot[s] = t;
			t = next;
		}
	}

	if(dict->slot)
		free((Void_t*)dict->slot);
	dict->slot = slot;
	dict->nslot = nslot;
	dict->here = dict->prev = NIL(Dtlink_t*);
}

#if __STD_C
static Void_t* _dthash(Dict_t* dict, reg Void_t* obj, int type)
#else
static Void_t* _dthash(dict,obj,type)
Dict_t*		dict;
reg Void_t*	obj;
int		type;
#endif
{
	reg Dtlink_t	*t, *prev, *r;
	reg Dtdisc_t*	disc;
	reg ulong	hsh;
	reg int		cmp;
	reg Void_t*	o;
	reg Dtlink_t	**slot, **eslot;
	reg Dtmake_f	makef;
	reg Dtfree_f	freef;

	if(!(disc = dict->disc))
		disc = &_Dtnil;
	makef = disc->makef;
	freef = disc->freef;

	if(dict->flat)
		_dtrestore(dict);

	dict->type |= DT_HASH;

	if(!obj)
	{	if(dict->nobj <= 0 || !(type&(DT_DELETE|DT_NEXT|DT_PREV)) )
			return NIL(Void_t*);

		eslot = (slot = dict->slot) + NSLOT(dict);

		if(type&DT_DELETE)
		{	/* clean out all objects */
			for(; slot < eslot; ++slot)
			{	if(freef || makef)
				{	for(t = *slot; t;)
					{	r = t->right;
						if(freef)
							(*freef)(dict,OBJ(t,makef),disc);
						if(makef)
							free((Void_t*)t);
						t = r;
					}
				}
				*slot = NIL(Dtlink_t*);
			}
			dict->here = dict->prev = NIL(Dtlink_t*);
			dict->nobj = 0;
			return NIL(Void_t*);
		}
		else	/* computing the first/last object */
		{	t = prev = NIL(Dtlink_t*);
			while(slot < eslot)
				if((t = (type&DT_PREV) ? *--eslot : *slot++) )
					break;
			if(t && (type&DT_PREV))
				for(; t->right; prev = t, t = t->right)
					;

			dict->here = t;
			dict->prev = prev;
			return t ? OBJ(t,makef) : NIL(Void_t*);
		}
	}

	if((type&(DT_RENEW|DT_REORDER)) || !makef)
	{	r = (Dtlink_t*)obj;
		obj = OBJ(r,makef);
	}

	if(!(t = dict->here) || OBJ(t,makef) != obj)
	{	/* try to find it */
		if(type&DT_REORDER)
			hsh = r->hash;
		else	HASH(dict,hsh,obj,disc);

		prev = NIL(Dtlink_t*);
		if(!dict->nslot)
			t = NIL(Dtlink_t*);
		else
		{	/* look in the hash bucket */
			t = *(dict->slot + HINDEX(dict->nslot,hsh));
			for(; t; prev = t, t = t->right)
			{	if(hsh != t->hash)
					continue;
				o = OBJ(t,makef);
				CMP(dict,cmp,obj,o,disc);
				if(cmp == 0)
					break;
			}
		}
	}
	else	
	{	/* current object is good */
		prev = dict->prev;
		hsh = t->hash;
	}

	if(!t)
	{	/* can't find it */
		if(!(type&(DT_RENEW|DT_REORDER|DT_INSERT)))
			return NIL(Void_t*);

		/* about to insert, resize table if necessary */
		if(dict->nslot >= 0 && (dict->nobj+1) > HLOAD(dict->nslot))
			_hresize(dict,dict->nobj+1);
		if(dict->nslot == 0)
			return NIL(Void_t*);

		if(type&(DT_RENEW|DT_REORDER))
			t = r;
		else if(!makef)
			t = (Dtlink_t*) obj;
		else if(t = (Dtlink_t*) malloc(sizeof(Dthold_t)))
			((Dthold_t*)t)->_dobj = (*makef)(dict,obj,disc);
		else	return NIL(Void_t*);

		/* insert object */
		dict->nobj += 1;
		t->hash = hsh;
		slot = dict->slot + HINDEX(dict->nslot,hsh);
		t->right = *slot;
		*slot = t;
		dict->prev = NIL(Dtlink_t*);
		dict->here = t;
		return OBJ(t,makef);
	}

	slot = dict->slot + HINDEX(dict->nslot,hsh);
	switch(type)
	{
	default:
	case DT_SEARCH:	/* move to front heuristic */
		if(prev && prev != (r = *slot) && prev != r->right)
		{	prev->right = t->right;
			t->right = *slot;
			*slot = t;
			prev = NIL(Dtlink_t*);
		}
		break;
	case DT_NEXT:	/* get next element */
		if((r = t->right) )
		{	prev = t;
			t = r;
		}
		else
		{	t = prev = NIL(Dtlink_t*);
			eslot = dict->slot + NSLOT(dict);
			for(slot += 1; slot < eslot; ++slot)
				if((t = *slot) != NIL(Dtlink_t*))
					break;
		}
		break;
	case DT_PREV:	/* compute previous element */
		if(prev)
		{	t = prev;
			if(*slot == prev)
				prev = NIL(Dtlink_t*);
			else for(prev = *slot; prev->right != t; prev = prev->right)
				;
		}
		else
		{	t = prev = NIL(Dtlink_t*);
			eslot = dict->slot;
			for(slot -= 1; slot >= eslot; --slot)
			{	if(!(t = *slot))
					continue;
				for(; t->right; prev = t, t = t->right)
					;
				break;
			}
		}
		break;
	case DT_DELETE:	/* get it off the hash table */
		if(prev)
			prev->right = t->right;
		else	*slot = t->right;
		r = t->right;
		if(freef)
			(*freef)(dict,OBJ(t,makef),disc);
		if(makef)
			free((Void_t*)t);
		dict->nobj -= 1;
		t = r;
		break;
	case DT_RENEW:	/* a duplicate */
	case DT_REORDER:
		if(freef)
			(*freef)(dict,OBJ(r,makef),disc);
		if(makef)
			free((Void_t*)r);
		break;
	}

	dict->prev = prev;
	dict->here = t;
	return (!t || type == DT_DELETE) ? NIL(Void_t*) : OBJ(t,makef);
}

/* make this method available */
Dtmethod_t	Dthash = _dthash;
