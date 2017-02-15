/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"

/*
**	Look up an element in a tree.
**	dict:	dictionary being searched
**	obj:	the object to look for.
**	type:	a bit vector with fields:
**		DT_INSERT: insert if not found.
**		DT_DELETE: delete if found.
**		DT_NEXT: find next elt after obj
**		DT_PREV: find elt before obj
**		DT_RENEW or DT_REORDER: during a reordering
**
**      Written by Kiem-Phong Vo (12/10/92)
*/

#define RLINK(r,x)	(r = r->left = x )
#define LLINK(l,x)	(l = l->right = x )

#if __STD_C
static Void_t* _dttree(Dict_t* dict, reg Void_t* obj, reg int type)
#else
static Void_t* _dttree(dict,obj,type)
Dict_t*		dict;
reg Void_t* 	obj;
int		type;
#endif
{
	reg int		cmp;
	reg Void_t*	o;
	reg Dtlink_t	*root, *l, *r, *t, *me;
	reg Dtdisc_t*	disc;
	reg Dtmake_f	makef;
	reg Dtfree_f	freef;
	Dtlink_t	link;

	if(!(disc = dict->disc))
		disc = &_Dtnil;
	makef = disc->makef;
	freef = disc->freef;

	if(dict->flat)
		_dtrestore(dict);

	root = dict->here;

	if(!obj)
	{	if(!root || !(type&(DT_DELETE|DT_NEXT|DT_PREV)) )
			return NIL(Void_t*);

		if(type&DT_DELETE) /* delete all objects */
		{	if(freef || makef)
			{	do
				{	while((t = root->left) )
						RROTATE(root,t);
					t = root->right;
					if(freef)
						(*freef)(dict,OBJ(root,makef),disc);
					if(makef)
						free((Void_t*)root);
				} while((root = t) );
			}

			dict->nobj = 0;
			dict->here = NIL(Dtlink_t*);
			return NIL(Void_t*);
		}
		else /* computing largest/smallest element */
		{	if(type&DT_PREV)
			{	while((t = root->right) )
					LROTATE(root,t);
			}
			else /* type&DT_NEXT */
			{	while((t = root->left) )
					RROTATE(root,t);
			}

			dict->here = root;
			return OBJ(root,makef);
		}
	}

	if((type&(DT_RENEW|DT_REORDER)) || !makef)
	{	me = (Dtlink_t*) obj;
		obj = OBJ(me,makef);
	}

	/* note that link.right is LEFT tree and link.left is RIGHT tree */
	l = r = &link;

	if(root && obj != OBJ(root,makef))
	{	do /* top-down splaying */
		{	o = OBJ(root,makef);
			CMP(dict,cmp,obj,o,disc);

			if(cmp == 0)
				break;
			else if(cmp < 0)	/* left turn */
			{	if((t = root->left) )
				{	o = OBJ(t,makef);
					CMP(dict,cmp,obj,o,disc);
					if(cmp <= 0)	/* left, left */
					{	RROTATE(root,t);
						if(cmp == 0)
							break;
						t = root->left;
					}
					else		/* left, right */
					{	LLINK(l,t);
						t = t->right;
					}
				}
				RLINK(r,root);
			}
			else			/* right turn */
			{	if((t = root->right) )
				{	o = OBJ(t,makef);
					CMP(dict,cmp,obj,o,disc);
					if(cmp >= 0)	/* right, right */
					{	LROTATE(root,t);
						if(cmp == 0)
							break;
						t = root->right;
					}
					else		/* right, left */
					{	RLINK(r,t);
						t = t->left;
					}
				}
				LLINK(l,root);
			}
		} while((root = t) );
	}

	if(root)
	{	/* found it, now isolate it */
		l->right = root->left;
		r->left = root->right;

		if(type&DT_NEXT)	/* looking for immediate successor */
		{	root->left = link.right;	/* put root to LEFT tree */
			root->right = NIL(Dtlink_t*);
			link.right = root;
			goto dt_next;
		}
		else if(type&DT_PREV)	/* looking for immediate predecessor */
		{	root->right = link.left;	/* put root to RIGHT tree */
			root->left = NIL(Dtlink_t*);
			link.left = root;
			goto dt_prev;
		}
		else if(type&DT_DELETE)
		{	if(freef)
				(*freef)(dict,OBJ(root,makef),disc);
			if(makef)
				free((Void_t*)root);
			if(dict->nobj > 0)
				dict->nobj -= 1;
			root = NIL(Dtlink_t*);
		}
		else if(type&(DT_RENEW|DT_REORDER))
		{	/* duplicated elt */
			if(freef)
				(*freef)(dict,OBJ(me,makef),disc);
			if(makef)
				free((Void_t*)me);
		}
	}
	else
	{	/* not found, finish up LEFT and RIGHT trees */
		r->left = NIL(Dtlink_t*);
		l->right = NIL(Dtlink_t*);

		if(type&DT_NEXT)
		{ dt_next : /* immediate successor is smallest in RIGHT tree */
			if((root = link.left) )	
			{	while((t = root->left) )
					RROTATE(root,t);
				link.left = root->right;
			}
		}
		else if(type&DT_PREV)
		{ dt_prev : /* immediate predecessor is largest in LEFT tree */
			if((root = link.right) )
			{	while((t = root->right) )
					LROTATE(root,t);
				link.right = root->left;
			}
		}
		else if(type&(DT_RENEW|DT_REORDER))
		{	root = me;
			if(dict->nobj >= 0)
				dict->nobj += 1;
		}
		else if(type&DT_INSERT)
		{	/* create a new object to insert */
			if(!makef)
				root = me;
			else if(root = (Dtlink_t*) malloc(sizeof(Dthold_t)))
				((Dthold_t*)root)->_dobj = (*makef)(dict,obj,disc);
			if(root && dict->nobj >= 0)
				dict->nobj += 1;
		}
	}

	if(root) /* got it, reconstruct the tree with it as root */
	{	root->left = link.right;
		root->right = link.left;
		dict->here = root;
		return OBJ(root,makef);
	}
	else	/* not found, reconstruct tree by hooking LEFT tree to RIGHT tree */
	{	while((t = r->left) )
			r = t;
		r->left = link.right;
		dict->here = link.left;
		return NIL(Void_t*);
	}
}


/* make this method available */
Dtmethod_t	Dttree = _dttree;
