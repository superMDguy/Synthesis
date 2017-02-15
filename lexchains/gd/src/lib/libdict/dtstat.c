/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"


/*
**	Get statistics of a dictionary
**
**	Written by Kiem-Phong Vo (12/10/92)
*/

#if __STD_C
static void _dtstat(Dtstat_t* ds, Dtlink_t* root, int depth, int* level)
#else
static void _dtstat(ds,root,depth,level)
Dtstat_t*	ds;
Dtlink_t*	root;
int		depth;
int*		level;
#endif
{
	if(root->left)
		_dtstat(ds,root->left,depth+1,level);
	if(root->right)
		_dtstat(ds,root->right,depth+1,level);
	if(depth > ds->dt_n)
		ds->dt_n = depth;
	if(level)
		level[depth] += 1;
}

#if __STD_C
static void _dhstat(Dict_t* dict, Dtstat_t* ds, reg int* count)
#else
static void _dhstat(dict, ds, count)
Dict_t*		dict;
Dtstat_t*	ds;
reg int*	count;
#endif
{
	reg Dtlink_t*	t;
	reg int		n, h;

	for(h = NSLOT(dict)-1; h >= 0; --h)
	{	n = 0;
		for(t = dict->slot[h]; t; t = t->right)
			n += 1;
		if(count)
			count[n] += 1;
		else if(n > 0)
		{	ds->dt_n += 1;
			if(n > ds->dt_max)
				ds->dt_max = n;
		}
	}
}

#if __STD_C
dtstat(reg Dict_t* dict, Dtstat_t* ds, int all)
#else
dtstat(dict, ds, all)
reg Dict_t*	dict;
Dtstat_t*	ds;
int		all;
#endif
{
	reg int		i;

	ds->dt_n = ds->dt_max = 0;
	ds->dt_count = NIL(int*);

	if(dict->flat)
		_dtrestore(dict);

	ds->dt_size = dtsize(dict);

	/* dtopen() may not have set the type yet */
	if(dict->methodf == Dthash)
		dict->type |= DT_HASH;

	ds->dt_hash = ISHASH(dict) ? 1 : 0;
	if(!all)
		return 0;

	if(ISHASH(dict))
	{	_dhstat(dict,ds,NIL(int*));
		if(!(ds->dt_count = (int*)malloc((ds->dt_max+1)*sizeof(int))) )
			return -1;

		for(i = ds->dt_max; i >= 0; --i)
			ds->dt_count[i] = 0;
		_dhstat(dict,ds,ds->dt_count);
	}
	else if(dict->here)
	{	_dtstat(ds,dict->here,0,NIL(int*));
		if(!(ds->dt_count = (int*)malloc((ds->dt_n+1)*sizeof(int))) )
			return -1;

		for(i = ds->dt_n; i >= 0; --i)
			ds->dt_count[i] = 0;
		_dtstat(ds,dict->here,0,ds->dt_count);

		for(i = 0; i <= ds->dt_n; ++i)
			if(ds->dt_count[i] > ds->dt_max)
				ds->dt_max = ds->dt_count[i];
	}

	return 0;
}
