/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"

/*	Return the # of objects in the dictionary
**
**	Written by Kiem-Phong Vo (07/27/93)
*/

#if __STD_C
static _dtcount(reg Dtlink_t* here)
#else
static _dtcount(here)
reg Dtlink_t*	here;
#endif
{	reg int	size = 1;

	if(here->left)
		size += _dtcount(here->left);
	if(here->right)
		size += _dtcount(here->right);

	return size;
}

#if __STD_C
_dtsize(Dict_t* dict)
#else
_dtsize(dict)
Dict_t*	dict;
#endif
{
	if(!ISHASH(dict) && dict->nobj < 0)
		dict->nobj = dict->here ? _dtcount(dict->here) : 0;
	return dict->nobj;
}
