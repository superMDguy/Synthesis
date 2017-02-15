/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"

/*
**	Walk a dictionary and all dictionaries viewed through it.
**	userf:	user function
**
**	Written by Kiem-Phong Vo (12/10/92)
*/

#if __STD_C
dtwalk(reg Dict_t* dict, reg int (*userf)(Void_t*))
#else
dtwalk(dict,userf)
reg Dict_t	*dict;
reg int		(*userf)();
#endif
{
	reg Dtlink_t*	e;
	reg int		rv;
	reg Dtmake_f	makef;

	makef = dict->disc ? dict->disc->makef : NIL(Dtmake_f);

	rv = 0;
	for(e = dtflatten(dict); e; e = e->right)
		if((rv = (*userf)(OBJ(e,makef))) < 0)
			break;
	_dtrestore(dict);

	return rv;
}
