/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dicthdr.h"

/*	Close a dictionary
**
**	Written by Kiem-Phong Vo (06/18/91)
*/
#if __STD_C
dtclose(reg Dict_t* dict)
#else
dtclose(dict)
reg Dict_t	*dict;
#endif
{
	if(dict->nview > 0)
		return -1;
	if(dict->view)
		dtview(dict,NIL(Dict_t*));
	(void)(*(dict->searchf))(dict,NIL(Void_t*),DT_DELETE);
	free((Void_t*)dict);
	return 0;
}
