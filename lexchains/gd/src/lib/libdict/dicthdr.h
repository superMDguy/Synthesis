/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#ifndef _DICTHDR_H
#define _DICTHDR_H	1

/*	Internal definitions for libdict.
**	Written by Kiem-Phong Vo (09/11/90)
**	AT&T Bell Laboratories
*/

#include	"FEATURE/dict"

#if __STD_C
#include	<stddef.h>
#else
#if _sys_types
#include	<sys/types.h>
#else
typedef unsigned int	size_t;
#endif
#endif

/* short-hand notations */
#define reg		register
#define ulong		unsigned long
typedef struct _dthold_	Dthold_t;

#define _DICT_PRIVATE \
	short		nview;	/* number of parent view dictionaries	*/ \
	Dtdisc_t*	disc;	/* methods to manipulate objs		*/ \
	int		nslot;	/* number of hash slots			*/ \
	Dtlink_t**	slot;	/* hash slots				*/ \
	struct _dict_s*	walk;	/* dict on viewpath being walked	*/

#include	"dict.h"

/* bits used in the type field */
#define DT_FRWD		00010		/* forward walk				*/
#define DT_BKWD		00020		/* backward walk			*/

/* bits used for secondary hashing */
#define HIGHBIT		(~(((ulong)~0L) >> 1))
#define SETVISIT(h)	((h) | HIGHBIT)
#define CLRVISIT(h)	((h) & ~HIGHBIT)
#define ISVISIT(h)	((h) & HIGHBIT)
#define REHASH(h)	(((h)<<5) - (h))

/* hash start size and load factor */
#define HSLOT		(4)
#define HRESIZE(n)	((n) << 1)
#define HLOAD(s)	((s) << 1)
#define HINDEX(n,h)	((n) < 0 ? (h)%(-(n)) : (h)&((n)-1))
#define NSLOT(d)	((d)->nslot < 0 ? -(d)->nslot : (d)->nslot)
#define ISHASH(d)	((d)->type&DT_HASH)

/* the pointer to the actual object */
#define OBJ(e,mkf)	((mkf) ? ((Dthold_t*)(e))->_dobj : (Void_t*)(e))

/* compare function */
#define CMP(dt,cmp,o1,o2,d) \
	{ if((o1) == (o2))	(cmp) = 0; \
	  else if((d)->key < 0) \
	  { if((d)->comparf) (cmp) = (*(d)->comparf)((dt),(char*)(o1),(char*)(o2),(d)); \
	    else	     (cmp) = (o1) < (o2) ? -1 : (o1) == (o2) ? 0 : 1; \
	  } else \
	  { reg char	*_do1 = (char*)(o1)+(d)->key, *_do2 = (char*)(o2)+(d)->key; \
	    if((d)->size < 0)	{ _do1 = *((char**)_do1); _do2 = *((char**)_do2); } \
	    if((d)->comparf)	(cmp) = (*(d)->comparf)((dt),_do1,_do2,(d)); \
	    else if(((cmp) = *_do1 - *_do2) == 0) \
	    { if((d)->size > 0)	(cmp) = memcmp(_do1,_do2,(d)->size); \
	      else		(cmp) = strcmp(_do1,_do2); \
	    } \
	  } \
	}

/* hash function */
#define HASH(dt,hash,o,d) \
	{ if((d)->key < 0) \
	  { if((d)->hashf)	(hash) = (*(d)->hashf)((dt),(char*)o,(d)); \
	    else		(hash) = ((ulong)(o)>>2) + ((ulong)o); \
	  } else \
	  { reg char	*_do = (char*)(o) + (d)->key; \
	    if((d)->size < 0)	_do = *((char**)_do); \
	    if((d)->hashf)	(hash) = (*(d)->hashf)((dt),_do,(d)); \
	    else		dtstrhash((hash),_do,(d)->size <= 0 ? -1 : (d)->size); \
	  } \
	}

/* short-hand for node fields */
#define hash	_dtleft._dhash
#define left	_dtleft._dleft
#define right	_dtright

/* tree rotation functions */
#define RROTATE(x,y)	((x)->left = (y)->right, (y)->right = (x), (x) = (y))
#define LROTATE(x,y)	((x)->right = (y)->left, (y)->left = (x), (x) = (y))

_BEGIN_EXTERNS_
extern Dtdisc_t	_Dtnil;
extern Void_t*	malloc _ARG_((int));
extern void	free _ARG_((Void_t*));
extern int	memcmp _ARG_((const Void_t*, const Void_t*, size_t));
extern int	strcmp _ARG_((const char*, const char*));
_END_EXTERNS_

#endif /* _DICTHDR_H */
