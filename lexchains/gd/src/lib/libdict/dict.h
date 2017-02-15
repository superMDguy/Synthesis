/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#ifndef _DICT_H
#define _DICT_H		1

/*
**	Public interface for the dictionary library
**
**      Written by Kiem-Phong Vo (12/04/92)
**	AT&T Bell Laboratories
*/

#ifndef __KPV__
#define __KPV__		1

#ifndef __STD_C
#ifdef __STDC__
#define	__STD_C		1
#else
#if __cplusplus
#define __STD_C		1
#else
#define __STD_C		0
#endif /*__cplusplus*/
#endif /*__STDC__*/
#endif /*__STD_C*/

#ifndef _BEGIN_EXTERNS_
#if __cplusplus
#define _BEGIN_EXTERNS_	extern "C" {
#define _END_EXTERNS_	}
#else
#define _BEGIN_EXTERNS_
#define _END_EXTERNS_
#endif
#endif /*_BEGIN_EXTERNS_*/

#ifndef _ARG_
#if __STD_C
#define _ARG_(x)	x
#else
#define _ARG_(x)	()
#endif
#endif /*_ARG_*/

#ifndef Void_t
#if __STD_C
#define Void_t		void
#else
#define Void_t		char
#endif
#endif /*Void_t*/

#ifndef NIL
#define NIL(type)	((type)0)
#endif /*NIL*/

#endif /*__KPV__*/

typedef struct _dtlink_s	Dtlink_t;
typedef struct _dtdisc_s	Dtdisc_t;
typedef struct _dtstat_s	Dtstat_t;
typedef struct _dict_s		Dict_t;

struct _dtlink_s
{	Dtlink_t*	_dtright;	/* right child		*/
	union
	{
	Dtlink_t*	_dleft;		/* left child		*/
	unsigned long	_dhash;		/* hash value		*/
	} _dtleft;
};

/* private structure to hold an object */
struct _dthold_
{	Dtlink_t	_dhdr;		/* header		*/
	Void_t*		_dobj;		/* user object		*/
};

/* method to manipulate dictionary structure */
typedef Void_t*		(*Dtmethod_t)_ARG_((Dict_t*, Void_t*, int));

/* structure to hold methods that manipulate an object		*/
typedef Void_t* 	(*Dtmake_f)_ARG_((Dict_t*, Void_t*, Dtdisc_t*));
typedef void 		(*Dtfree_f)_ARG_((Dict_t*, Void_t*, Dtdisc_t*));
typedef int		(*Dtcompar_f)_ARG_((Dict_t*, char*, char*, Dtdisc_t*));
typedef unsigned long	(*Dthash_f)_ARG_((Dict_t*, char*, Dtdisc_t*));
struct _dtdisc_s
{	int		key;	/* where key begins in object		*/
	int		size;	/* key size				*/
	Dtmake_f	makef;	/* object constructor			*/
	Dtfree_f	freef;	/* object destructor			*/
	Dtcompar_f	comparf;/* to compare two objects		*/
	Dthash_f	hashf;	/* to compute a hash value for objects	*/
};

struct _dict_s
{	Dtmethod_t	searchf;	/* method for searching		*/
	Dtmethod_t	methodf;	/* actual dictionary method	*/
	Dtlink_t*	here;		/* current elt or root		*/
	Dtlink_t*	prev;		/* elt before here when hashed	*/
	Dict_t*		flat;		/* top dict when flattened	*/
	Dict_t*		view;		/* next on viewpath		*/
	int		nobj;		/* number of objects		*/
	short		type;		/* type of dictionary		*/
#ifdef _DICT_PRIVATE
	_DICT_PRIVATE
#endif
};

struct _dtstat_s
{	int	dt_hash;	/* 1 if is a hash table			*/
	int	dt_size;	/* total  number of objects		*/
	int	dt_n;		/* number of chains or levels		*/
	int	dt_max;		/* max size of a chain or a level	*/
	int*	dt_count;	/* counts of chains or levels by size	*/
};

/* type of dictionary */
#define DT_HASH		000001	/* in hash mode				*/
#define DT_MAKEF	000002	/* has make-object function		*/

/* types of search - for internal use only */
#define DT_INSERT	000001	/* insert object if not found		*/
#define DT_DELETE	000002	/* delete object if found		*/
#define DT_SEARCH	000004	/* look for an object			*/
#define DT_NEXT		000010	/* look for the next element		*/
#define DT_PREV		000020	/* find previous element		*/
#define DT_RENEW	000100	/* renewing tree or hash table		*/
#define DT_REORDER	000200	/* reordering				*/

_BEGIN_EXTERNS_
extern Dict_t*		dtopen _ARG_((Dtdisc_t*, Dtmethod_t));
extern int		dtclose _ARG_((Dict_t*));
extern Dtdisc_t*	dtdisc _ARG_((Dict_t* dict, Dtdisc_t*));
extern Dict_t*		dtview _ARG_((Dict_t*, Dict_t*));
extern Dtmethod_t	dtmethod _ARG_((Dict_t*, Dtmethod_t, int));

extern int		dtwalk _ARG_((Dict_t*, int(*)(Void_t*) ));
extern Dtlink_t*	dtflatten _ARG_((Dict_t*));
extern Dtlink_t*	_dtextract _ARG_((Dict_t*));
extern void		_dtrestore _ARG_((Dict_t*));
extern int		_dtsize _ARG_((Dict_t*));

extern int		dtstat _ARG_((Dict_t*, Dtstat_t*, int));

extern Dtmethod_t	Dthash;
extern Dtmethod_t	Dttree;
_END_EXTERNS_

#define dtsize(d)	((d)->nobj < 0 ? _dtsize(d) : (d)->nobj)
#define dtgetview(d)	((d)->view)

#define dtlink(d,e)	(((Dtlink_t*)(e))->_dtright)
#define dtobj(d,e)	(((d)->type&DT_MAKEF) ? \
				(((struct _dthold_*)(e))->_dobj) : (Void_t*)(e) )

#define dtreset(d,l)	((d)->here = (l), (d)->prev = NIL(Dtlink_t*), (d)->nobj = -1, \
			 (((d)->type&DT_HASH) ? ((d)->flat = (d), _dtrestore(d), 0) : \
			  ((d)->flat = NIL(Dict_t*), 0) ) )
#define dtextract(d)	((((d)->type&DT_HASH) || (d)->flat) ? _dtextract(d) : (d)->here )

#define dtfirst(d)	(*((d)->searchf))((d),NIL(Void_t*),DT_NEXT)
#define dtnext(d,o)	(*((d)->searchf))((d),(Void_t*)(o),DT_NEXT)
#define dtlast(d)	(*((d)->searchf))((d),NIL(Void_t*),DT_PREV)
#define dtprev(d,o)	(*((d)->searchf))((d),(Void_t*)(o),DT_PREV)
#define dtsearch(d,o)	(*((d)->searchf))((d),(Void_t*)(o),DT_SEARCH)
#define dtinsert(d,o)	(*((d)->searchf))((d),(Void_t*)(o),DT_INSERT)
#define dtdelete(d,o)	(*((d)->searchf))((d),(Void_t*)(o),DT_DELETE)

/* A decent linear congruential hash */
#define dtcharhash(h,c)	((h) += ((h)<<7) + (c) + 987654321L )
#define dtstrhash(h,str,n) \
	{	register unsigned char *_dhs, *_dhe; register unsigned int _dhc; \
		(h) = 0L; _dhs = (unsigned char*)(str); \
		if((n) < 0) while((_dhc = *_dhs++) )	dtcharhash((h),_dhc); \
		else for(_dhe = _dhs+(n); _dhs < _dhe;)	dtcharhash((h),*_dhs++); \
	}

#endif /* _DICT_H */
