/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/* patch around broken compilers that don't handle void* correctly */
#if	(UTS || vax || ultrix || alliant)
#define void char
#endif

/* if HP-UX, enable SVR4 headers */
#if (__hpux)
#define _INCLUDE_POSIX_SOURCE
#endif

/* libc substitutes */

#define HAS_STRDUP 1
#if (UTS || vax || ultrix || alliant)
#undef HAS_STRDUP
#endif

#define HAS_HYPOT 1
#if (UTS)		/* hypot of x == y == 0.0 dumps core */
#undef HAS_HYPOT
#endif

#if (__STDC__ || __svr4__ || sun || SGI)
#define HAS_STRCASECMP 1
#endif

#if ((sun || SGI) && !(__STDC__))
#define HAS_SINCOS 1
#endif
