/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _MEM_H
#define _MEM_H
typedef struct Mheader_t {
    char type;
    char area;
    short size;
} Mheader_t;
#define M_HEADERSIZE sizeof (Mheader_t)

typedef enum {
    M_GCFULL, M_GCINCR
} Mgctype_t;

typedef enum {
    M_GCOFF, M_GCON
} Mgcstate_t;

#define M_UNITSIZE sizeof (long)

#define M_BYTE2SIZE(l) ((l + M_UNITSIZE - 1) / M_UNITSIZE)
#define Msetsize(p, s) ((Mheader_t *) p)->size = s
#define M_AREAOF(p) ((int) (((Mheader_t *) p)->area))
#define M_TYPEOF(p) ((int) (((Mheader_t *) p)->type))

#define M_MAXTYPES 6

extern int MSNEAR Mhaspointers[];
extern Mgcstate_t Mgcstate;
extern int Mcouldgc;

void Minit (void (*) (void));
void Mterm (void);
void *Mnew (long, int);
void *Mallocate (long);
void Mfree (void *);
void *Marrayalloc (long);
void *Marraygrow (void *, long);
void Marrayfree (void *);
long Mpushmark (void *);
void Mpopmark (long);
void Mresetmark (long, void *);
void Mmkcurr (void *);
void Mdogc (Mgctype_t);
void Mreport (void);
#endif /* _MEM_H */
