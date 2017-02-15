/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _INTERNAL_H
#define _INTERNAL_H
typedef struct Ifunc_t {
    char *name;
    int (*func) (int, Tonm_t *);
    int min, max;
} Ifunc_t;

void Iinit (void);
void Iterm (void);
int Igetfunc (char *);

extern Ifunc_t MSNEAR Ifuncs[];
extern int Ifuncn;
#endif /* _INTERNAL_H */
