/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _STR_H
#define _STR_H
void Sinit (void);
void Sterm (void);
char *Spath (char *, Tobj);
char *Sseen (Tobj, char *);
char *Sabstract (Tobj, Tobj);
char *Stfull (Tobj);
char *Ssfull (Tobj, Tobj);
char *Scfull (Tobj, long, long);
#endif /* _STR_H */
