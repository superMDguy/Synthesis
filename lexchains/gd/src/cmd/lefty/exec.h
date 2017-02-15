/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _EXEC_H
#define _EXEC_H
typedef struct Tonm_t lvar_t;
#define LVARINCR 1000
#define LVARSIZE sizeof (lvar_t)

extern Tobj root, null;
extern lvar_t *lvarp;
extern long lvarn, llvari, flvari;
extern Tobj rtno;
extern int Erun;
extern int Eerrlevel, Estackdepth, Eshowbody, Eshowcalls, Eoktorun;

void Einit (void);
void Eterm (void);
Tobj Eunit (Tobj);
Tobj Efunction (Tobj, char *);
#endif /* _EXEC_H */
