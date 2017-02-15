/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _PARSE_H
#define _PARSE_H
typedef struct Psrc_t {
    int flag;
    char *s;
    FILE *fp;
    int tok;
    int lnum;
} Psrc_t;

void Pinit (void);
void Pterm (void);
Tobj Punit (Psrc_t *);
Tobj Pfcall (Tobj, Tobj);
Tobj Pfunction (char *, int);
#endif /* _PARSE_H */
