/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _IODDE_H
#define _IODDE_H
#define IODDE_READ 1
#define IODDE_WRITE 2

#define IODDEBUFSIZE 4096
typedef struct iodde_t {
    HSZ namehsz;  /* name of other process */
    HCONV ih, oh;  /* input and output handles */
    int lastop;  /* IODDE_READ or IODDE_WRITE */
    int enabled; /* whether callback is enabled */
    char buf[IODDEBUFSIZE];
    int bufi, bufj;
} iodde_t;

void IODDEinit (char *);
void IODDEterm (void);
int IODDEopen (iodde_t *, char *, int);
int IODDEclose (iodde_t *);
long IODDEread (iodde_t *, char *, long);
long IODDEreadline (iodde_t *, char *, long);
long IODDEwrite (iodde_t *, char *, long);
long IODDEwriteline (iodde_t *, char *, long);
long IODDEflush (iodde_t *);
long IODDEsuck (iodde_t *);
#endif
