/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _IO_H
#define _IO_H
typedef enum {
    IO_FILE, IO_PTY, IO_PIPE, IO_SOCKET,
#ifdef HAVECS
    IO_CS,
#endif
    IO_SIZE
} iotype_t;
#if !defined(HAVEMSWIN) || defined(HAVEPOSIX)
typedef struct io_t {
    int inuse;
    iotype_t type;
    FILE *ifp, *ofp;
    int pid;
} io_t;
#else
#include "iodde.h"
typedef struct io_t {
    int inuse;
    iotype_t type;
    union {
        struct {
            FILE *ifp, *ofp;
        } f;
        iodde_t p;
    } u;
} io_t;
#endif
#define IOINCR 5
#define IOSIZE sizeof (io_t)

extern io_t *iop;
extern int ion;

void IOinit (void);
void IOterm (void);
int IOopen (char *, char *, char *, char *);
int IOclose (int, char *);
int IOreadline (int, char *, int);
int IOread (int, char *, int);
int IOwriteline (int, char *);
#endif /* _IO_H */
