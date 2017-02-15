/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#include "common.h"
#include "code.h"
#include "mem.h"

Code_t *cbufp;
long cbufn, cbufi;
#define CBUFINCR 1000
#define CBUFSIZE sizeof (Code_t)

static int Cstringoffset;

void Cinit (void) {
    Code_t c;

    cbufp = Marrayalloc ((long) CBUFINCR * CBUFSIZE);
    cbufn = CBUFINCR;
    cbufi = 0;
    Cstringoffset = (char *) &c.u.s[0] - (char *) &c + 1;
    /* the + 1 above accounts for the null character */
}

void Cterm (void) {
    Marrayfree (cbufp);
    cbufp = NULL;
    cbufn = cbufi = 0;
}

void Creset (void) {
    cbufi = 0;
}

long Cnew (Ctype_t ctype) {
    long i;

    if (cbufi >= cbufn) {
        cbufp = Marraygrow (cbufp, (long) (cbufn + CBUFINCR) * CBUFSIZE);
        cbufn += CBUFINCR;
    }
    i = cbufi++;
    cbufp[i].ctype = ctype;
    cbufp[i].next = C_NULL;
    return i;
}

long Cinteger (long i) {
    long j;

    if (cbufi >= cbufn) {
        cbufp = Marraygrow (cbufp, (long) (cbufn + CBUFINCR) * CBUFSIZE);
        cbufn += CBUFINCR;
    }
    j = cbufi++;
    cbufp[j].ctype = C_INTEGER;
    cbufp[j].u.i = i;
    cbufp[j].next = C_NULL;
    return j;
}

long Creal (double d) {
    long i;

    if (cbufi >= cbufn) {
        cbufp = Marraygrow (cbufp, (long) (cbufn + CBUFINCR) * CBUFSIZE);
        cbufn += CBUFINCR;
    }
    i = cbufi++;
    cbufp[i].ctype = C_REAL;
    cbufp[i].u.d = d;
    cbufp[i].next = C_NULL;
    return i;
}

long Cstring (char *s) {
    int size, incr;
    long i;

    size = (strlen (s) + Cstringoffset + CBUFSIZE - 1) / CBUFSIZE;
    if (cbufi + size > cbufn) {
        incr = size > CBUFINCR ? size : CBUFINCR;
        cbufp = Marraygrow (cbufp, (long) (cbufn + incr) * CBUFSIZE);
        cbufn += incr;
    }
    i = cbufi, cbufi += size;
    cbufp[i].ctype = C_STRING;
    strcpy ((char *) &cbufp[i].u.s[0], s);
    cbufp[i].next = C_NULL;
    return i;
}
