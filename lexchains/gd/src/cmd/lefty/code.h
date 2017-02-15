/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _CODE_H
#define _CODE_H
#define C_NULL -1

#define C_ISSTMT(ct) (ct >= C_STMT && ct <= C_RETURN)

typedef enum {
    C_CODE, C_ASSIGN, C_INTEGER, C_REAL, C_STRING, C_OR, C_AND,
    C_EQ, C_NE, C_LT, C_LE, C_GT, C_GE, C_PLUS, C_MINUS, C_MUL,
    C_DIV, C_MOD, C_UMINUS, C_NOT, C_PEXPR, C_FCALL, C_GVAR, C_LVAR,
    C_PVAR, C_FUNCTION, C_TCONS, C_DECL, C_STMT, C_IF, C_WHILE,
    C_FOR, C_FORIN, C_BREAK, C_CONTINUE, C_RETURN, C_INTERNAL,
    C_ARGS, C_NOP, C_SIZE
} Ctype_t;

typedef struct Code_t {
    Ctype_t ctype;
    long next;
    union {
        char s[1];
        double d;
        long i;
        long fp;
        void *o;
    } u;
} Code_t;
#define C_CODESIZE sizeof (Code_t)

#define Cgetstring(i) (char *) &cbufp[i].u.s[0]
#define Cgetindex() cbufi

#define Csettype(a, b) cbufp[a].ctype = b
#define Csetfp(a, b) cbufp[a].u.fp = b
#define Csetnext(a, b) cbufp[a].next = b
#define Csetinteger(a, b) cbufp[a].u.i = b
#define Csetobject(a, b) cbufp[a].u.o = b
#define Csetreal(a, b) cbufp[a].u.d = b

extern Code_t *cbufp;
extern long cbufn, cbufi;

void Cinit (void);
void Cterm (void);
void Creset (void);
long Cnew (Ctype_t);
long Cinteger (long);
long Creal (double);
long Cstring (char *);
#endif /* _CODE_H */
