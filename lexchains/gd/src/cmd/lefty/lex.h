/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _LEX_H
#define _LEX_H
typedef enum {
    L_SEMI, L_ASSIGN, L_OR, L_AND, L_EQ, L_NE, L_LT, L_LE, L_GT, L_GE, L_PLUS,
    L_MINUS, L_MUL, L_DIV, L_MOD, L_NOT, L_STRING, L_NUMBER, L_ID, L_DOT,
    L_LB, L_RB, L_FUNCTION, L_LP, L_RP, L_LCB, L_RCB, L_LOCAL, L_COLON,
    L_COMMA, L_IF, L_ELSE, L_WHILE, L_FOR, L_IN, L_BREAK, L_CONTINUE,
    L_RETURN, L_INTERNAL, L_EOF, L_SIZE
} Ltype_t;

#define MAXTOKEN 1000
typedef char *Lname_t;

extern Ltype_t Ltok;
extern char MSNEAR Lstrtok[];
extern Lname_t MSNEAR Lnames[];

void Lsetsrc (int, char *, FILE *, int, int);
void Lgetsrc (int *, char **, FILE **, int *, int *);
void Lprintpos (void);
void Lgtok (void);
#endif /* _LEX_H */
