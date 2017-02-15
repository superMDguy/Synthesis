/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _TXTVIEW_H
#define _TXTVIEW_H
void TXTinit (Grect_t);
void TXTterm (void);
int TXTmode (int argc, lvar_t *argv);
int TXTask (int argc, lvar_t *argv);
void TXTprocess (int, char *);
void TXTupdate (void);
void TXTtoggle (int, void *);
#endif /* _TXTVIEW_H */
