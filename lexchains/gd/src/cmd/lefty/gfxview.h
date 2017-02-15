/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _GFXVIEW_H
#define _GFXVIEW_H
void GFXinit (void);
void GFXterm (void);
void GFXprune (void);
void GFXlabelcb (Gevent_t *);
void GFXviewcb (Gevent_t *);
void GFXevent (Gevent_t *);
void GFXmove (void);
void GFXredraw (void);
void GFXtextcb (int, char *);
void GFXbuttoncb (int, void *);
void GFXmonitorfile (int);
void GFXidle (void);

int GFXcreatewidget (int, lvar_t *);
int GFXsetwidgetattr (int, lvar_t *);
int GFXgetwidgetattr (int, lvar_t *);
int GFXdestroywidget (int, lvar_t *);
int GFXclear (int, lvar_t *);
int GFXsetgfxattr (int, lvar_t *);
int GFXgetgfxattr (int, lvar_t *);
int GFXarrow (int, lvar_t *);
int GFXline (int, lvar_t *);
int GFXbox (int, lvar_t *);
int GFXpolygon (int, lvar_t *);
int GFXsplinegon (int, lvar_t *);
int GFXarc (int, lvar_t *);
int GFXtext (int, lvar_t *);
int GFXtextsize (int, lvar_t *);
int GFXclearpick (int, lvar_t *);
int GFXsetpick (int, lvar_t *);
int GFXdisplaymenu (int, lvar_t *);
#endif /* _GFXVIEW_H */
