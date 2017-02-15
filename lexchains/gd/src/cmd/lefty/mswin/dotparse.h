/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#ifndef _DOTPARSE_H
#define _DOTPARSE_H

typedef union
#ifdef __cplusplus
	YYSTYPE
#endif
	{
    long i;
    char *s;
    void *o;
} YYSTYPE;
extern YYSTYPE yylval;
# define T_graph 257
# define T_digraph 258
# define T_subgraph 259
# define T_strict 260
# define T_node 261
# define T_edge 262
# define T_edgeop 263
# define T_id 264
#endif /* _DOTPARSE_H */
