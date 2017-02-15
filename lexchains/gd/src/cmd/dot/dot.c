/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/*
 * Written by Stephen North and Eleftherios Koutsofios.
 */

#include	"dot.h"
char *Version = "dot version 95 (4-10-95)";
char *Copyright = "Copyright (c) AT&T Corp. 1994, 1995.\n";


main(argc,argv)
int		argc;
char	**argv;
{
	graph_t	*g, *prev;

	prev = NIL(graph_t*);
	initialize(argc,argv);
	while (g = next_input_graph()) {
		if (prev) dot_close(prev);
		dot_init(g);
		dot_rank(g);
		dot_mincross(g);
		dot_position(g);
		dot_splines(g);
		dot_postprocess(g);
		dot_write(g);
		prev = g;
	}
	terminate();
}
