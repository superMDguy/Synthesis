/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

pointf		cvt2ptf();
point		cvt2pt();
point		add_points();
point		sub_points();
point		pointof();
point		spline_at_y();
point		coord();
double		atan2pt();
int			round();
char		*strdup();

char		**parse_style();
char		*username();
char		*late_string(),*late_nnstring();
char		*colorxlate(),*canoncolor();
point		pageincr();

graph_t		*input_graph();

char		*late_string(),*late_nnstring();
double		late_float();

shape_desc	*bind_shape();
textlabel_t	*make_label();
pointf		Bezier ();
pointf		label_size();
FILE		*file_select();

#ifdef OLD_MALLOC
extern char	*malloc(),*realloc(),*calloc();
extern int	free();
#endif

extern char	*zmalloc(),*zrealloc();
double drand48();	/* why isn't this in /usr/include ? */
