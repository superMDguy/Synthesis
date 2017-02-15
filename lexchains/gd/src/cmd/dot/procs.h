/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

double		atof(),hypot();
int			round();
char		*strdup();
char		*new_string();
int			strccnt();
char		*username();
char		**parse_style();
int			maptoken(),mapbool();
int			parse_layers();

queue		*new_queue();
void		free_queue();
void		enqueue();
node_t		*dequeue();

Agnode_t	*UF_union();
Agnode_t	*UF_find();

Agedge_t	*virtual_edge();
Agedge_t	*new_virtual_edge();
Agnode_t	*virtual_node();
Agedge_t	*find_fast_edge(),*find_flat_edge();
Agedge_t	*make_aux_edge();

Agraph_t	*next_input_graph();
shape_desc	*bind_shape();
point		*routesplines();
point		add_points();
point		sub_points();
point		pointof();
point		spline_at_y();
box			boxof(),mkbox();
double		atan2pt();
pointf		cvt2ptf();
point		cvt2pt();

pointf		label_size();
boolean		spline_merge();

double		elapsed_sec();

point		map_point ();
pointf		Bezier ();
char		*colorxlate(),*canoncolor();
point		pageincr();

int			late_int();
float		late_float();
char		*late_string(),*late_nnstring();
textlabel_t	*make_label();
FILE		*file_select();

#ifdef UTS
char		*getpwuid();
#endif
#ifdef OLD_MALLOC
extern char *malloc(),*realloc(),*calloc();
extern int	free();
#endif
extern char *zmalloc(),*zrealloc();
