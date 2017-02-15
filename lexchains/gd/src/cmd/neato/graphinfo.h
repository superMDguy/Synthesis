/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

typedef struct graphinfo_t {

		/* to place nodes */
	node_t				**nlist;
	int					move;
	double				**dist,**spring,**sum_t,***t;

		/* to generate code */
	layout_t			*drawing;
    textlabel_t        	*label;         /* if the cluster has a title */
	box					bb;				/* bounding box */

		/* not used at present, safe if set to zero */
	int					n_cluster;
	graph_t				**clust;
	boolean				left_to_right;
} graphinfo_t;

typedef struct nodeinfo_t {
	int					id,heapindex,hops;
	double				pos[NDIM],dist;
	boolean				pinned;
	char				state;
	float				width,height;
	short				xsize,ysize;
	label_t				*label;
	shape_desc			*shape;
	void				*shape_instance;
} nodeinfo_t;

typedef struct edgeinfo_t {
	float				factor;
	float				dist;
	splines				*spl;
	label_t				*label;

	port_t				tail_port,head_port;	/* not used */
} edgeinfo_t;
