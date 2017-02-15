/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

typedef unsigned char boolean;

typedef struct Agraph_t graph_t;
typedef struct Agnode_t node_t;
typedef struct Agedge_t edge_t;
typedef struct Agsym_t attrsym_t;

typedef struct pointf {
	double		x,y;
} pointf;

typedef struct point {
	int			x,y;
} point;

typedef struct box {
	point		LL,UR;
} box;

typedef struct port_t {			/* internal edge endpoint specification */
	point		p;					/* aiming point */
	float		theta;				/* slope in radians */
	boolean	constrained,defined;
	unsigned char order;			/* for mincross */
} port_t;

typedef struct path {			/* internal specification for an edge spline */
	port_t		start,end;
	point		*ulpp, *urpp, *llpp, *lrpp;	/* tangents of near splines */
	int			nbox;				/* number of subdivisions */
	box			*boxes;				/* rectangular regions of subdivision */
	void		*data;
} path;

typedef struct bezier {
	point		*list;
	int			size;
	int			sflag, eflag;
	point		sp, ep;
} bezier;

typedef struct splines {
	bezier		*list;
	int			size;
} splines;

typedef struct polygon_t {		/* mutable shape information for a node */
	int			regular; 			/* true for symmetric shapes */
	int			peripheries;		/* number of periphery lines */
	int			sides;				/* number of sides */
	float		orientation;		/* orientation of shape (+ve degrees) */
	float		distortion;			/* distortion factor - as in trapezium */
	float		skew;				/* skew factor - as in parallelogram */
	int			option;				/* ROUNDED, DIAGONAL corners, etc. */
	pointf		*vertices;		    /* array of vertex points */
} polygon_t;

typedef struct shape_desc {		/* read-only shape descriptor */
	char		*name;				/* as read from graph file */
	int			(*initfn)();		/* initializes shape from node label */
	port_t		(*portfn)();		/* finds aiming point and slope of port */
	int			(*insidefn)();		/* clips incident edge spline */
	int			(*pboxfn)();		/* finds box path to reach port */
	int			(*codefn)();		/* emits graphics code for node */
	polygon_t	*polygon;			/* base polygon info */
} shape_desc;

typedef struct codegen_t {
	int     (*reset)();
	int		(*begin_job)(),(*end_job)();
	int		(*begin_graph)(),(*end_graph)();
	int		(*begin_page)(),(*end_page)();
	int		(*begin_node)(),(*end_node)();
	int		(*begin_edge)(),(*end_edge)();
	int		(*begin_context)(),(*end_context)();
	int		(*set_font)(),(*textline)();
	int		(*set_color)(),(*set_style)();
	int		(*ellipse)(),(*polygon)();
	int		(*beziercurve)(),(*polyline)();
	int		(*arrowhead)(),(*user_shape)();
	int		(*comment)();
} codegen_t;

typedef struct queue {
	node_t	**store,**limit,**head,**tail;
} queue;

typedef struct adjmatrix_t {
	int		nrows,ncols;
	char	*data;
} adjmatrix_t;

typedef struct rank_t {
	int				n;			/* number of nodes in this rank			*/
	node_t			**v;		/* ordered list of nodes in rank		*/
	int				an;			/* globally allocated number of nodes	*/
	node_t			**av;		/* allocated list of nodes in rank		*/
	int 			ht2;		/* height above the centerline			*/
	boolean			candidate;	/* for transpose ()						*/
	boolean			valid;
	int				cache_nc;	/* caches number of crossings			*/
	adjmatrix_t		*flat;
} rank_t;

typedef struct textline_t {
	char			*str;
	short			width;
	char			just;
} textline_t;

typedef struct textlabel_t {
	char			*text,*fontname,*fontcolor;
	float			fontsize;
	pointf			dimen;
	point			p;
	textline_t		*line;
	char			nlines;
} textlabel_t;

typedef struct layout_t {
	float			quantum,scale;
	point			margin, page, size;
	boolean			landscape,centered;
} layout_t;

/* for "record" shapes */
typedef struct field_t {
	point		size;			/* its dimension */
	box			b;				/* its final placement */
	int			n_flds;	
	textlabel_t	*lp;			/* n_flds == 0 */
	struct field_t	**fld;		/* n_flds > 0 */
	int		LR;					/* if box list is horizontal (left to right) */
	char	*id;				/* user's identifier */
} field_t;

typedef struct hsbcolor_t {
	char			*name;
	unsigned char	h,s,b;
} hsbcolor_t;

typedef struct Agraphinfo_t {

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
	boolean				left_to_right,has_edge_labels;
} Agraphinfo_t;

typedef struct Agnodeinfo_t {
	int					id,heapindex,hops;
	double				pos[NDIM],dist;
	boolean				pinned;
	char				state;
	float				width,height;
	int					ht,lw,rw;
	point				coord;
	short				xsize,ysize;
	textlabel_t			*label;
	shape_desc			*shape;
	void				*shape_info;
} Agnodeinfo_t;

typedef struct Agedgeinfo_t {
	float				factor;
	float				dist;
	splines				*spl;
	textlabel_t			*label;
	port_t				tail_port,head_port;	/* might be used someday */
	edge_t				*to_orig;				/* for dot's shapes.c    */
	char				edge_type;
} Agedgeinfo_t;
