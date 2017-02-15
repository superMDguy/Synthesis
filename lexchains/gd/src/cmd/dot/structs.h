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

typedef struct nlist_t {
	node_t			**list;
	int				size;
} nlist_t;

typedef struct elist {
	edge_t			**list;
	int				size;
} elist;

#define elist_fastapp(item,L)	do {L.list[L.size++] = item; L.list[L.size] = NULL;} while(0)
#define elist_append(item,L)	do {L.list = ALLOC(L.size + 2,L.list,edge_t*); L.list[L.size++] = item; L.list[L.size] = NULL;} while(0)
#define alloc_elist(n,L)		do {L.size = 0; L.list = N_NEW(n + 1,edge_t*); } while (0)
#define free_list(L)			do {if (L.list) free(L.list);} while (0)

typedef struct Agraphinfo_t {
	rank_t				*rank;
	node_t				*nlist;					/* fast graph node list */
	nlist_t				comp;					/* connected components */
	node_t				*minset,*maxset;		/* set leaders */
	long				n_nodes;				/* includes virtual */
	short				minrank,maxrank;

	/* various flags */
	boolean				has_flat_edges;
	boolean				has_edge_labels;
	boolean				left_to_right;
	boolean				showboxes;

	/* to draw graphs or clusters */
	layout_t			*drawing;
    textlabel_t       	*label;         /* if graph or cluster has a title */
	int					nodesep,ranksep;
	box					bb;				/* bounding box */
    node_t		 		*ln,*rn;        /* left, right nodes of bounding box */

	/* for parents of clusters */
	int					n_cluster;
	graph_t				**clust;

	/* for clusters */
	node_t				*leader,**rankleader;
	boolean				expanded;
	char				installed;
	char				set_type;
	boolean				exact_ranksep;
} Agraphinfo_t;

typedef struct Agnodeinfo_t {
		/* fast graph */
	char				node_type,state,mark,onstack;
	char				ranktype,weight_class;
	node_t				*next,*prev;
	elist				in,out,flat_out,flat_in,other;
	graph_t				*clust;

		/* for union-find and collapsing nodes */
	int					UF_size;
	node_t				*UF_parent;
	node_t				*inleaf,*outleaf;

		/* for placing nodes */
	int					rank,order;	/* initially, order = 1 for ordered edges */
	int					ht,lw,rw;
	point				coord;
	int					mval;
	elist				save_in,save_out;

		/* for network-simplex */
	elist				tree_in,tree_out;
	edge_t				*par;
	int					low,lim;
	int					priority;

		/* for drawing nodes */
	float				width,height;
	textlabel_t			*label;
	shape_desc			*shape;
	void				*shape_info;
	boolean				showboxes,has_port;
} Agnodeinfo_t;

typedef struct Agedgeinfo_t {
	char				edge_type;
	boolean				showboxes;
	short				xpenalty;
	int					weight;
	int					cutvalue,tree_index;
	short				count,minlen;

	port_t				tail_port,head_port;
	splines				*spl;
	edge_t				*to_virt,*to_orig;
	textlabel_t			*label;
} Agedgeinfo_t;
