/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include <stdio.h>
#include <dict.h>
#include <sys/types.h>
#include <malloc.h>

typedef struct Agraph_t		Agraph_t;
typedef struct Agnode_t		Agnode_t;
typedef struct Agedge_t		Agedge_t;
typedef struct Agdict_t		Agdict_t;
typedef struct Agsym_t		Agsym_t;
typedef struct Agdata_t		Agdata_t;
typedef struct Agproto_t	Agproto_t;

#define AGFLAG_DIRECTED		(1<<0)
#define AGFLAG_STRICT		(1<<1)
#define AGFLAG_METAGRAPH	(1<<2)

#define	AGRAPH				0
#define	AGRAPHSTRICT		(AGRAPH | AGFLAG_STRICT)
#define AGDIGRAPH 			AGFLAG_DIRECTED
#define AGDIGRAPHSTRICT		(AGDIGRAPH | AGFLAG_STRICT)
#define AGMETAGRAPH			(AGFLAG_DIRECTED | AGFLAG_STRICT | AGFLAG_METAGRAPH)

#define AG_IS_DIRECTED(g)	((g)->kind & AGFLAG_DIRECTED)
#define AG_IS_STRICT(g)		((g)->kind & AGFLAG_STRICT)
#define AG_IS_METAGRAPH(g)	((g)->kind & AGFLAG_METAGRAPH)
#define aginit()			aginitlib(sizeof(Agraph_t),sizeof(Agnode_t),sizeof(Agedge_t))

struct Agraph_t {
	int				tag : 4;
	int				kind : 4;
	int				handle : 24;	/* for tcl/tk interface */
	char			**attr;
	char			*name;
	Agdata_t		*univ;
	Dict_t			*nodes,*inedges,*outedges;
	Agraph_t		*root;
	Agnode_t		*meta_node;
	Agproto_t		*proto;
	Agraphinfo_t	u;
};

struct Agnode_t {
	int				tag : 4;
	int				pad : 4;
	int				handle : 24;
	char			**attr;
	char			*name;
	int				id;
	Agraph_t		*graph;
	Agnodeinfo_t	u;
};

struct Agedge_t {
	int				tag : 4;
	int				printkey : 4;
	int				handle : 24;
	char			**attr;
	Agnode_t		*head,*tail;
	int				id;
	Agedgeinfo_t	u;
};

struct Agdata_t {				/* for main graph */
	Dict_t			*node_dict;
	Agdict_t		*nodeattr;
	Agdict_t		*edgeattr;
	Agdict_t		*globattr;
	int				max_node_id, max_edge_id;
};

struct Agsym_t {
	char			*name,*value;
	int				index;
	unsigned char	printed;
};

struct Agdict_t  {
	char			*name;
	Dict_t			*dict;
	Agsym_t			**list;
};

struct Agproto_t {
	Agnode_t		*n;
	Agedge_t		*e;
	Agproto_t		*prev;
};

char		*agstrcanon();
char		*agget();
char		*agxget();
void		agset();
void		agxset();
int			agindex();

Agraph_t	*agopen();
Agraph_t	*agsubg();
Agraph_t	*agfindsubg();
void		agclose();
Agraph_t	*agread();
int			agwrite();
int			agerrors();

Agraph_t	*agusergraph();
Agnode_t	*agmetanode();

Agnode_t	*agnode();
Agnode_t	*agfindnode();
Agnode_t	*agfstnode();
Agnode_t	*agnxtnode();

void		aginsert();
void		agdelete();
int			agcontains();

Agedge_t	*agedge();
Agedge_t	*agfindedge();
Agedge_t	*agfstedge();
Agedge_t	*agnxtedge();
Agedge_t	*agfstin();
Agedge_t 	*agnxtin();
Agedge_t	*agfstout();
Agedge_t	*agnxtout();

Agsym_t*	agraphattr();
Agsym_t*	agnodeattr();
Agsym_t*	agedgeattr();
Agsym_t*	agfindattr();
