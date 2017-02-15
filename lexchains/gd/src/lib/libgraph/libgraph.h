/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/* patch around really broken compilers that don't handle void* correctly */
#if	(UTS || vax || ultrix || alliant)
#define void char
#endif

/* if HP-UX, enable the complete SVR4 headers */
#define _INCLUDE_POSIX_SOURCE

#include <ctype.h>
#include <string.h>
#include <unistd.h>

#ifndef EXTERN
#define EXTERN extern
#endif
#ifndef uchar
#define uchar	unsigned char
#define ulong	unsigned long
#endif

typedef struct Agraphinfo_t {char notused;} Agraphinfo_t;
typedef struct Agnodeinfo_t {char notused;} Agnodeinfo_t;
typedef struct Agedgeinfo_t {char notused;} Agedgeinfo_t;

#include "graph.h"

#define NOT(v)		(!(v))
#define	FALSE		0
#define TRUE		NOT(FALSE)
#define NEW(t)		 (t*)calloc(1,sizeof(t))
#define N_NEW(n,t)	 (t*)calloc((n),sizeof(t))
#define ALLOC(size,ptr,type) (ptr? (type*)realloc(ptr,(size)*sizeof(type)):(type*)malloc((size)*sizeof(type)))
#define MIN(a,b)	((a)<(b)?(a):(b))
#define MAX(a,b)	((a)>(b)?(a):(b))
#define SMALLBUF	128

#define	NOPRINT		0
#define MULTIPLE    1
#define MUSTPRINT	2

#define ISEMPTYSTR(s) 	(((s) == NULL) || (*(s) == '\0'))
#define NULL_FN(t) 		(t(*)())0
#define ZFREE(p)		if (p) free(p);

#define	TAG_NODE			1
#define	TAG_EDGE			2
#define	TAG_GRAPH			3
#define TAG_OF(p)			(((Agraph_t*)(p))->tag)

#define AGFLAG_STRICT		(1<<1)
#define AGFLAG_METAGRAPH	(1<<2)
#define METAGRAPH			(AGFLAG_DIRECTED | AGFLAG_STRICT | AGFLAG_METAGRAPH)

#define	KEYX				0
#define KEY_ID				"key"
#define	TAILX				1
#define TAIL_ID				"tailport"
#define HEADX				2
#define HEAD_ID				"headport"

EXTERN struct {
	int			graph_nbytes,node_nbytes,edge_nbytes;
	Agraph_t	*proto_g,*parsed_g;
	char		*edge_op;
	char		*linebuf;
	short		syntax_errors;
	uchar		accepting_state,init_called;
} AG;

/* follow structs used in graph parser */
typedef struct objport_t {
	void			*obj;
	char			*port;
} objport_t;

typedef struct objlist_t {
	objport_t			data;
	struct objlist_t	*link;
} objlist_t;

typedef struct objstack_t {
	Agraph_t			*subg;
	objlist_t        	*list,*last;
	int			in_edge_stmt;
	struct objstack_t	*link;
} objstack_t;

char		*agstrdup();
void		agstrfree();
char 		*strdup();

Agnode_t	*agNEWnode();
Agedge_t	*agNEWedge();
Agdict_t	*agNEWdict();
Agdict_t	*agdictof();
Agnode_t	*agidnode();
Agsym_t		*agnewsym();
void		agFREEdict();
void		agFREEnode();
void		agFREEedge();

int			agnodecmp(),agfastnode();
int			agcmpin(),agcmpout();
Void_t*		agnomake();

void		agINSnode();
void		agINSedge();
void		agINSgraph();

void		agDELedge();
void		agDELnode();
void		agDELgraph();

void		agerror();

extern Dtdisc_t agNamedisc,agNodedisc,agOutdisc,agIndisc,agEdgedisc;

