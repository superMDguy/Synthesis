
# line 2 "parser.y"
/*
 * Written by Stephen North.  3/1/93 release.
 * graph parser.
 */

#include	"libgraph.h"

static char		Port[SMALLBUF],*Symbol;
static char		In_decl,In_edge_stmt;
static int		Current_class,Agraph_type;
static Agraph_t		*G;
static Agnode_t		*N;
static Agedge_t		*E;
static objstack_t	*SP;
static Agraph_t		*Gstack[32];
static int			GSP;

static void push_subg(g)
     Agraph_t *g;
{
	G = Gstack[GSP++] = g;
}

static Agraph_t *pop_subg()
{
	Agraph_t		*g;
	if (GSP == 0) {
		fprintf(stderr,"Gstack underflow in graph parser\n"); exit(1);
	}
	g = Gstack[--GSP];
	G = Gstack[GSP - 1];
	return g;
}

static objport_t pop_gobj()
{
	objport_t	rv;
	rv.obj = pop_subg();
	rv.port = NULL;
	return rv;
}

static void begin_graph(name)
     char *name;
{
	Agraph_t		*g;
	g = AG.parsed_g = agopen(name,Agraph_type);
	push_subg(g);
	In_decl = TRUE;
}

static void end_graph()
{
	pop_subg();
}

static Agnode_t *bind_node(name)
     char *name;
{
	Agnode_t	*n = agnode(G,name);
	In_decl = FALSE;
	return n;
}

static void anonsubg()
{
	static int		anon_id = 0;
	char			buf[SMALLBUF];
	Agraph_t			*subg;

	In_decl = FALSE;
	sprintf(buf,"_anonymous_%d",anon_id++);
	subg = agsubg(G,buf);
	push_subg(subg);
}

static int isanonsubg(g)
     Agraph_t *g;
{
	return (strncmp("_anonymous_",g->name,11) == 0);
}

static void begin_edgestmt(objp)
     objport_t objp;
{
	struct objstack_t	*new_sp;

	new_sp = NEW(objstack_t);
	new_sp->link = SP;
	SP = new_sp;
	SP->list = SP->last = NEW(objlist_t);
	SP->list->data  = objp;
	SP->list->link = NULL;
	SP->in_edge_stmt = In_edge_stmt;
	SP->subg = G;
	agpushproto(G);
	In_edge_stmt = TRUE;
}

static void mid_edgestmt(objp)
     objport_t objp;
{
	SP->last->link = NEW(objlist_t);
	SP->last = SP->last->link;
	SP->last->data = objp;
	SP->last->link = NULL;
}

static void end_edgestmt()
{
	objstack_t	*old_SP;
	objlist_t	*tailptr,*headptr,*freeptr;
	Agraph_t		*t_graph,*h_graph;
	Agnode_t	*t_node,*h_node,*t_first,*h_first;
	Agedge_t	*e;
	char		*tport,*hport;

	for (tailptr = SP->list; tailptr->link; tailptr = tailptr->link) {
		headptr = tailptr->link;
		tport = tailptr->data.port;
		hport = headptr->data.port;
		if (TAG_OF(tailptr->data.obj) == TAG_NODE) {
			t_graph = NULL;
			t_first = (Agnode_t*)(tailptr->data.obj);
		}
		else {
			t_graph = (Agraph_t*)(tailptr->data.obj);
			t_first = agfstnode(t_graph);
		}
		if (TAG_OF(headptr->data.obj) == TAG_NODE) {
			h_graph = NULL;
			h_first = (Agnode_t*)(headptr->data.obj);
		}
		else {
			h_graph = (Agraph_t*)(headptr->data.obj);
			h_first = agfstnode(h_graph);
		}

		for (t_node = t_first; t_node; t_node = t_graph ?
		  agnxtnode(t_graph,t_node) : NULL) {
			for (h_node = h_first; h_node; h_node = h_graph ?
			  agnxtnode(h_graph,h_node) : NULL ) {
				e = agedge(G,t_node,h_node);
				if (tport && tport[0]) agxset(e,TAILX,tport);
				if (hport && hport[0]) agxset(e,HEADX,hport);
			}
		}
	}
	tailptr = SP->list; 
	while (tailptr) {
		freeptr = tailptr;
		tailptr = tailptr->link;
		if (TAG_OF(freeptr->data.obj) == TAG_NODE)
		free(freeptr->data.port);
		free(freeptr);
	}
	if (G != SP->subg) abort();
	agpopproto(G);
	In_edge_stmt = SP->in_edge_stmt;
	old_SP = SP;
	SP = SP->link;
	In_decl = FALSE;
	free(old_SP);
	Current_class = TAG_GRAPH;
}

static Agraph_t *parent_of(g)
     Agraph_t *g;
{
	Agraph_t		*rv;
	rv = agusergraph(agfstin(g->meta_node->graph,g->meta_node)->tail);
	return rv;
}

static void attr_set(name, value)
     char *name;
     char *value;
{
	Agsym_t		*ap = NULL;
	char		*defval = "";

	if (In_decl && (G->root == G)) defval = value;
	switch (Current_class) {
		case TAG_NODE:
			ap = agfindattr(G->proto->n,name);
			if (ap == NULL)
				ap = agnodeattr(AG.parsed_g,name,defval);
			agxset(N,ap->index,value);
			break;
		case TAG_EDGE:
			ap = agfindattr(G->proto->e,name);
			if (ap == NULL)
				ap = agedgeattr(AG.parsed_g,name,defval);
			agxset(E,ap->index,value);
			break;
		case 0:		/* default */
		case TAG_GRAPH:
			ap = agfindattr(G,name);
			if (ap == NULL) 
				ap = agraphattr(AG.parsed_g,name,defval);
			agxset(G,ap->index,value);
			break;
	}
}


# line 209 "parser.y"
typedef union
#ifdef __cplusplus
	YYSTYPE
#endif
	{
			int					i;
			char				*str;
			struct objport_t	obj;
			struct Agnode_t		*n;
} YYSTYPE;
# define T_graph 257
# define T_digraph 258
# define T_subgraph 259
# define T_strict 260
# define T_node 261
# define T_edge 262
# define T_edgeop 263
# define T_symbol 264

#include <malloc.h>
#include <memory.h>
#include <values.h>

#ifdef __cplusplus

#ifndef agerror
	void agerror(const char *);
#endif

#ifndef aglex
#ifdef __EXTERN_C__
	extern "C" { int aglex(void); }
#else
	int aglex(void);
#endif
#endif
	int agparse(void);

#endif
#define agclearin agchar = -1
#define agerrok agerrflag = 0
extern int agchar;
extern int agerrflag;
YYSTYPE aglval;
YYSTYPE agval;
typedef int agtabelem;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#if YYMAXDEPTH > 0
int ag_ags[YYMAXDEPTH], *ags = ag_ags;
YYSTYPE ag_agv[YYMAXDEPTH], *agv = ag_agv;
#else	/* user does initial allocation */
int *ags;
YYSTYPE *agv;
#endif
static int agmaxdepth = YYMAXDEPTH;
# define YYERRCODE 256
agtabelem agexca[] ={
-1, 0,
	0, 4,
	-2, 0,
-1, 1,
	0, -1,
	-2, 0,
-1, 19,
	263, 50,
	-2, 31,
-1, 20,
	263, 47,
	-2, 45,
-1, 29,
	61, 20,
	-2, 35,
-1, 65,
	263, 54,
	-2, 53,
-1, 66,
	263, 57,
	-2, 56,
	};
# define YYNPROD 64
# define YYLAST 227
agtabelem agact[]={

    24,    24,    61,    86,    55,    76,    75,    62,    47,     7,
    49,     3,     4,     6,    74,     5,     8,     9,    73,    31,
    39,    11,    70,    38,    48,    22,    53,    44,    45,    50,
    33,    63,    44,    45,    37,    19,    85,    20,    72,    42,
    12,    43,    87,    14,    40,    23,    79,    78,    64,    34,
    69,    36,    35,    82,    41,    18,    17,    32,    16,    15,
    13,    52,    46,    51,    54,    71,    21,    10,     2,     1,
    25,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    56,    57,     0,    59,    58,    66,    68,    65,     0,     0,
     0,     0,     0,     0,    77,     0,     0,    54,    81,    80,
     0,     0,     0,    83,    84,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    26,     0,    30,    30,    27,    28,
     0,    29,    67,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    60 };
agtabelem agpact[]={

  -245,-10000000,  -255,-10000000,-10000000,  -241,-10000000,-10000000,-10000000,-10000000,
  -102,  -123,  -106,  -123,-10000000,   -29,-10000000,-10000000,-10000000,-10000000,
-10000000,   -68,-10000000,  -103,-10000000,   -31,-10000000,-10000000,-10000000,-10000000,
  -256,-10000000,-10000000,-10000000,  -253,-10000000,  -253,-10000000,  -260,  -123,
  -123,-10000000,   -36,   -26,   -38,  -257,   -30,-10000000,-10000000,  -122,
-10000000,   -68,-10000000,   -71,    -6,-10000000,  -107,  -111,-10000000,-10000000,
-10000000,  -258,-10000000,  -259,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,  -260,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,  -253,  -253,
-10000000,-10000000,    -8,-10000000,-10000000,  -261,     1,-10000000 };
agtabelem agpgo[]={

     0,    70,    37,    35,    69,    68,    67,    40,    66,    26,
    25,    65,    34,    63,    29,    62,    60,    43,    59,    58,
    56,    55,    54,    39,    41,    53,    52,    51,    24,    50,
    49,    48,    47,    46,    45,    44 };
agtabelem agr1[]={

     0,     6,     4,     4,     4,     5,     5,     5,     5,     8,
     8,     8,     9,     9,    11,    11,    12,    13,    13,    14,
    15,    10,     7,     7,    16,    16,    17,    17,    18,    18,
    18,    18,    21,    21,     2,     1,    22,    22,    22,    22,
    22,    23,    25,    23,    24,    26,    19,    27,    29,    20,
    30,    31,    20,    28,    32,    28,    28,    33,    28,     3,
    35,     3,     3,    34 };
agtabelem agr2[]={

     0,     1,    13,     3,     1,     3,     5,     3,     5,     3,
     3,     3,     6,     0,     0,     2,     6,     4,     0,     2,
     1,     9,     2,     0,     2,     4,     2,     4,     2,     2,
     2,     2,     5,     3,     5,     3,     0,     2,     2,     4,
     4,     5,     1,    15,     5,     1,     7,     1,     1,    11,
     1,     1,    11,     5,     1,     8,     5,     1,     8,     9,
     1,     9,     3,     5 };
agtabelem agchk[]={

-10000000,    -4,    -5,   256,   257,   260,   258,   264,   257,   258,
    -6,   123,    -7,   -16,   -17,   -18,   -19,   -20,   -21,    -3,
    -2,    -8,   -10,   -34,   123,    -1,   257,   261,   262,   264,
   259,   125,   -17,    59,   -30,   -26,   -27,   -12,    91,   123,
   -35,   -22,   -23,   -24,    58,    64,   -15,   264,   -28,   263,
   -14,   -13,   -28,    -9,   -10,   264,    -7,    -7,   -24,   -23,
   264,    40,   264,    61,   -31,    -2,    -3,   264,   -12,   -29,
    93,   -11,    44,   125,   125,   264,   264,   -14,   -32,   -33,
   -14,    -9,   -25,   -28,   -28,    44,   264,    41 };
agtabelem agdef[]={

    -2,    -2,     0,     3,     5,     0,     7,     1,     6,     8,
     0,    23,     0,    22,    24,    26,    28,    29,    30,    -2,
    -2,     0,    33,    62,    60,    36,     9,    10,    11,    -2,
     0,     2,    25,    27,     0,    18,     0,    32,    13,    23,
    23,    34,    37,    38,     0,     0,     0,    63,    51,     0,
    46,    19,    48,     0,    14,    20,     0,     0,    40,    39,
    41,     0,    44,     0,    18,    -2,    -2,    35,    17,    18,
    16,    13,    15,    59,    61,    42,    21,    52,     0,     0,
    49,    12,     0,    55,    58,     0,     0,    43 };
typedef struct
#ifdef __cplusplus
	agtoktype
#endif
{ char *t_name; int t_val; } agtoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

agtoktype agtoks[] =
{
	"T_graph",	257,
	"T_digraph",	258,
	"T_subgraph",	259,
	"T_strict",	260,
	"T_node",	261,
	"T_edge",	262,
	"T_edgeop",	263,
	"T_symbol",	264,
	"{",	123,
	"-unknown-",	-1	/* ends search */
};

char * agreds[] =
{
	"-no such reduction-",
	"file : graph_type T_symbol",
	"file : graph_type T_symbol '{' stmt_list '}'",
	"file : error",
	"file : /* empty */",
	"graph_type : T_graph",
	"graph_type : T_strict T_graph",
	"graph_type : T_digraph",
	"graph_type : T_strict T_digraph",
	"attr_class : T_graph",
	"attr_class : T_node",
	"attr_class : T_edge",
	"inside_attr_list : attr_set optcomma inside_attr_list",
	"inside_attr_list : /* empty */",
	"optcomma : /* empty */",
	"optcomma : ','",
	"attr_list : '[' inside_attr_list ']'",
	"rec_attr_list : rec_attr_list attr_list",
	"rec_attr_list : /* empty */",
	"opt_attr_list : rec_attr_list",
	"attr_set : T_symbol",
	"attr_set : T_symbol '=' T_symbol",
	"stmt_list : stmt_list1",
	"stmt_list : /* empty */",
	"stmt_list1 : stmt",
	"stmt_list1 : stmt_list1 stmt",
	"stmt : stmt1",
	"stmt : stmt1 ';'",
	"stmt1 : node_stmt",
	"stmt1 : edge_stmt",
	"stmt1 : attr_stmt",
	"stmt1 : subg_stmt",
	"attr_stmt : attr_class attr_list",
	"attr_stmt : attr_set",
	"node_id : node_name node_port",
	"node_name : T_symbol",
	"node_port : /* empty */",
	"node_port : port_location",
	"node_port : port_angle",
	"node_port : port_angle port_location",
	"node_port : port_location port_angle",
	"port_location : ':' T_symbol",
	"port_location : ':' '(' T_symbol",
	"port_location : ':' '(' T_symbol ',' T_symbol ')'",
	"port_angle : '@' T_symbol",
	"node_stmt : node_id",
	"node_stmt : node_id opt_attr_list",
	"edge_stmt : node_id",
	"edge_stmt : node_id edgeRHS",
	"edge_stmt : node_id edgeRHS opt_attr_list",
	"edge_stmt : subg_stmt",
	"edge_stmt : subg_stmt edgeRHS",
	"edge_stmt : subg_stmt edgeRHS opt_attr_list",
	"edgeRHS : T_edgeop node_id",
	"edgeRHS : T_edgeop node_id",
	"edgeRHS : T_edgeop node_id edgeRHS",
	"edgeRHS : T_edgeop subg_stmt",
	"edgeRHS : T_edgeop subg_stmt",
	"edgeRHS : T_edgeop subg_stmt edgeRHS",
	"subg_stmt : subg_hdr '{' stmt_list '}'",
	"subg_stmt : '{'",
	"subg_stmt : '{' stmt_list '}'",
	"subg_stmt : subg_hdr",
	"subg_hdr : T_subgraph T_symbol",
};
#endif /* YYDEBUG */
#if !defined(lint) && !defined(__cplusplus)
static  char __yaccpar_sccsid1[] = "@(#) 9/3/92 yaccpar 6.11 Copyr 1991 Sun Micro";
#endif

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto agerrlab
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( agchar >= 0 || ( agr2[ agtmp ] >> 1 ) != 1 )\
	{\
		agerror( "syntax error - cannot backup" );\
		goto agerrlab;\
	}\
	agchar = newtoken;\
	agstate = *agps;\
	aglval = newvalue;\
	goto agnewstate;\
}
#define YYRECOVERING()	(!!agerrflag)
#define YYNEW(type)	malloc(sizeof(type) * agnewmax)
#define YYCOPY(to, from, type) \
	(type *) memcpy(to, (char *) from, agnewmax * sizeof(type))
#define YYENLARGE( from, type) \
	(type *) realloc((char *) from, agnewmax * sizeof(type))
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int agdebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-10000000)

/*
** global variables used by the parser
*/
YYSTYPE *agpv;			/* top of value stack */
int *agps;			/* top of state stack */

int agstate;			/* current state */
int agtmp;			/* extra var (lasts between blocks) */

int agnerrs;			/* number of errors */
int agerrflag;			/* error recovery flag */
int agchar;			/* current input token number */



#ifdef YYNMBCHARS
#define YYLEX()		agcvtok(aglex())
/*
** agcvtok - return a token if i is a wchar_t value that exceeds 255.
**	If i<255, i itself is the token.  If i>255 but the neither 
**	of the 30th or 31st bit is on, i is already a token.
*/
#if defined(__STDC__) || defined(__cplusplus)
int agcvtok(int i)
#else
int agcvtok(i) int i;
#endif
{
	int first = 0;
	int last = YYNMBCHARS - 1;
	int mid;
	wchar_t j;

	if(i&0x60000000){/*Must convert to a token. */
		if( agmbchars[last].character < i ){
			return i;/*Giving up*/
		}
		while ((last>=first)&&(first>=0)) {/*Binary search loop*/
			mid = (first+last)/2;
			j = agmbchars[mid].character;
			if( j==i ){/*Found*/ 
				return agmbchars[mid].tvalue;
			}else if( j<i ){
				first = mid + 1;
			}else{
				last = mid -1;
			}
		}
		/*No entry in the table.*/
		return i;/* Giving up.*/
	}else{/* i is already a token. */
		return i;
	}
}
#else/*!YYNMBCHARS*/
#define YYLEX()		aglex()
#endif/*!YYNMBCHARS*/

/*
** agparse - return 0 if worked, 1 if syntax error not recovered from
*/
#if defined(__STDC__) || defined(__cplusplus)
int agparse(void)
#else
int agparse()
#endif
{
	register YYSTYPE *agpvt;	/* top of value stack for $vars */

#if defined(__cplusplus) || defined(lint)
/*
	hacks to please C++ and lint - goto's inside switch should never be
	executed; agpvt is set to 0 to avoid "used before set" warning.
*/
	static int __yaccpar_lint_hack__ = 0;
	switch (__yaccpar_lint_hack__)
	{
		case 1: goto agerrlab;
		case 2: goto agnewstate;
	}
	agpvt = 0;
#endif

	/*
	** Initialize externals - agparse may be called more than once
	*/
	agpv = &agv[-1];
	agps = &ags[-1];
	agstate = 0;
	agtmp = 0;
	agnerrs = 0;
	agerrflag = 0;
	agchar = -1;

#if YYMAXDEPTH <= 0
	if (agmaxdepth <= 0)
	{
		if ((agmaxdepth = YYEXPAND(0)) <= 0)
		{
			agerror("yacc initialization error");
			YYABORT;
		}
	}
#endif

	{
		register YYSTYPE *ag_pv;	/* top of value stack */
		register int *ag_ps;		/* top of state stack */
		register int ag_state;		/* current state */
		register int  ag_n;		/* internal state number info */
	goto agstack;	/* moved from 6 lines above to here to please C++ */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	agnewstate:
		ag_pv = agpv;
		ag_ps = agps;
		ag_state = agstate;
		goto ag_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	agstack:
		ag_pv = agpv;
		ag_ps = agps;
		ag_state = agstate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	ag_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( agdebug )
		{
			register int ag_i;

			printf( "State %d, token ", ag_state );
			if ( agchar == 0 )
				printf( "end-of-file\n" );
			else if ( agchar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( ag_i = 0; agtoks[ag_i].t_val >= 0;
					ag_i++ )
				{
					if ( agtoks[ag_i].t_val == agchar )
						break;
				}
				printf( "%s\n", agtoks[ag_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++ag_ps >= &ags[ agmaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int agps_index = (ag_ps - ags);
			int agpv_index = (ag_pv - agv);
			int agpvt_index = (agpvt - agv);
			int agnewmax;
#ifdef YYEXPAND
			agnewmax = YYEXPAND(agmaxdepth);
#else
			agnewmax = 2 * agmaxdepth;	/* double table size */
			if (agmaxdepth == YYMAXDEPTH)	/* first time growth */
			{
				char *newags = (char *)YYNEW(int);
				char *newagv = (char *)YYNEW(YYSTYPE);
				if (newags != 0 && newagv != 0)
				{
					ags = YYCOPY(newags, ags, int);
					agv = YYCOPY(newagv, agv, YYSTYPE);
				}
				else
					agnewmax = 0;	/* failed */
			}
			else				/* not first time */
			{
				ags = YYENLARGE(ags, int);
				agv = YYENLARGE(agv, YYSTYPE);
				if (ags == 0 || agv == 0)
					agnewmax = 0;	/* failed */
			}
#endif
			if (agnewmax <= agmaxdepth)	/* tables not expanded */
			{
				agerror( "yacc stack overflow" );
				YYABORT;
			}
			agmaxdepth = agnewmax;

			ag_ps = ags + agps_index;
			ag_pv = agv + agpv_index;
			agpvt = agv + agpvt_index;
		}
		*ag_ps = ag_state;
		*++ag_pv = agval;

		/*
		** we have a new state - find out what to do
		*/
	ag_newstate:
		if ( ( ag_n = agpact[ ag_state ] ) <= YYFLAG )
			goto agdefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		agtmp = agchar < 0;
#endif
		if ( ( agchar < 0 ) && ( ( agchar = YYLEX() ) < 0 ) )
			agchar = 0;		/* reached EOF */
#if YYDEBUG
		if ( agdebug && agtmp )
		{
			register int ag_i;

			printf( "Received token " );
			if ( agchar == 0 )
				printf( "end-of-file\n" );
			else if ( agchar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( ag_i = 0; agtoks[ag_i].t_val >= 0;
					ag_i++ )
				{
					if ( agtoks[ag_i].t_val == agchar )
						break;
				}
				printf( "%s\n", agtoks[ag_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( ag_n += agchar ) < 0 ) || ( ag_n >= YYLAST ) )
			goto agdefault;
		if ( agchk[ ag_n = agact[ ag_n ] ] == agchar )	/*valid shift*/
		{
			agchar = -1;
			agval = aglval;
			ag_state = ag_n;
			if ( agerrflag > 0 )
				agerrflag--;
			goto ag_stack;
		}

	agdefault:
		if ( ( ag_n = agdef[ ag_state ] ) == -2 )
		{
#if YYDEBUG
			agtmp = agchar < 0;
#endif
			if ( ( agchar < 0 ) && ( ( agchar = YYLEX() ) < 0 ) )
				agchar = 0;		/* reached EOF */
#if YYDEBUG
			if ( agdebug && agtmp )
			{
				register int ag_i;

				printf( "Received token " );
				if ( agchar == 0 )
					printf( "end-of-file\n" );
				else if ( agchar < 0 )
					printf( "-none-\n" );
				else
				{
					for ( ag_i = 0;
						agtoks[ag_i].t_val >= 0;
						ag_i++ )
					{
						if ( agtoks[ag_i].t_val
							== agchar )
						{
							break;
						}
					}
					printf( "%s\n", agtoks[ag_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *agxi = agexca;

				while ( ( *agxi != -1 ) ||
					( agxi[1] != ag_state ) )
				{
					agxi += 2;
				}
				while ( ( *(agxi += 2) >= 0 ) &&
					( *agxi != agchar ) )
					;
				if ( ( ag_n = agxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( ag_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( agerrflag )
			{
			case 0:		/* new error */
				agerror( "syntax error" );
				goto skip_init;
			agerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				ag_pv = agpv;
				ag_ps = agps;
				ag_state = agstate;
			skip_init:
				agnerrs++;
				/* FALLTHRU */
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				agerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( ag_ps >= ags )
				{
					ag_n = agpact[ *ag_ps ] + YYERRCODE;
					if ( ag_n >= 0 && ag_n < YYLAST &&
						agchk[agact[ag_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						ag_state = agact[ ag_n ];
						goto ag_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( agdebug )
						printf( _POP_, *ag_ps,
							ag_ps[-1] );
#	undef _POP_
#endif
					ag_ps--;
					ag_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( agdebug )
				{
					register int ag_i;

					printf( "Error recovery discards " );
					if ( agchar == 0 )
						printf( "token end-of-file\n" );
					else if ( agchar < 0 )
						printf( "token -none-\n" );
					else
					{
						for ( ag_i = 0;
							agtoks[ag_i].t_val >= 0;
							ag_i++ )
						{
							if ( agtoks[ag_i].t_val
								== agchar )
							{
								break;
							}
						}
						printf( "token %s\n",
							agtoks[ag_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( agchar == 0 )	/* reached EOF. quit */
					YYABORT;
				agchar = -1;
				goto ag_newstate;
			}
		}/* end if ( ag_n == 0 ) */
		/*
		** reduction by production ag_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( agdebug )
			printf( "Reduce by (%d) \"%s\"\n",
				ag_n, agreds[ ag_n ] );
#endif
		agtmp = ag_n;			/* value to switch over */
		agpvt = ag_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using ag_state here as temporary
		** register variable, but why not, if it works...
		** If agr2[ ag_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto ag_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int ag_len = agr2[ ag_n ];

			if ( !( ag_len & 01 ) )
			{
				ag_len >>= 1;
				agval = ( ag_pv -= ag_len )[1];	/* $$ = $1 */
				ag_state = agpgo[ ag_n = agr1[ ag_n ] ] +
					*( ag_ps -= ag_len ) + 1;
				if ( ag_state >= YYLAST ||
					agchk[ ag_state =
					agact[ ag_state ] ] != -ag_n )
				{
					ag_state = agact[ agpgo[ ag_n ] ];
				}
				goto ag_stack;
			}
			ag_len >>= 1;
			agval = ( ag_pv -= ag_len )[1];	/* $$ = $1 */
			ag_state = agpgo[ ag_n = agr1[ ag_n ] ] +
				*( ag_ps -= ag_len ) + 1;
			if ( ag_state >= YYLAST ||
				agchk[ ag_state = agact[ ag_state ] ] != -ag_n )
			{
				ag_state = agact[ agpgo[ ag_n ] ];
			}
		}
					/* save until reenter driver code */
		agstate = ag_state;
		agps = ag_ps;
		agpv = ag_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( agtmp )
	{
		
case 1:
# line 226 "parser.y"
{begin_graph(agpvt[-0].str);} break;
case 2:
# line 228 "parser.y"
{AG.accepting_state = TRUE; end_graph();} break;
case 3:
# line 230 "parser.y"
{
					if (AG.parsed_g)
						agclose(AG.parsed_g);
					AG.parsed_g = NULL;
					/*exit(1);*/
				} break;
case 4:
# line 236 "parser.y"
{AG.parsed_g = NULL;} break;
case 5:
# line 241 "parser.y"
{Agraph_type = AGRAPH; AG.edge_op = "--";} break;
case 6:
# line 243 "parser.y"
{Agraph_type = AGRAPHSTRICT; AG.edge_op = "--";} break;
case 7:
# line 245 "parser.y"
{Agraph_type = AGDIGRAPH; AG.edge_op = "->";} break;
case 8:
# line 247 "parser.y"
{Agraph_type = AGDIGRAPHSTRICT; AG.edge_op = "->";} break;
case 9:
# line 251 "parser.y"
{Current_class = TAG_GRAPH;} break;
case 10:
# line 253 "parser.y"
{Current_class = TAG_NODE; N = G->proto->n;} break;
case 11:
# line 255 "parser.y"
{Current_class = TAG_EDGE; E = G->proto->e;} break;
case 20:
# line 274 "parser.y"
{Symbol = strdup(agpvt[-0].str);} break;
case 21:
# line 275 "parser.y"
{attr_set(Symbol,agpvt[-0].str); free(Symbol);} break;
case 32:
# line 297 "parser.y"
{Current_class = TAG_GRAPH; /* reset */} break;
case 33:
# line 299 "parser.y"
{Current_class = TAG_GRAPH;} break;
case 34:
# line 303 "parser.y"
{
					objport_t		rv;
					rv.obj = agpvt[-1].n;
					rv.port = strdup(Port);
					Port[0] = '\0';
					agval.obj = rv;
				} break;
case 35:
# line 312 "parser.y"
{agval.n = bind_node(agpvt[-0].str);} break;
case 41:
# line 322 "parser.y"
{strcat(Port,":"); strcat(Port,agpvt[-0].str);} break;
case 42:
# line 323 "parser.y"
{Symbol = strdup(agpvt[-0].str);} break;
case 43:
# line 324 "parser.y"
{	char buf[SMALLBUF];
					sprintf(buf,":(%s,%s)",Symbol,agpvt[-1].str);
					strcat(Port,buf); free(Symbol);
				} break;
case 44:
# line 331 "parser.y"
{	char buf[SMALLBUF];
					sprintf(buf,"@%s",agpvt[-0].str);
					strcat(Port,buf);
				} break;
case 45:
# line 338 "parser.y"
{Current_class = TAG_NODE; N = (Agnode_t*)(agpvt[-0].obj.obj);} break;
case 46:
# line 340 "parser.y"
{Current_class = TAG_GRAPH; /* reset */} break;
case 47:
# line 344 "parser.y"
{begin_edgestmt(agpvt[-0].obj);} break;
case 48:
# line 346 "parser.y"
{ E = SP->subg->proto->e;
				  Current_class = TAG_EDGE; } break;
case 49:
# line 349 "parser.y"
{end_edgestmt();} break;
case 50:
# line 351 "parser.y"
{begin_edgestmt(agpvt[-0].obj);} break;
case 51:
# line 353 "parser.y"
{ E = SP->subg->proto->e;
				  Current_class = TAG_EDGE; } break;
case 52:
# line 356 "parser.y"
{end_edgestmt();} break;
case 53:
# line 359 "parser.y"
{mid_edgestmt(agpvt[-0].obj);} break;
case 54:
# line 361 "parser.y"
{mid_edgestmt(agpvt[-0].obj);} break;
case 56:
# line 364 "parser.y"
{mid_edgestmt(agpvt[-0].obj);} break;
case 57:
# line 366 "parser.y"
{mid_edgestmt(agpvt[-0].obj);} break;
case 59:
# line 371 "parser.y"
{agval.obj = pop_gobj();} break;
case 60:
# line 372 "parser.y"
{ anonsubg(); } break;
case 61:
# line 372 "parser.y"
{agval.obj = pop_gobj();} break;
case 62:
# line 373 "parser.y"
{agval.obj = pop_gobj();} break;
case 63:
# line 377 "parser.y"
{ Agraph_t	 *subg;
				if (subg = agfindsubg(AG.parsed_g,agpvt[-0].str))
				aginsert(G,subg);
				else subg = agsubg(G,agpvt[-0].str); 
				push_subg(subg);
				In_decl = FALSE;
				} break;
	}
	goto agstack;		/* reset registers in driver code */
}

