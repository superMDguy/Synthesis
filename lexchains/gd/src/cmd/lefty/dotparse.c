
# line 2 "dot2l/dotparse.y"
#include <stdio.h>
typedef void *Tobj;
#include "dot2l.h"

static char portstr[SMALLBUF];

# line 9 "dot2l/dotparse.y"
typedef union
#ifdef __cplusplus
	YYSTYPE
#endif
 {
    long i;
    char *s;
    void *o;
} YYSTYPE;
# define T_graph 257
# define T_digraph 258
# define T_subgraph 259
# define T_strict 260
# define T_node 261
# define T_edge 262
# define T_edgeop 263
# define T_id 264

#include <malloc.h>
#include <memory.h>
#include <values.h>

#ifdef __cplusplus

#ifndef yyerror
	void yyerror(const char *);
#endif

#ifndef yylex
#ifdef __EXTERN_C__
	extern "C" { int yylex(void); }
#else
	int yylex(void);
#endif
#endif
	int yyparse(void);

#endif
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
YYSTYPE yylval;
YYSTYPE yyval;
typedef int yytabelem;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#if YYMAXDEPTH > 0
int yy_yys[YYMAXDEPTH], *yys = yy_yys;
YYSTYPE yy_yyv[YYMAXDEPTH], *yyv = yy_yyv;
#else	/* user does initial allocation */
int *yys;
YYSTYPE *yyv;
#endif
static int yymaxdepth = YYMAXDEPTH;
# define YYERRCODE 256
yytabelem yyexca[] ={
-1, 0,
	0, 4,
	-2, 0,
-1, 1,
	0, -1,
	-2, 0,
-1, 19,
	263, 25,
	-2, 18,
-1, 34,
	263, 22,
	-2, 19,
-1, 61,
	263, 32,
	-2, 31,
-1, 73,
	263, 29,
	-2, 28,
	};
# define YYNPROD 63
# define YYLAST 227
yytabelem yyact[]={

    24,    24,    51,    69,    84,    66,    58,    52,    43,     7,
    45,     3,     4,     6,    71,     5,     8,     9,    70,    30,
    40,    11,    78,    54,    44,    22,    63,    67,    37,    53,
    38,    34,    42,    32,    38,    37,    20,    19,    80,    77,
    87,    35,    14,    12,    41,    36,    23,    79,    64,    39,
    21,    74,    81,    59,    33,    76,    31,    47,    46,    18,
    17,    16,    15,    13,    10,     2,     1,    57,     0,     0,
     0,     0,    65,     0,     0,     0,     0,     0,    49,     0,
    68,    48,    60,    61,    55,    56,    72,     0,     0,     0,
     0,     0,    73,     0,    75,     0,     0,     0,     0,    82,
     0,     0,     0,    83,     0,    68,    86,    85,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    26,     0,    29,    29,    27,    28,
     0,    25,    62,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    50 };
yytabelem yypact[]={

  -245,-10000000,  -255,-10000000,-10000000,  -241,-10000000,-10000000,-10000000,-10000000,
  -102,  -123,  -106,  -123,-10000000,   -26,-10000000,-10000000,-10000000,-10000000,
   -30,-10000000,-10000000,  -103,-10000000,   -29,-10000000,-10000000,-10000000,  -256,
-10000000,-10000000,-10000000,  -253,-10000000,   -34,   -23,   -38,  -257,   -68,
  -123,  -123,  -258,-10000000,-10000000,  -122,-10000000,  -253,-10000000,-10000000,
-10000000,  -259,-10000000,-10000000,  -261,  -107,  -111,-10000000,-10000000,-10000000,
   -30,-10000000,-10000000,-10000000,   -68,-10000000,    -5,   -71,    -6,   -29,
-10000000,-10000000,-10000000,-10000000,  -253,-10000000,-10000000,  -260,-10000000,  -261,
-10000000,  -253,-10000000,-10000000,    -1,-10000000,-10000000,-10000000 };
yytabelem yypgo[]={

     0,    67,    37,    36,    66,    65,    64,    43,    63,    42,
    62,    61,    60,    59,    31,    58,    26,    57,    24,    55,
    54,    53,    52,    51,    41,    45,    50,    49,    29,    25,
    48,    27,    47,    46,    44 };
yytabelem yyr1[]={

     0,     6,     4,     4,     4,     5,     5,     5,     5,     7,
     7,     8,     8,     9,     9,    10,    10,    10,    10,    15,
    11,     3,    17,    19,    12,    20,    21,    12,    18,    22,
    18,    18,    23,    18,    14,    14,    14,    14,    14,    24,
    24,    25,    27,    13,    13,    26,    26,    26,    16,    30,
    30,    28,    31,    31,    29,    32,    32,     1,     2,    34,
     2,     2,    33 };
yytabelem yyr2[]={

     0,     1,    13,     3,     1,     3,     5,     3,     5,     2,
     0,     2,     4,     2,     4,     2,     2,     2,     2,     1,
     9,     3,     1,     1,    13,     1,     1,    11,     7,     1,
    10,     5,     1,     8,     0,     2,     2,     4,     4,     5,
    13,     5,     1,     7,     3,     3,     3,     3,     2,     4,
     0,     6,     6,     0,     7,     0,     2,     2,     9,     1,
     9,     3,     5 };
yytabelem yychk[]={

-10000000,    -4,    -5,   256,   257,   260,   258,   264,   257,   258,
    -6,   123,    -7,    -8,    -9,   -10,   -11,   -12,   -13,    -2,
    -3,   -26,   -29,   -33,   123,   264,   257,   261,   262,   259,
   125,    -9,    59,   -20,   -14,   -24,   -25,    58,    64,   -27,
   123,   -34,    61,   264,   -18,   263,   -15,   -17,   -25,   -24,
   264,    40,   264,   -28,    91,    -7,    -7,    -1,   264,   -21,
    -3,    -2,   264,   -16,   -30,   -18,   264,   -31,   -29,   264,
   125,   125,   -16,   -14,   -23,   -28,   -19,    44,    93,   -32,
    44,   -22,   -18,   -16,   264,   -31,   -18,    41 };
yytabelem yydef[]={

    -2,    -2,     0,     3,     5,     0,     7,     1,     6,     8,
     0,    10,     0,     9,    11,    13,    15,    16,    17,    -2,
    34,    42,    44,    61,    59,    21,    45,    46,    47,     0,
     2,    12,    14,     0,    -2,    35,    36,     0,     0,     0,
    10,    10,     0,    62,    26,     0,    50,     0,    38,    37,
    39,     0,    41,    43,    53,     0,     0,    54,    57,    50,
    34,    -2,    21,    20,    48,    23,     0,     0,    55,     0,
    58,    60,    27,    -2,     0,    49,    50,     0,    51,    53,
    56,     0,    33,    24,     0,    52,    30,    40 };
typedef struct
#ifdef __cplusplus
	yytoktype
#endif
{ char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"T_graph",	257,
	"T_digraph",	258,
	"T_subgraph",	259,
	"T_strict",	260,
	"T_node",	261,
	"T_edge",	262,
	"T_edgeop",	263,
	"T_id",	264,
	"{",	123,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"file : graph_type T_id",
	"file : graph_type T_id '{' stmt_list '}'",
	"file : error",
	"file : /* empty */",
	"graph_type : T_graph",
	"graph_type : T_strict T_graph",
	"graph_type : T_digraph",
	"graph_type : T_strict T_digraph",
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
	"node_stmt : node_id node_port",
	"node_stmt : node_id node_port opt_attr_list",
	"node_id : T_id",
	"edge_stmt : node_id node_port",
	"edge_stmt : node_id node_port edgeRHS",
	"edge_stmt : node_id node_port edgeRHS opt_attr_list",
	"edge_stmt : subg_stmt",
	"edge_stmt : subg_stmt edgeRHS",
	"edge_stmt : subg_stmt edgeRHS opt_attr_list",
	"edgeRHS : T_edgeop node_id node_port",
	"edgeRHS : T_edgeop node_id node_port",
	"edgeRHS : T_edgeop node_id node_port edgeRHS",
	"edgeRHS : T_edgeop subg_stmt",
	"edgeRHS : T_edgeop subg_stmt",
	"edgeRHS : T_edgeop subg_stmt edgeRHS",
	"node_port : /* empty */",
	"node_port : port_location",
	"node_port : port_angle",
	"node_port : port_angle port_location",
	"node_port : port_location port_angle",
	"port_location : ':' T_id",
	"port_location : ':' '(' T_id ',' T_id ')'",
	"port_angle : '@' T_id",
	"attr_stmt : attr_class",
	"attr_stmt : attr_class attr_list",
	"attr_stmt : attr_set",
	"attr_class : T_graph",
	"attr_class : T_node",
	"attr_class : T_edge",
	"opt_attr_list : rec_attr_list",
	"rec_attr_list : rec_attr_list attr_list",
	"rec_attr_list : /* empty */",
	"attr_list : '[' inside_attr_list ']'",
	"inside_attr_list : attr_set optcomma inside_attr_list",
	"inside_attr_list : /* empty */",
	"attr_set : T_id '=' expr",
	"optcomma : /* empty */",
	"optcomma : ','",
	"expr : T_id",
	"subg_stmt : subg_hdr '{' stmt_list '}'",
	"subg_stmt : '{'",
	"subg_stmt : '{' stmt_list '}'",
	"subg_stmt : subg_hdr",
	"subg_hdr : T_subgraph T_id",
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
#define YYERROR		goto yyerrlab
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#define YYNEW(type)	malloc(sizeof(type) * yynewmax)
#define YYCOPY(to, from, type) \
	(type *) memcpy(to, (char *) from, yynewmax * sizeof(type))
#define YYENLARGE( from, type) \
	(type *) realloc((char *) from, yynewmax * sizeof(type))
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-10000000)

/*
** global variables used by the parser
*/
YYSTYPE *yypv;			/* top of value stack */
int *yyps;			/* top of state stack */

int yystate;			/* current state */
int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */
int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */



#ifdef YYNMBCHARS
#define YYLEX()		yycvtok(yylex())
/*
** yycvtok - return a token if i is a wchar_t value that exceeds 255.
**	If i<255, i itself is the token.  If i>255 but the neither 
**	of the 30th or 31st bit is on, i is already a token.
*/
#if defined(__STDC__) || defined(__cplusplus)
int yycvtok(int i)
#else
int yycvtok(i) int i;
#endif
{
	int first = 0;
	int last = YYNMBCHARS - 1;
	int mid;
	wchar_t j;

	if(i&0x60000000){/*Must convert to a token. */
		if( yymbchars[last].character < i ){
			return i;/*Giving up*/
		}
		while ((last>=first)&&(first>=0)) {/*Binary search loop*/
			mid = (first+last)/2;
			j = yymbchars[mid].character;
			if( j==i ){/*Found*/ 
				return yymbchars[mid].tvalue;
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
#define YYLEX()		yylex()
#endif/*!YYNMBCHARS*/

/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
#if defined(__STDC__) || defined(__cplusplus)
int yyparse(void)
#else
int yyparse()
#endif
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */

#if defined(__cplusplus) || defined(lint)
/*
	hacks to please C++ and lint - goto's inside switch should never be
	executed; yypvt is set to 0 to avoid "used before set" warning.
*/
	static int __yaccpar_lint_hack__ = 0;
	switch (__yaccpar_lint_hack__)
	{
		case 1: goto yyerrlab;
		case 2: goto yynewstate;
	}
	yypvt = 0;
#endif

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

#if YYMAXDEPTH <= 0
	if (yymaxdepth <= 0)
	{
		if ((yymaxdepth = YYEXPAND(0)) <= 0)
		{
			yyerror("yacc initialization error");
			YYABORT;
		}
	}
#endif

	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */
	goto yystack;	/* moved from 6 lines above to here to please C++ */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
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
		if ( yydebug )
		{
			register int yy_i;

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			int yynewmax;
#ifdef YYEXPAND
			yynewmax = YYEXPAND(yymaxdepth);
#else
			yynewmax = 2 * yymaxdepth;	/* double table size */
			if (yymaxdepth == YYMAXDEPTH)	/* first time growth */
			{
				char *newyys = (char *)YYNEW(int);
				char *newyyv = (char *)YYNEW(YYSTYPE);
				if (newyys != 0 && newyyv != 0)
				{
					yys = YYCOPY(newyys, yys, int);
					yyv = YYCOPY(newyyv, yyv, YYSTYPE);
				}
				else
					yynewmax = 0;	/* failed */
			}
			else				/* not first time */
			{
				yys = YYENLARGE(yys, int);
				yyv = YYENLARGE(yyv, YYSTYPE);
				if (yys == 0 || yyv == 0)
					yynewmax = 0;	/* failed */
			}
#endif
			if (yynewmax <= yymaxdepth)	/* tables not expanded */
			{
				yyerror( "yacc stack overflow" );
				YYABORT;
			}
			yymaxdepth = yynewmax;

			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
			skip_init:
				yynerrs++;
				/* FALLTHRU */
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
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
				if ( yydebug )
				{
					register int yy_i;

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 1:
# line 26 "dot2l/dotparse.y"
{ D2Lbegin (yypvt[-0].s); free (yypvt[-0].s); } break;
case 2:
# line 28 "dot2l/dotparse.y"
{ D2Lend (); } break;
case 3:
# line 30 "dot2l/dotparse.y"
{ D2Labort (); } break;
case 4:
# line 32 "dot2l/dotparse.y"
{ D2Labort (); } break;
case 5:
# line 36 "dot2l/dotparse.y"
{ gtype = "graph"; etype = "--"; } break;
case 6:
# line 38 "dot2l/dotparse.y"
{ gtype = "strict graph"; etype = "--"; } break;
case 7:
# line 40 "dot2l/dotparse.y"
{ gtype = "digraph"; etype = "->"; } break;
case 8:
# line 42 "dot2l/dotparse.y"
{ gtype = "strict digraph"; etype = "->"; } break;
case 19:
# line 63 "dot2l/dotparse.y"
{ attrclass = NODE; portstr[0] = '\000'; } break;
case 20:
# line 64 "dot2l/dotparse.y"
{ attrclass = GRAPH; } break;
case 21:
# line 68 "dot2l/dotparse.y"
{ yyval.o = D2Linsertnode (yypvt[-0].s); free (yypvt[-0].s); } break;
case 22:
# line 72 "dot2l/dotparse.y"
{ D2Lbeginedge (NODE, yypvt[-1].o, portstr); portstr[0] = '\000'; } break;
case 23:
# line 74 "dot2l/dotparse.y"
{ attrclass = EDGE; } break;
case 24:
# line 76 "dot2l/dotparse.y"
{ D2Lendedge (); attrclass = GRAPH; } break;
case 25:
# line 78 "dot2l/dotparse.y"
{ D2Lbeginedge (GRAPH, yypvt[-0].o, ""); } break;
case 26:
# line 80 "dot2l/dotparse.y"
{ attrclass = EDGE; } break;
case 27:
# line 82 "dot2l/dotparse.y"
{ D2Lendedge (); attrclass = GRAPH; } break;
case 28:
# line 86 "dot2l/dotparse.y"
{ D2Lmidedge (NODE, yypvt[-1].o, portstr); portstr[0] = '\000'; } break;
case 29:
# line 88 "dot2l/dotparse.y"
{ D2Lmidedge (NODE, yypvt[-1].o, portstr); portstr[0] = '\000'; } break;
case 31:
# line 90 "dot2l/dotparse.y"
{ D2Lmidedge (GRAPH, yypvt[-0].o, ""); portstr[0] = '\000'; } break;
case 32:
# line 92 "dot2l/dotparse.y"
{ D2Lmidedge (GRAPH, yypvt[-0].o, ""); portstr[0] = '\000'; } break;
case 39:
# line 103 "dot2l/dotparse.y"
{
        strcat (portstr, yypvt[-0].s); free (yypvt[-0].s);
    } break;
case 40:
# line 107 "dot2l/dotparse.y"
{
        strcat (portstr, "("); strcat (portstr, yypvt[-3].s);
        strcat (portstr, ","); strcat (portstr, yypvt[-1].s);
        strcat (portstr, ")");
        free (yypvt[-3].s), free (yypvt[-1].s);
    } break;
case 41:
# line 116 "dot2l/dotparse.y"
{
        strcat (portstr, "@"); strcat (portstr, yypvt[-0].s); free (yypvt[-0].s);
    } break;
case 42:
# line 122 "dot2l/dotparse.y"
{ inattrstmt = TRUE; } break;
case 43:
# line 124 "dot2l/dotparse.y"
{ attrclass = GRAPH; inattrstmt = FALSE; } break;
case 44:
# line 126 "dot2l/dotparse.y"
{ attrclass = GRAPH; } break;
case 45:
# line 130 "dot2l/dotparse.y"
{ attrclass = GRAPH; } break;
case 46:
# line 132 "dot2l/dotparse.y"
{ attrclass = NODE; } break;
case 47:
# line 134 "dot2l/dotparse.y"
{ attrclass = EDGE; } break;
case 54:
# line 152 "dot2l/dotparse.y"
{ D2Lsetattr (yypvt[-2].s, yypvt[-0].s); free (yypvt[-2].s); free (yypvt[-0].s); } break;
case 58:
# line 163 "dot2l/dotparse.y"
{ yyval.o = D2Lpopgraph (); } break;
case 59:
# line 164 "dot2l/dotparse.y"
{ D2Lpushgraph (NULL); } break;
case 60:
# line 165 "dot2l/dotparse.y"
{ yyval.o = D2Lpopgraph (); } break;
case 61:
# line 167 "dot2l/dotparse.y"
{ yyval.o = D2Lpopgraph (); } break;
case 62:
# line 171 "dot2l/dotparse.y"
{ D2Lpushgraph (yypvt[-0].s); free (yypvt[-0].s); } break;
	}
	goto yystack;		/* reset registers in driver code */
}

