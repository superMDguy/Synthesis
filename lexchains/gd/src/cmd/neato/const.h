/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#ifndef NULL
#define NULL 0
#endif

#ifndef PI
#define	PI			3.14159265
#endif
#define SMALLBUF	128
#define LPAREN		'('
#define RPAREN		')'
#define LBRACE		'{'
#define RBRACE		'}'

/*	node,edge types */
#define		NORMAL		0		/* an original input node */
#define		VIRTUAL		1		/* virtual nodes in long edge chains */
#define		SLACKNODE	2		/* encode edges in node position phase */
#define		REVERSED	3		/* reverse of an original edge */
#define		FLATORDER	4		/* for ordered edges */
#define		CLUSTER_EDGE 5		/* for ranking clusters */
#define		IGNORED		6		/* concentrated multi-edges */

/* collapsed node classifications */
#define		NOCMD		0		/* default */
#define		SAMERANK	1		/* place on same rank */
#define		MINRANK		2		/* place on "least" rank */
#define		SOURCERANK	3		/* strict version of MINRANK */
#define		MAXRANK		4		/* place on "greatest" rank */
#define		SINKRANK	5		/* strict version of MAXRANK */
#define		LEAFSET		6		/* set of collapsed leaf nodes */
#define		CLUSTER		7		/* set of clustered nodes */

/* type of cluster rank assignment */
#define		LOCAL		100
#define		GLOBAL		101
#define		NOCLUST		102

/* default attributes */
#define		DEFAULT_COLOR		"black"
#define		DEFAULT_FONTSIZE	14.0
#define		MIN_FONTSIZE		1.0
#define		DEFAULT_FONTNAME	"Times-Roman"
#define		DEFAULT_FILL		"lightgrey"

#define		DEFAULT_NODEHEIGHT	0.5
#define		MIN_NODEHEIGHT		0.02
#define		DEFAULT_NODEWIDTH	0.75
#define		MIN_NODEWIDTH		0.01
#define		DEFAULT_NODESHAPE	"ellipse"
#define		NODENAME_ESC		"\\N"

#define		DEFAULT_NODESEP	0.25
#define		MIN_NODESEP		0.02	
#define		DEFAULT_RANKSEP	0.5
#define		MIN_RANKSEP		0.02
#define		DEFAULT_MARGIN	36
#define		DEFAULT_PAGEHT	792
#define		DEFAULT_PAGEWD	612

#define		SELF_EDGE_SIZE	18
#define		MC_SCALE		256	/* for mincross */
#define		ARROW_LENGTH	10
#define		ARROW_WIDTH		5

/* sides of boxes for SHAPE_path */
#define		BOTTOM		(1<<1)
#define		RIGHT		(1<<2)
#define		TOP			(1<<3)
#define		LEFT		(1<<4)

/* output languages */
#define		ATTRIBUTED_DOT	0		/* default */
#define		POSTSCRIPT	1
#define		HPGL		2
#define		PCL			3
#define		MIF			4
#define		PLAIN		5
#define		GIF			6		/* bitmap format */
#define 	ISMAP		7		/* control file for WWW httpd servers */
#define		CANONICAL_DOT   8	/* wanted for tcl/tk version */

/* for clusters */
#define		CL_BACK		10		/* cost of backward pointing edge */
#define		CL_OFFSET	8		/* margin of cluster box in PS points */
#ifndef DOS
#define		CL_CROSS	1000	/* cost of cluster skeleton edge crossing */
#else
#define		CL_CROSS	100		/* avoid 16 bit overflow */
#endif

/* for graph server */
#define		SERVER_NN	200
#define		SERVER_NE	500
