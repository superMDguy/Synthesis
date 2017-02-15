/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include "tcl.h"
#include "dot.h"
#include <malloc.h>

char *Version = "tcldot version 95";
char *Copyright = "Copyright (c) AT&T Corp. 1994, 1995.\n";

extern FILE    *Output_file;
extern int      Output_lang;
extern codegen_t GD_CodeGen, *CodeGen;
extern void	*GDHandleTable;

static void        *graphTblPtr, *nodeTblPtr, *edgeTblPtr;

static void
deleteEdges(interp, g, n)
Tcl_Interp     *interp;
Agraph_t       *g;
Agnode_t       *n;
{
    Agedge_t  **ep, *e, *e1;
    char	buf[16];

    e = agfstedge(g, n);
    while (e) {
        sprintf (buf, "edge%d", e->handle);
	Tcl_DeleteCommand(interp, buf);
	ep = (Agedge_t **) Tcl_HandleXlate(interp, edgeTblPtr, buf);
	if (!ep) fprintf(stderr, "Bad entry in edgeTbl\n");
	Tcl_HandleFree(edgeTblPtr, ep);
	e1 = agnxtedge(g, e, n);
	agdelete(g, e);
	e = e1;
    }
}

static void
deleteNodes(interp, g)
Tcl_Interp     *interp;
Agraph_t       *g;
{
    Agnode_t  **np, *n, *n1;
    char        buf[16];

    n = agfstnode(g);
    while (n) {
	sprintf (buf, "node%d", n->handle);
	Tcl_DeleteCommand(interp, buf);
	np = (Agnode_t **) Tcl_HandleXlate(interp, nodeTblPtr, buf);
	if (!np) fprintf(stderr, "Bad entry in nodeTbl\n");
	Tcl_HandleFree(nodeTblPtr, np);
	deleteEdges(interp, g, n); 
	n1 = agnxtnode(g, n);
	agdelete(g, n);
	n = n1;
    }
}

static void
deleteSubg(interp, g)
Tcl_Interp     *interp;
Agraph_t       *g;
{
    Agraph_t   **sgp, *sg;
    Agnode_t  **np, *n, *n1;
    Agedge_t   *e, *e1;
    char        buf[16];

    n = agfstnode(g);
    while (n) {
	sg = agusergraph(n);
	sprintf (buf, "graph%d", sg->handle);
	Tcl_DeleteCommand(interp, buf);
	sgp = (Agraph_t **) Tcl_HandleXlate(interp, graphTblPtr, buf);
	if (!sgp) fprintf(stderr, "Bad entry in graphTbl\n");
	Tcl_HandleFree(graphTblPtr, sgp);
        e = agfstedge(g, n);
        while (e) {
	    e1 = agnxtedge(g, e, n);
	    agdelete(g, e);
	    e = e1;
        }
	n1 = agnxtnode(g, n);
	agdelete(g, n);
	n = n1;
    }
}

static int
edgecmd(clientData, interp, argc, argv)
    ClientData      clientData;
    Tcl_Interp     *interp;
    int             argc;
    char           *argv[];
{
    char            c, buf[16], *s;
    int             i, length;
    Agraph_t       *g;
    Agedge_t      **ep, *e;
    Agsym_t        *a;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], "\" option ?arg arg ...?", (char *) NULL);
	return TCL_ERROR;
    }
    if (!(ep = (Agedge_t **) Tcl_HandleXlate(interp, edgeTblPtr, argv[0]))) {
	Tcl_AppendResult(interp, " \"", argv[0], "\"", (char *) NULL);
	return TCL_ERROR;
    }
    e = *ep;
    g = e->tail->graph;

    c = argv[1][0];
    length = strlen(argv[1]);

    if ((c == 'd') && (strncmp(argv[1], "delete", length) == 0)) {
	Tcl_HandleFree(edgeTblPtr, ep);
	Tcl_DeleteCommand(interp, argv[0]);
	agdelete(g, e);
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "listattributes", length) == 0)) {
	for (i = 0; i < g->univ->edgeattr->dict->nobj; i++) {
	    a = g->univ->edgeattr->list[i];
	    Tcl_AppendElement(interp, a->name);
	}
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "listnodes", length) == 0)) {
	sprintf (buf, "node%d", e->tail->handle);
	Tcl_AppendElement(interp, buf);
	sprintf (buf, "node%d", e->head->handle);
	Tcl_AppendElement(interp, buf);
	return TCL_OK;

    } else if ((c == 'q') && (strncmp(argv[1], "queryattributes", length) == 0)) {
	for (i = 2; i < argc; i++) {
	    if (a = agfindattr(g->proto->e, argv[i])) {
		Tcl_AppendElement(interp, agxget(e, a->index));
	    } else {
		Tcl_AppendResult(interp, " No attribute named \"",
				 argv[i], "\"", (char *) 0);
		return TCL_ERROR;
	    }
	}
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "setattributes", length) == 0)) {
	if ((argc < 4) || (argc % 2)) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		 argv[0], "\" setattributes attributename attributevalue ?attributename attributevalue? ?...?",
			     (char *) NULL);
	}
	g = g->root;
	for (i = 2; i < argc; i++) {
	    if (!(a = agfindattr(g->proto->e, argv[i])))
		a = agedgeattr(g, argv[i], "");
	    agxset(e, a->index, argv[++i]);
	}
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "showname", length) == 0)) {
	if (g->kind && AGFLAG_DIRECTED) s = "->"; else s = "--";
	Tcl_AppendResult(interp,
		e->tail->name, s, e->head->name, (char *) NULL);
	return TCL_OK;

    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": must be one of:",
		"\n\tdelete, listattributes, listnodes, queryattributes,",
		"\n\tsetattributes, showname",
		(char *) NULL);
	return TCL_ERROR;
    }
}

static int
nodecmd(clientData, interp, argc, argv)
    ClientData      clientData;
    Tcl_Interp     *interp;
    int             argc;
    char           *argv[];
{

    char            c, *cp, buf[16];
    int             i, length;
    Agraph_t       *g;
    Agnode_t      **np, *n, *tail, *head;
    Agedge_t      **ep, *e, *e1;
    Agsym_t        *a;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (!(np = (Agnode_t **) Tcl_HandleXlate(interp, nodeTblPtr, argv[0]))) {
	Tcl_AppendResult(interp, " \"", argv[0], "\"", (char *) NULL);
	return TCL_ERROR;
    }
    n = *np;
    g = n->graph;

    c = argv[1][0];
    length = strlen(argv[1]);


    if ((c == 'a') && (strncmp(argv[1], "addedge", length) == 0)) {
	if ((argc < 3) || (! (argc % 2))) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " addedge head ?attributename attributevalue? ?...?\"",
		(char *) NULL);
	    return TCL_ERROR;
	}
	if (!(np = (Agnode_t **) Tcl_HandleXlate(interp, nodeTblPtr, argv[2]))) {
	    if (!(head = agfindnode(g, argv[2]))) {
    	        Tcl_AppendResult(interp, "Head node \"", argv[2],
			     "\" not found.", (char *) NULL);
	        return TCL_ERROR;
	    }
	} else {
	    head = *np;
	    if (g != head->graph) {
	        Tcl_AppendResult(interp, "Nodes ", argv[0], " and ", argv[2],
		    "are not in the same graph.", (char *) NULL);
	        return TCL_ERROR;
	    }
	}
	ep = (Agedge_t **) AllocEntry (edgeTblPtr, &i);
	sprintf (interp->result, "edge%d", i);
	*ep = e = agedge(g, n, head);
	e->handle = i;
	Tcl_CreateCommand(interp, interp->result, edgecmd, (ClientData) NULL,
			  (Tcl_CmdDeleteProc *) NULL);
	g = g->root;
	for (i = 3; i < argc; i++) {
	    if (!(a = agfindattr(g->proto->e, argv[i])))
		a = agedgeattr(g, argv[i], "");
	    agxset(e, a->index, argv[++i]);
	}
	return TCL_OK;

    } else if ((c == 'd') && (strncmp(argv[1], "delete", length) == 0)) {
	deleteEdges(interp, g, n);
	Tcl_HandleFree(nodeTblPtr, np);
	Tcl_DeleteCommand(interp, argv[0]);
	agdelete(g, n);
	return TCL_OK;

    } else if ((c == 'f') && (strncmp(argv[1], "findedge", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
			     argv[0], " findedge headnodename\"",
			     (char *) NULL);
	    return TCL_ERROR;
	}
	if (!(head = agfindnode(g, argv[2]))) {
	    Tcl_AppendResult(interp, "Head node \"", argv[2],
			     "\" not found.", (char *) NULL);
	    return TCL_ERROR;
	}
	if (!(e = agfindedge(g, n, head))) {
	    sprintf (buf, "node%d", head->handle);
	    Tcl_AppendResult(interp, "Edge \"", argv[0],
			     " - ", buf, "\" not found.",
			     (char *) NULL);
	    return TCL_ERROR;
	}
	sprintf (buf, "edge%d", e->handle);
	Tcl_AppendElement(interp, buf);
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "listattributes", length) == 0)) {
	for (i = 0; i < g->univ->nodeattr->dict->nobj; i++) {
	    a = g->univ->nodeattr->list[i];
	    Tcl_AppendElement(interp, a->name);
	}
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "listedges", length) == 0)) {
	for (e = agfstedge(g, n); e; e = agnxtedge(g, e, n)) {
	    sprintf (buf, "edge%d", e->handle);
	    Tcl_AppendElement(interp, buf);
	}
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "listinedges", length) == 0)) {
	for (e = agfstin(g, n); e; e = agnxtin(g, e, n)) {
	    sprintf (buf, "edge%d", e->handle);
	    Tcl_AppendElement(interp, buf);
	}
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "listoutedges", length) == 0)) {
	for (e = agfstout(g, n); e; e = agnxtout(g, e, n)) {
	    sprintf (buf, "edge%d", e->handle);
	    Tcl_AppendElement(interp, buf);
	}
	return TCL_OK;

    } else if ((c == 'q') && (strncmp(argv[1], "queryattributes", length) == 0)) {
	for (i = 2; i < argc; i++) {
	    if (a = agfindattr(g->proto->n, argv[i])) {
		Tcl_AppendElement(interp, agxget(n, a->index));
	    } else {
		Tcl_AppendResult(interp, " No attribute named \"",
				 argv[i], "\"", (char *) 0);
		return TCL_ERROR;
	    }
	}
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "setattributes", length) == 0)) {
	if ((argc < 4) || (argc % 2)) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		"\" setattributes attributename attributevalue ?attributename attributevalue? ?...?",
		(char *) NULL);
	}
	g = g->root;
	for (i = 2; i < argc; i++) {
	    if (!(a = agfindattr(g->proto->n, argv[i])))
		a = agnodeattr(g, argv[i], "");
	    agxset(n, a->index, argv[++i]);
	}
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "showname", length) == 0)) {
	interp->result = n->name;
	return TCL_OK;

    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
	    "\": must be one of:",
	    "\n\taddedge, listattributes, listedges, listinedges,",
	    "\n\tlistoutedges, queryattributes, setattributes, showname.",
	    (char *) NULL);
	return TCL_ERROR;
    }
}

static int
graphcmd(clientData, interp, argc, argv)
    ClientData      clientData;
    Tcl_Interp     *interp;
    int             argc;
    char           *argv[];
{

    Agraph_t      **gp, *g, **sgp, *sg;
    Agnode_t      **np, *n, *n1, *tail, *head;
    Agedge_t      **ep, *e, *e1;
    Agsym_t        *a;
    char            c, buf[256], *s;
    int             i, j, length;
    FILE           *f;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
	    argv[0], " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (!(gp = (Agraph_t **) Tcl_HandleXlate(interp, graphTblPtr, argv[0]))) {
	Tcl_AppendResult(interp, " \"", argv[0], "\"", (char *) NULL);
	return TCL_ERROR;
    }
    g = *gp;

    c = argv[1][0];
    length = strlen(argv[1]);

    if ((c == 'a') && (strncmp(argv[1], "addedge", length) == 0)) {
	if ((argc < 4) || (argc % 2)) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		"addedge tail head ?attributename attributevalue? ?...?\"",
                (char *) NULL);
	    return TCL_ERROR;
	}
	if (!(np = (Agnode_t **) Tcl_HandleXlate(interp, nodeTblPtr, argv[2]))) {
	    if (!(tail = agfindnode(g, argv[2]))) {
    	        Tcl_AppendResult(interp, "Tail node \"", argv[2],
		     "\" not found.", (char *) NULL);
	        return TCL_ERROR;
	    }
	} else {
	    tail = *np;
	    if (g != tail->graph) {
	        Tcl_AppendResult(interp, "Node ", argv[2],
		    "is not in the graph.", (char *) NULL);
	        return TCL_ERROR;
	    }
	}
	if (!(np = (Agnode_t **) Tcl_HandleXlate(interp, nodeTblPtr, argv[3]))) {
	    if (!(head = agfindnode(g, argv[3]))) {
    	        Tcl_AppendResult(interp, "Head node \"", argv[3],
			     "\" not found.", (char *) NULL);
	        return TCL_ERROR;
	    }
	} else {
	    head = *np;
	    if (g != head->graph) {
	        Tcl_AppendResult(interp, "Node ", argv[3],
		    "is not in the graph.", (char *) NULL);
	        return TCL_ERROR;
	    }
	}
	ep = (Agedge_t **) AllocEntry (edgeTblPtr, &i);
	sprintf (interp->result, "edge%d", i);
	*ep = e = agedge(g, tail, head);
	e->handle = i;
	Tcl_CreateCommand(interp, interp->result, edgecmd, (ClientData) NULL,
			  (Tcl_CmdDeleteProc *) NULL);
	for (i = 4; i < argc; i++) {
	    if (!(a = agfindattr(g->proto->e, argv[i])))
		a = agedgeattr(g, argv[i], "");
	    agxset(e, a->index, argv[++i]);
	}
	return TCL_OK;

    } else if ((c == 'a') && (strncmp(argv[1], "addnode", length) == 0)) {
        np = (Agnode_t **) AllocEntry (nodeTblPtr, &i);
        sprintf (interp->result, "node%d", i);
        if (argc % 2) {
	    /* if odd number of args then argv[2] is name */
	    n = agnode(g, argv[2]);
	    j = 3;
	} else {
	    /* else use handle as name */
	    n = agnode(g, interp->result);
	    j = 2;
	}
	*np = n;
	n->handle = i;
	Tcl_CreateCommand(interp, interp->result, nodecmd, (ClientData) NULL,
	    (Tcl_CmdDeleteProc *) NULL);
	g = g->root;
	for (i = j; i < argc; i++) {
	    if (!(a = agfindattr(g->proto->n, argv[i])))
	        a = agnodeattr(g, argv[i], "");
	    agxset(n, a->index, argv[++i]);
	}
	return TCL_OK;

    } else if ((c == 'a') && (strncmp(argv[1], "addsubgraph", length) == 0)) {
	if (argc < 2) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	         "\" addsubgraph ?name? ?attributename attributevalue? ?...?",
		 (char *) NULL);
	}
        sgp = (Agraph_t **) AllocEntry (graphTblPtr, &i);
        sprintf (interp->result, "graph%d", i);
        if (argc % 2) {
            /* if odd number of args then argv[2] is name */
            sg = agsubg(g, argv[2]);
            j = 3;
        } else {
            /* else use handle as name */
            sg = agsubg(g, interp->result);
            j = 2;
        }
	*sgp = sg;
	sg->handle = i;
	Tcl_CreateCommand(interp, interp->result, graphcmd, (ClientData) NULL,
	    (Tcl_CmdDeleteProc *) NULL);
	for (i = j; i < argc; i++) {
            if (!(a = agfindattr(g->root, argv[i])))
	        a = agraphattr(g->root, argv[i], "");
	    agxset(sg, a->index, argv[++i]);
	}
	return TCL_OK;

    } else if ((c == 'c') && (strncmp(argv[1], "countnodes", length) == 0)) {
	sprintf(buf, "%d", agnnodes(g));
	Tcl_AppendResult(interp, buf, (char *) NULL);
	return TCL_OK;

    } else if ((c == 'c') && (strncmp(argv[1], "countedges", length) == 0)) {
	sprintf(buf, "%d", agnedges(g));
	Tcl_AppendResult(interp, buf, (char *) NULL);
	return TCL_OK;

    } else if ((c == 'd') && (strncmp(argv[1], "delete", length) == 0)) {
	g = g->root;
	deleteNodes(interp, g);
	if (g->meta_node) {
	    deleteSubg(interp, g->meta_node->graph);
	} else {
	    Tcl_DeleteCommand(interp, argv[0]);
	    Tcl_HandleFree(graphTblPtr, gp);
	}
	agclose(g);
	return TCL_OK;

    } else if ((c == 'f') && (strncmp(argv[1], "findedge", length) == 0)) {
	if (argc < 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " findedge tailnodename headnodename\"",
		(char *) NULL);
	    return TCL_ERROR;
	}
	if (!(tail = agfindnode(g, argv[2]))) {
	    Tcl_AppendResult(interp, "Tail node \"", argv[2],
		"\" not found.", (char *) NULL);
	    return TCL_ERROR;
	}
	if (!(head = agfindnode(g, argv[3]))) {
	    Tcl_AppendResult(interp, "Head node \"", argv[3],
			     "\" not found.", (char *) NULL);
	    return TCL_ERROR;
	}
	if (!(e = agfindedge(g, tail, head))) {
	    Tcl_AppendResult(interp, "Edge \"", argv[2],
		" - ", argv[3], "\" not found.",
		(char *) NULL);
	    return TCL_ERROR;
	}
        sprintf (buf, "edge%d", e->handle);
	Tcl_AppendElement(interp, buf);
	return TCL_OK;

    } else if ((c == 'f') && (strncmp(argv[1], "findnode", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " findnode nodename\"",
		(char *) NULL);
	    return TCL_ERROR;
	}
	if (!(n = agfindnode(g, argv[2]))) {
	    Tcl_AppendResult(interp, "Node not found.",
		(char *) NULL);
	    return TCL_ERROR;
	}
        sprintf (buf, "node%d", n->handle);
	Tcl_AppendResult(interp, buf, (char *) NULL);
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "layoutedges", length) == 0)) {
	/* convert attempts to layout subgraphs to layouts of the main graph */
	g = g->root;
	dot_cleanup(g);	/* in case previously drawn */
	dot_init(g);
	dot_rank(g);
	dot_mincross(g);
	dot_position(g);
	dot_splines(g);
	dot_postprocess(g);

	sprintf(buf,"%dp %dp %dp %dp",0,0,g->u.bb.UR.x+1,g->u.bb.UR.y+1);
	if (!(a = agfindattr(g, "bb")))
	     a = agraphattr(g, "bb", "");
	agxset(g, a->index, buf);

	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "layoutnodes", length) == 0)) {
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "listattributes", length) == 0)) {
	for (i = 0; i < g->univ->globattr->dict->nobj; i++) {
	    a = g->univ->globattr->list[i];
	    Tcl_AppendElement(interp, a->name);
	}
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "listedgeattributes", length) == 0)) {
	for (i = 0; i < g->univ->edgeattr->dict->nobj; i++) {
	    a = g->univ->nodeattr->list[i];
	    Tcl_AppendElement(interp, a->name);
	}
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "listnodeattributes", length) == 0)) {
	for (i = 0; i < g->univ->nodeattr->dict->nobj; i++) {
	    a = g->univ->nodeattr->list[i];
	    Tcl_AppendElement(interp, a->name);
	}
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "listnodes", length) == 0)) {
	for (n = agfstnode(g); n; n = agnxtnode(g, n)) {
            sprintf (buf, "node%d", n->handle);
	    Tcl_AppendElement(interp, buf);
	}
	return TCL_OK;

    } else if ((c == 'l') && (strncmp(argv[1], "listsubgraphs", length) == 0)) {
        if (g->meta_node) {
	    for (n = agfstnode(g->meta_node->graph); n; n = agnxtnode(g->meta_node->graph, n)) {
	        sg = agusergraph(n);
	        sprintf (buf, "graph%d", sg->handle);
	        Tcl_AppendElement(interp, buf);
	    }
        }
	return TCL_OK;

    } else if ((c == 'q') && (strncmp(argv[1], "queryattributes", length) == 0)) {
	for (i = 2; i < argc; i++) {
	    if (a = agfindattr(g->root, argv[i])) {
		Tcl_AppendElement(interp, agxget(g, a->index));
	    } else {
		Tcl_AppendResult(interp, " No attribute named \"",
				 argv[i], "\"", (char *) 0);
		return TCL_ERROR;
	    }
	}
	return TCL_OK;

    } else if ((c == 'r') && (strncmp(argv[1], "render", length) == 0)) {
	if (argc < 3) {
	    s = "";
	} {
	    s = argv[2];
        }

	/* convert attempts to render subgraphs to renders of the main graph */
	g = g->root;

        render(interp, g, s);

	return TCL_OK;

    } else if ((c == 'r') && (strncmp(argv[1], "rendergd", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		" rendergd gdhandle\"", (char *) NULL);
            return TCL_ERROR;
        }
	if (! (Output_file = (FILE *)Tcl_HandleXlate(interp, GDHandleTable, argv[2]))) {
	    Tcl_AppendResult(interp, "GD Image not found.", (char *) NULL);
	    return TCL_ERROR;
	}

	/* convert attempts to render subgraphs to renders of the main graph */
	g = g->root;

	/* render graph to open GD structure */
	CodeGen = &GD_CodeGen;
	emit_graph(g);

	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "setattributes", length) == 0)) {
	if ((argc < 4) || (argc % 2)) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	         " setattributes attributename attributevalue ?attributename attributevalue? ?...?\"",
		 (char *) NULL);
	     return TCL_ERROR;
	}
	for (i = 2; i < argc; i++) {
	    /* this may be a subgraph.
	       Make sure the attribute name exists in the root graph */
            if (!(a = agfindattr(g->root, argv[i])))
	        a = agraphattr(g->root, argv[i], "");
	    agxset(g, a->index, argv[++i]);
	}
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "setedgeattributes", length) == 0)) {
	if ((argc < 4) || (argc % 2)) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	         "\" setedgeattributes attributename attributevalue ?attributename attributevalue? ?...?",
		 (char *) NULL);
	     return TCL_ERROR;
	}
	for (i = 2; i < argc; i++) {
	    j = i++;
	    agedgeattr(g, argv[j], argv[i]);
	}
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "setnodeattributes", length) == 0)) {
	if ((argc < 4) || (argc % 2)) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	         "\" setnodeattributes attributename attributevalue ?attributename attributevalue? ?...?",
		 (char *) NULL);
	     return TCL_ERROR;
	}
	for (i = 2; i < argc; i++) {
	    j = i++;
	    agnodeattr(g, argv[j], argv[i]);
	}
	return TCL_OK;

    } else if ((c == 's') && (strncmp(argv[1], "showname", length) == 0)) {
	interp->result = g->name;
	return TCL_OK;

    } else if ((c == 'w') && (strncmp(argv[1], "write", length) == 0)) {
	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " write fileHandle ?language?\"", (char *) NULL);
	    return TCL_ERROR;
	}
	if (Tcl_GetOpenFile(interp, argv[2], 1, 1, &Output_file) != TCL_OK)
	    return TCL_ERROR;
	if (argc < 4) {
	    Output_lang = ATTRIBUTED_DOT;
        } else {
	    Output_lang = lang_select(argv[3]);
	}
	/* convert attempts to write subgraphs to writes of the main graph */
	g = g->root;
	emit_reset(g);		/* reset page numbers in postscript */
	dot_write(g);
	return TCL_OK;

    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
	    "\": must be one of:",
	    "\n\taddedge, addnode, addsubgraph, countedges, countnodes,",
	    "\n\tlayout, listattributes, listedgeattributes, listnodeattributes,",
	    "\n\tlistnodes, listsubgraphs, render, rendergd, queryattributes, setattributes,",
	    "\n\tsetedgeattributes, setnodeattributes, showname, write.",
	    (char *) NULL);
	return TCL_ERROR;
    }
}				/* graphcmd */

static int
dotnew(clientData, interp, argc, argv)
    ClientData      clientData;
    Tcl_Interp     *interp;
    int             argc;
    char           *argv[];
{
    Agraph_t       *g, **gp;
    Agsym_t	   *a;
    char            c, *nam;
    int             i, j, length, kind;

    if ((argc < 2)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
	    argv[0], " graphtype ?graphname? ?attributename attributevalue? ?...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == 'd') && (strncmp(argv[1], "digraph", length) == 0)) {
	kind = AGDIGRAPH;
    } else if ((c == 'd') && (strncmp(argv[1], "digraphstrict", length) == 0)) {
	kind = AGDIGRAPHSTRICT;
    } else if ((c == 'g') && (strncmp(argv[1], "graph", length) == 0)) {
	kind = AGRAPH;
    } else if ((c == 'g') && (strncmp(argv[1], "graphstrict", length) == 0)) {
	kind = AGRAPHSTRICT;
    } else {
	Tcl_AppendResult(interp, "bad graphtype \"", argv[1],
	    "\": must be one of:",
	    "\n\tdigraph, digraphstrict, graph, graphstrict.",
	    (char *) NULL);
	return TCL_ERROR;
    }
    gp = (Agraph_t **) AllocEntry (graphTblPtr, &i);
    sprintf (interp->result, "graph%d", i);
    if (argc % 2) {
        /* if odd number of args then argv[2] is name */
        g = agopen(argv[2], kind);
        j = 3;
    } else {
        /* else use handle as name */
        g = agopen(interp->result, kind);
        j = 2;
    }
    if (!g) {
	Tcl_AppendResult(interp, "Failure to open graph.", (char *) NULL);
	return TCL_ERROR;
    }
    *gp = g;
    g->handle = i;
    Tcl_CreateCommand(interp, interp->result, graphcmd, (ClientData) NULL,
	(Tcl_CmdDeleteProc *) NULL);
    for (i = j; i < argc; i++) {
        if (!(a = agfindattr(g, argv[i])))
	    a = agraphattr(g, argv[i], "");
	agxset(g, a->index, argv[++i]);
    }
    return TCL_OK;
}

static int
dotread(clientData, interp, argc, argv)
    ClientData      clientData;
    Tcl_Interp     *interp;
    int             argc;
    char           *argv[];
{
    Agraph_t       *g, **gp, *sg, **sgp;
    Agnode_t       *n, **np;
    Agedge_t       *e, **ep;
    char            buf[16];
    FILE           *f;
    int             i;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
	    argv[0], " fileHandle\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (Tcl_GetOpenFile(interp, argv[1], 0, 0, &f) != TCL_OK)
	return TCL_ERROR;
    if (!(g = agread(f))) {
	Tcl_AppendResult(interp, "Failure to read file \"",
	    argv[1], "\"", (char *) NULL);
        if (i = agerrors()) {
	    Tcl_AppendResult(interp, " because of syntax errors.",
		(char *) NULL);
	}
	return TCL_ERROR;
    }
    if (agerrors()) {
	Tcl_AppendResult(interp, "Syntax errors in file \"",
	    argv[1], " \"", (char *) NULL);
	return TCL_ERROR;
    }
    if (g->meta_node) {
	for (n = agfstnode(g->meta_node->graph); n; n = agnxtnode(g->meta_node->graph, n)) {
	    sg = agusergraph(n);
	    sgp = (Agraph_t **) AllocEntry (graphTblPtr, &i);
	    *sgp = sg;
	    sg->handle = i;
	    sprintf (buf, "graph%d", i);
	    Tcl_CreateCommand(interp, buf, graphcmd, (ClientData) NULL,
		(Tcl_CmdDeleteProc *) NULL);
	    if (sg == g) sprintf (interp->result, "graph%d", i);
	}
    } else {
        gp = (Agraph_t **) AllocEntry (graphTblPtr, &i);
        *gp = g;
        g->handle = i;
        sprintf (interp->result, "graph%d", i);
        Tcl_CreateCommand(interp, interp->result, graphcmd, (ClientData) NULL,
	    (Tcl_CmdDeleteProc *) NULL);
    }
    for (n = agfstnode(g); n; n = agnxtnode(g, n)) {
        np = (Agnode_t **) AllocEntry (nodeTblPtr, &i);
	*np = n;
	n->handle = i;
	sprintf (buf, "node%d", i);
	Tcl_CreateCommand(interp, buf, nodecmd, (ClientData) NULL,
	    (Tcl_CmdDeleteProc *) NULL);
	for (e = agfstedge(g, n); e; e = agnxtedge(g, e, n)) {
	    ep = (Agedge_t **) AllocEntry (edgeTblPtr, &i);
	    *ep = e;
	    e->handle = i;
	    sprintf (buf, "edge%d", i);
	    Tcl_CreateCommand(interp, buf, edgecmd, (ClientData) NULL,
	        (Tcl_CmdDeleteProc *) NULL);
	}
    }
    return TCL_OK;
}

void
Tcl_dotGraph_Init(interp)
    Tcl_Interp     *interp;
{
    aginit();
    /* set persistent attributes here */
    agnodeattr(NULL, "label", NODENAME_ESC);

    Tcl_CreateCommand(interp, "dotnew", dotnew, (ClientData) NULL,
	(Tcl_CmdDeleteProc *) NULL);
    Tcl_CreateCommand(interp, "dotread", dotread, (ClientData) NULL,
	(Tcl_CmdDeleteProc *) NULL);

    graphTblPtr = (void *) Tcl_HandleTblInit("graph", sizeof(Agraph_t *), 10);
    nodeTblPtr = (void *) Tcl_HandleTblInit("node", sizeof(Agnode_t *), 100);
    edgeTblPtr = (void *) Tcl_HandleTblInit("edge", sizeof(Agedge_t *), 100);
}
