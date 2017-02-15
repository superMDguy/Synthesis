/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped

#include "common.h"
#include "mem.h"
#include "code.h"
#include "tbl.h"
#include "exec.h"
#include "dot2l.h"

char *gtype, *etype;
int yaccdone;
int attrclass;
int inattrstmt;

static graphframe_t *gstack, *topgframe;
static Tobj allgraphs, alledges, allnodes;
static Tobj gdict, edict, ndict, N;
static long newgid, neweid, newnid, gmark, errflag;

static jmp_buf ljbuf;

static void filllabeltable (Tobj, int);
static void filllabelrect (Tobj);

static char *lsp, *rsp;

Tobj D2Lparsegraphlabel (Tobj lo, Tobj ro) {
    volatile long lm;
    volatile Tobj to;

    lm = Mpushmark (lo);
    Mpushmark (ro);
    to = Ttable (4);
    Mpushmark (to);
    lsp = Tgetstring (lo);
    if (ro && T_ISSTRING (ro))
        rsp = Tgetstring (ro);
    else
        rsp = NULL;

    if (setjmp (ljbuf)) {
        to = NULL;
        fprintf (stderr, "error in label >>%s<<\n", lsp);
    } else
        filllabeltable (to, TRUE);
    Mpopmark (lm);
    return to;
}

#define HASTEXT 1
#define HASPORT 2
#define HASTABLE 4
#define INTEXT 8
#define INPORT 16

#define ISCTRL(c) \
    ((c) == '{' || (c) == '}' || (c) == '|' || (c) == '<' || (c) == '>')

static void filllabeltable (Tobj to, int flag) {
    Tobj cto, fo;
    char *tsp, *psp, *hstsp, *hspsp;
    char text[BUFSIZ], port[256];
    int mode, wflag, cti, ishardspace;

    mode = 0;
    cti = 0;
    Tinsi (to, cti++, (cto = Ttable (2)));
    hstsp = tsp = &text[0], hspsp = psp = &port[0];
    wflag = TRUE;
    ishardspace = FALSE;
    while (wflag) {
        switch (*lsp) {
        case '<':
            if (mode & (HASTABLE | HASPORT))
                longjmp (ljbuf, 1); /* DOESN'T RETURN */
            mode &= ~INTEXT;
            mode |= (HASPORT | INPORT);
            lsp++;
            break;
        case '>':
            if (!(mode & INPORT))
                longjmp (ljbuf, 1); /* DOESN'T RETURN */
            mode &= ~INPORT;
            lsp++;
            break;
        case '{':
            lsp++;
            if (mode != 0 || !*lsp)
                longjmp (ljbuf, 1); /* DOESN'T RETURN */
            Tinss (cto, "fields", (fo = Ttable (2)));
            mode = HASTABLE;
            filllabeltable (fo, FALSE);
            break;
        case '}':
        case '|':
        case '\000':
            if ((!*lsp && !flag) || (mode & INPORT))
                longjmp (ljbuf, 1); /* DOESN'T RETURN */
            if (mode & HASPORT) {
                if (psp > &port[0] + 1 && psp - 1 != hspsp && *(psp - 1) == ' ')
                    psp--;
                *psp = '\000';
                Tinss (cto, "port", Tstring (&port[0]));
                hspsp = psp = &port[0];
            }
            if (!(mode & (HASTEXT | HASTABLE)))
                mode |= HASTEXT, *tsp++ = ' ';
            if (mode & HASTEXT) {
                if (tsp > &text[0] + 1 && tsp - 1 != hstsp && *(tsp - 1) == ' ')
                    tsp--;
                *tsp = '\000';
                Tinss (cto, "text", Tstring (&text[0]));
                hstsp = tsp = &text[0];
            }
            if (mode & (HASTEXT | HASPORT))
                filllabelrect (cto);
            if (*lsp) {
                if (*lsp == '}') {
                    lsp++;
                    return;
                }
                Tinsi (to, cti++, (cto = Ttable (2)));
                mode = 0;
                lsp++;
            } else
                wflag = FALSE;
            break;
        case '\\':
            if (*(lsp + 1))
                if (ISCTRL (*(lsp + 1)))
                    lsp++;
                else if (*(lsp + 1) == ' ')
                    ishardspace = TRUE, lsp++;
            /* falling through ... */
        default:
            if ((mode & HASTABLE) && *lsp != ' ')
                longjmp (ljbuf, 1); /* DOESN'T RETURN */
            if (!(mode & (INTEXT | INPORT)) && (ishardspace || *lsp != ' '))
                mode |= (INTEXT | HASTEXT);
            if (mode & INTEXT) {
                if (!(*lsp == ' ' && !ishardspace && *(tsp - 1) == ' '))
                    *tsp++ = *lsp;
                if (ishardspace)
                    hstsp = tsp - 1;
            } else if (mode & INPORT) {
                if (!(*lsp == ' ' && !ishardspace && (psp == &port[0] ||
                        *(psp - 1) == ' ')))
                    *psp++ = *lsp;
                if (ishardspace)
                    hspsp = psp - 1;
            }
            ishardspace = FALSE;
            lsp++;
            break;
        }
    }
    return;
}

static void filllabelrect (Tobj to) {
    Tobj ro, p0o, p1o;
    char *s2, *s3;
    int c, c2, i;

    if (!rsp)
        return;
    for (s2 = rsp; *s2 && *s2 != ' '; s2++)
        ;
    c = *s2, *s2 = 0;
    Tinss (to, "rect", (ro = Ttable (2)));
    Tinsi (ro, 0, (p0o = Ttable (2)));
    Tinsi (ro, 1, (p1o = Ttable (2)));
    for (i = 0; i < 4; i++) {
        for (s3 = rsp; *s3 && *s3 != ','; s3++)
            ;
        c2 = *s3, *s3 = 0;
        if (s3 == rsp)
            longjmp (ljbuf, 1); /* DOESN'T RETURN */
        switch (i) {
        case 0: Tinss (p0o, "x", Tinteger (atoi (rsp)));
        case 1: Tinss (p0o, "y", Tinteger (atoi (rsp)));
        case 2: Tinss (p1o, "x", Tinteger (atoi (rsp)));
        case 3: Tinss (p1o, "y", Tinteger (atoi (rsp)));
        }
        *s3 = c2;
        rsp = s3;
        if (*rsp == ',')
            rsp++;
    }
    *s2 = c;
    rsp = s2 + 1;
}

static Tobj nameo, attro, edgeso, hporto, tporto, heado, tailo;

Tobj D2Lreadgraph (int ioi) {
    graphframe_t *gframe, *tgframe;
    edgeframe_t *eframe, *teframe;
    Tobj graph;
    long m;

    extern int lex_begin (int);

    nameo = Tstring ("name");
    m = Mpushmark (nameo);
    attro = Tstring ("attr");
    Mpushmark (attro);
    edgeso = Tstring ("edges");
    Mpushmark (edgeso);
    hporto = Tstring ("hport");
    Mpushmark (hporto);
    tporto = Tstring ("tport");
    Mpushmark (tporto);
    heado = Tstring ("head");
    Mpushmark (heado);
    tailo = Tstring ("tail");
    Mpushmark (tailo);
    yaccdone = FALSE;
    gstack = topgframe = NULL;
    errflag = FALSE;
    lex_begin (ioi);
    yyparse ();
    graph = NULL;
    if (topgframe) {
        graph = (errflag) ? NULL : topgframe->g;
        for (gframe = gstack; gframe; gframe = tgframe) {
            for (eframe = gframe->estack; eframe; eframe = teframe) {
                teframe = eframe->next;
                Msetsize (eframe, M_BYTE2SIZE (sizeof (edgeframe_t)));
                Mfree (eframe);
            }
            tgframe = gframe->next;
            Msetsize (gframe, M_BYTE2SIZE (sizeof (graphframe_t)));
            Mfree (gframe);
        }
        goto done;
    }
done:
    Mpopmark (m);
    return graph;
}

void D2Lbegin (char *name) {
    Tobj dotdefs;

    newgid = neweid = newnid = 0;
    attrclass = GRAPH;

    if (!(gstack = Mallocate (sizeof (graphframe_t))))
        panic (POS, "D2Lbegingraph", "cannot allocate graph stack");
    gstack->next = NULL;
    gstack->estack = NULL;
    topgframe = gstack;

    gmark = Mpushmark ((gstack->g = Ttable (12)));
    Tinss (gstack->g, "type", Tstring (gtype));
    Tinss (gstack->g, "name", Tstring (name));

    /* the dictionaries */
    Tinss (gstack->g, "graphdict", (gdict = Ttable (10)));
    Tinss (gstack->g, "nodedict", (ndict = Ttable (10)));
    Tinss (gstack->g, "edgedict", (edict = Ttable (10)));

    /* this graph's nodes, edges, subgraphs */
    Tinss (gstack->g, "nodes", (allnodes = gstack->nodes = Ttable (10)));
    Tinss (gstack->g, "graphs", (allgraphs = gstack->graphs = Ttable (10)));
    Tinss (gstack->g, "edges", (alledges = gstack->edges = Ttable (10)));

    /* attributes */
    gstack->gattr = gstack->nattr = gstack->eattr = NULL;
    if ((dotdefs = Tfinds (root, "dot"))) {
        gstack->gattr = Tfinds (dotdefs, "graphattr");
        gstack->nattr = Tfinds (dotdefs, "nodeattr");
        gstack->eattr = Tfinds (dotdefs, "edgeattr");
    }
    gstack->gattr = (gstack->gattr ? Tcopy (gstack->gattr) : Ttable (10));
    Tinss (gstack->g, "graphattr", gstack->gattr);
    gstack->nattr = (gstack->nattr ? Tcopy (gstack->nattr) : Ttable (10));
    Tinss (gstack->g, "nodeattr", gstack->nattr);
    gstack->eattr = (gstack->eattr ? Tcopy (gstack->eattr) : Ttable (10));
    Tinss (gstack->g, "edgeattr", gstack->eattr);
    gstack->ecopy = gstack->eattr;
}

void D2Lend (void) {
    Mpopmark (gmark);
    yaccdone = TRUE;
}

void D2Labort (void) {
    Mpopmark (gmark);
    errflag = TRUE;
    yaccdone = TRUE;
}

void D2Lpushgraph (char *name) {
    graphframe_t *gframe;
    Tobj g, idobj, nameobj;
    long gid;

    if (!(gframe = Mallocate (sizeof (graphframe_t))))
        panic (POS, "D2Lpushgraph", "cannot allocate graph stack");
    gframe->next = gstack, gstack = gframe;
    gstack->estack = NULL;

    if (name && (idobj = Tfinds (gdict, name))) {
        gid = Tgetnumber (idobj), gstack->g = g = Tfindi (allgraphs, gid);
        gstack->nodes = Tfinds (g, "nodes");
        gstack->graphs = Tfinds (g, "graphs");
        gstack->edges = Tfinds (g, "edges");
        gstack->gattr = Tfinds (g, "graphattr");
        gstack->nattr = Tfinds (g, "nodeattr");
        gstack->ecopy = gstack->eattr = Tfinds (g, "edgeattr");
        return;
    }
    if (!name)
        gid = newgid++, nameobj = Tinteger (gid),
                Tinso (gdict, nameobj, nameobj);
    else
        Tinso (gdict, (nameobj = Tstring (name)), Tinteger ((gid = newgid++)));
    Tinsi (allgraphs, gid, (gstack->g = g = Ttable (10)));
    Tinss (g, "name", nameobj);
    Tinss (g, "nodes", (gstack->nodes = Ttable (10)));
    Tinss (g, "graphs", (gstack->graphs = Ttable (10)));
    Tinss (g, "edges", (gstack->edges = Ttable (10)));
    Tinss (g, "graphattr", (gstack->gattr = Tcopy (gstack->next->gattr)));
    Tinss (g, "nodeattr", (gstack->nattr = Tcopy (gstack->next->nattr)));
    Tinss (g, "edgeattr",
            (gstack->ecopy = gstack->eattr = Tcopy (gstack->next->eattr)));
    for (gframe = gstack->next; gframe->graphs != allgraphs;
            gframe = gframe->next) {
        if (Tfindi (gframe->graphs, gid))
            break;
        Tinsi (gframe->graphs, gid, g);
    }
    return;
}

Tobj D2Lpopgraph (void) {
    graphframe_t *gframe;
    Tobj g;

    gframe = gstack, gstack = gstack->next;
    g = gframe->g;
    Msetsize (gframe, M_BYTE2SIZE (sizeof (graphframe_t))), Mfree (gframe);
    return g;
}

Tobj D2Linsertnode (char *name) {
    graphframe_t *gframe;
    Tobj n, idobj, nameobj;
    long nid, m;

    if ((idobj = Tfinds (ndict, name))) {
        nid = Tgetnumber (idobj), n = Tfindi (allnodes, nid);
    } else {
        m = Mpushmark ((nameobj = Tstring (name)));
        Tinso (ndict, nameobj, Tinteger ((nid = newnid++)));
        Mpopmark (m);
        Tinsi (allnodes, nid, (n = Ttable (3)));
        Tinso (n, nameo, nameobj);
        Tinso (n, attro, Tcopy (gstack->nattr));
        Tinso (n, edgeso, Ttable (2));
    }
    for (gframe = gstack; gframe->nodes != allnodes; gframe = gframe->next) {
        if (Tfindi (gframe->nodes, nid))
            break;
        Tinsi (gframe->nodes, nid, n);
    }
    N = n;
    return n;
}

void D2Linsertedge (Tobj tail, char *tport, Tobj head, char *hport) {
    graphframe_t *gframe;
    Tobj e;
    long eid;

    Tinsi (alledges, (eid = neweid++),
            (e = Ttable (3 + (tport ? 1 : 0) + (hport ? 1 : 0))));
    Tinso (e, tailo, tail);
    if (tport && tport[0])
        Tinso (e, tporto, Tstring (tport));
    Tinso (e, heado, head);
    if (hport && hport[0])
        Tinso (e, hporto, Tstring (hport));
    Tinso (e, attro, Tcopy (gstack->ecopy));
    Tinsi (Tfinds (head, "edges"), eid, e);
    Tinsi (Tfinds (tail, "edges"), eid, e);
    for (gframe = gstack; gframe->edges != alledges; gframe = gframe->next)
        Tinsi (gframe->edges, eid, e);
}

void D2Lbeginedge (int type, Tobj obj, char *port) {
    if (!(gstack->estack = Mallocate (sizeof (edgeframe_t))))
        panic (POS, "D2Lbeginedge", "cannot allocate edge stack");
    gstack->estack->next = NULL;
    gstack->estack->type = type;
    gstack->estack->obj = obj;
    gstack->estack->port = strdup (port);
    gstack->emark = Mpushmark ((gstack->ecopy = Tcopy (gstack->eattr)));
}

void D2Lmidedge (int type, Tobj obj, char *port) {
    edgeframe_t *eframe;

    if (!(eframe = Mallocate (sizeof (edgeframe_t))))
        panic (POS, "D2Lmidedge", "cannot allocate edge stack");
    eframe->next = gstack->estack, gstack->estack = eframe;
    gstack->estack->type = type;
    gstack->estack->obj = obj;
    gstack->estack->port = strdup (port);
}

void D2Lendedge (void) {
    edgeframe_t *eframe, *hframe, *tframe;
    Tobj tnodes, hnodes;
    Tkvindex_t tkvi, hkvi;

    for (eframe = gstack->estack; eframe->next; eframe = tframe) {
        hframe = eframe, tframe = eframe->next;
        if (hframe->type == NODE && tframe->type == NODE) {
            D2Linsertedge (tframe->obj, tframe->port,
                    hframe->obj, hframe->port);
        } else if (hframe->type == NODE && tframe->type == GRAPH) {
            tnodes = Tfinds (tframe->obj, "nodes");
            for (Tgetfirst (tnodes, &tkvi); tkvi.kvp; Tgetnext (&tkvi))
                D2Linsertedge (tkvi.kvp->vo, NULL, hframe->obj, hframe->port);
        } else if (eframe->type == GRAPH && eframe->next->type == NODE) {
            hnodes = Tfinds (hframe->obj, "nodes");
            for (Tgetfirst (hnodes, &hkvi); hkvi.kvp; Tgetnext (&hkvi))
                D2Linsertedge (tframe->obj, tframe->port, hkvi.kvp->vo, NULL);
        } else {
            tnodes = Tfinds (tframe->obj, "nodes");
            hnodes = Tfinds (hframe->obj, "nodes");
            for (Tgetfirst (tnodes, &tkvi); tkvi.kvp; Tgetnext (&tkvi))
                for (Tgetfirst (hnodes, &hkvi); hkvi.kvp; Tgetnext (&hkvi))
                    D2Linsertedge (tkvi.kvp->vo, NULL, hkvi.kvp->vo, NULL);
        }
        free (eframe->port);
        Msetsize (eframe, M_BYTE2SIZE (sizeof (edgeframe_t))), Mfree (eframe);
    }
    free (eframe->port);
    Msetsize (eframe, M_BYTE2SIZE (sizeof (edgeframe_t))), Mfree (eframe);
    Mpopmark (gstack->emark);
    gstack->estack = NULL;
}

void D2Lsetattr (char *name, char *value) {
    if (inattrstmt) {
        switch (attrclass) {
        case NODE: Tinss (gstack->nattr, name, Tstring (value)); break;
        case EDGE: Tinss (gstack->eattr, name, Tstring (value)); break;
        case GRAPH: Tinss (gstack->gattr, name, Tstring (value)); break;
        }
        return;
    }
    switch (attrclass) {
    case NODE: Tinss (Tfinds (N, "attr"), name, Tstring (value)); break;
    case EDGE: Tinss (gstack->ecopy, name, Tstring (value)); break;
    /* a subgraph cannot have optional attrs? */
    case GRAPH: Tinss (gstack->gattr, name, Tstring (value)); break;
    }
}
