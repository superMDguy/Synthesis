/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#include "common.h"
#include "g.h"
#include "mem.h"
#include "io.h"
#include "code.h"
#include "tbl.h"
#include "parse.h"
#include "exec.h"
#include "display.h"
#include "txtview.h"
#include "gfxview.h"
#ifdef HAVEDOT
#include "dot2l.h"
#endif
#include "internal.h"

int Idump (int, Tonm_t *);
int Icopy (int, Tonm_t *);
int Iremove (int, Tonm_t *);
int Itablesize (int, Tonm_t *);
int Iopenio (int, Tonm_t *);
int Icloseio (int, Tonm_t *);
int Ireadline (int, Tonm_t *);
int Iread (int, Tonm_t *);
int Iwriteline (int, Tonm_t *);
int Iatan (int, Tonm_t *);
int Icos (int, Tonm_t *);
int Isin (int, Tonm_t *);
int Isqrt (int, Tonm_t *);
int Irandom (int, Tonm_t *);
int Intos (int, Tonm_t *);
int Iston (int, Tonm_t *);
int Isplit (int, Tonm_t *);
int Iconcat (int, Tonm_t *);
int Itoint (int, Tonm_t *);
int Istrlen (int, Tonm_t *);
int Iload (int, Tonm_t *);
int Irun (int, Tonm_t *);
int Imonitor (int, Tonm_t *);
int Iidlerun (int, Tonm_t *);
int Isleep (int, Tonm_t *);
int Iecho (int, Tonm_t *);
int Igetenv (int, Tonm_t *);
int Iputenv (int, Tonm_t *);
int Isystem (int, Tonm_t *);
int Iexit (int, Tonm_t *);
#ifdef HAVEDOT
int Iparsegraphlabel (int, Tonm_t *);
int Ireadgraph (int, Tonm_t *);
#endif
#ifdef HAVECS
int C2Lreadcsmessage (int, Tonm_t *);
#endif

#define MAXN 10000

Ifunc_t MSNEAR Ifuncs[] = {
    { "createwidget",    GFXcreatewidget,  2,    2 },
    { "setwidgetattr",   GFXsetwidgetattr, 2,    2 },
    { "getwidgetattr",   GFXgetwidgetattr, 2,    2 },
    { "destroywidget",   GFXdestroywidget, 1,    1 },
    { "clear",           GFXclear,         1,    1 },
    { "setgfxattr",      GFXsetgfxattr,    2,    2 },
    { "getgfxattr",      GFXgetgfxattr,    2,    2 },
    { "arrow",           GFXarrow,         4,    5 },
    { "line",            GFXline,          4,    5 },
    { "box",             GFXbox,           3,    4 },
    { "polygon",         GFXpolygon,       3,    4 },
    { "splinegon",       GFXsplinegon,     3,    4 },
    { "arc",             GFXarc,           4,    5 },
    { "text",            GFXtext,          7,    8 },
    { "textsize",        GFXtextsize,      4,    4 },
    { "clearpick",       GFXclearpick,     2,    2 },
    { "setpick",         GFXsetpick,       3,    3 },
    { "displaymenu",     GFXdisplaymenu,   2,    2 },
    { "txtview",         TXTmode,          1,    1 },
    { "ask",             TXTask,           1,    1 },
    { "dump",            Idump,            0, MAXN },
    { "copy",            Icopy,            1,    1 },
    { "remove",          Iremove,          1,    2 },
    { "tablesize",       Itablesize,       1,    1 },
    { "openio",          Iopenio,          3,    4 },
    { "closeio",         Icloseio,         1,    2 },
    { "readline",        Ireadline,        1,    1 },
    { "read",            Iread,            1,    1 },
    { "writeline",       Iwriteline,       2,    2 },
    { "atan",            Iatan,            2,    2 },
    { "cos",             Icos,             1,    1 },
    { "sin",             Isin,             1,    1 },
    { "sqrt",            Isqrt,            1,    1 },
    { "random",          Irandom,          1,    1 },
    { "ntos",            Intos,            1,    1 },
    { "ston",            Iston,            1,    1 },
    { "split",           Isplit,           2,    2 },
    { "concat",          Iconcat,          1, MAXN },
    { "toint",           Itoint,           1,    1 },
    { "strlen",          Istrlen,          1,    1 },
    { "load",            Iload,            1,    1 },
    { "run",             Irun,             1,    1 },
    { "monitor",         Imonitor,         2,    2 },
    { "idlerun",         Iidlerun,         1,    1 },
    { "sleep",           Isleep,           1,    1 },
    { "echo",            Iecho,            1, MAXN },
    { "getenv",          Igetenv,          1,    1 },
    { "putenv",          Iputenv,          2,    2 },
    { "system",          Isystem,          1, MAXN },
    { "exit",            Iexit,            0,    0 },
#ifdef HAVEDOT
    { "parsegraphlabel", Iparsegraphlabel, 2,    2 },
    { "readgraph",       Ireadgraph,       1,    1 },
#endif
#ifdef HAVECS
    { "readcsmessage",   C2Lreadcsmessage, 1,    1 },
#endif
    { 0,                 0,                0,    0 }
};
int Ifuncn;

static char *bufp;
static int bufn;
#define BUFINCR 1024
#define BUFSIZE sizeof (char)
static void growbufp (int);

void Iinit (void) {
    int i;

    if (!(bufp = malloc (BUFINCR * BUFSIZE)))
        panic (POS, "Iinit", "buf malloc failed");
    bufn = BUFINCR;
    for (i = 0; Ifuncs[i].name; i++)
        Efunction (Pfunction (Ifuncs[i].name, i), Ifuncs[i].name);
    Ifuncn = sizeof (Ifuncs) / sizeof (Ifunc_t) - 1;
}

void Iterm (void) {
    int i;

    for (i = 0; i < Ifuncn; i++)
        Tdels (root, Ifuncs[i].name);
    Ifuncn = 0;
    free (bufp), bufp = NULL, bufn = 0;
}

int Igetfunc (char *name) {
    int i = 0;

    while (Ifuncs[i].name && Strcmp (Ifuncs[i].name, name) != 0)
        i++;
    return (Ifuncs[i].name) ? i : -1;
}

/* display.c functions */

int Idump (int argc, lvar_t *argv) {
    int i;

    if (argc == 0)
        Dtrace (root, 0);
    else
        for (i = 0; i < argc; i++)
            Dtrace (argv[i].o, 0);
    return SUCCEEDED;
}

/* tbl.c functions */

int Icopy (int argc, lvar_t *argv) {
    rtno = Tcopy (argv[0].o);
    return SUCCEEDED;
}

int Iremove (int argc, lvar_t *argv) {
    Tobj tblo, keyo;

    if (argc == 2)
        tblo = argv[1].o, keyo = argv[0].o;
    else
        tblo = root, keyo = argv[0].o;
    if (T_ISTABLE (tblo) && (T_ISNUMBER (keyo) || T_ISSTRING (keyo)))
        Tdelo (tblo, keyo);
    return SUCCEEDED;
}

int Itablesize (int argc, lvar_t *argv) {
    Tobj vo;

    if (Tgettype ((vo = argv[0].o)) == T_TABLE)
        rtno = Tinteger (((Ttable_t *) vo)->n);
    return SUCCEEDED;
}

/* file.c functions */

int Iopenio (int argc, lvar_t *argv) {
    if (argc == 3)
        rtno = Tinteger (IOopen (Tgetstring (argv[0].o),
                Tgetstring (argv[1].o), Tgetstring (argv[2].o), NULL));
    else
        rtno = Tinteger (IOopen (Tgetstring (argv[0].o),
                Tgetstring (argv[1].o), Tgetstring (argv[2].o),
                Tgetstring (argv[3].o)));
    return SUCCEEDED;
}

int Icloseio (int argc, lvar_t *argv) {
    if (argc == 1)
        IOclose ((int) Tgetnumber (argv[0].o), NULL);
    else
        IOclose ((int) Tgetnumber (argv[0].o), Tgetstring (argv[1].o));
    return SUCCEEDED;
}

int Ireadline (int argc, lvar_t *argv) {
    char *s;
    int m, n;

    s = bufp, n = bufn;
    while ((m = IOreadline ((int) Tgetnumber (argv[0].o), s, n)) != -1) {
        if (m < n - 1)
            break;
        m += (s - bufp);
        growbufp (bufn + BUFINCR);
        s = bufp + m, n = bufn - m;
    }
    if (m != -1)
        rtno = Tstring (bufp);
    return SUCCEEDED;
}

int Iread (int argc, lvar_t *argv) {
    if (IOread ((int) Tgetnumber (argv[0].o), bufp, bufn) != -1)
        rtno = Tstring (bufp);
    return SUCCEEDED;
}

int Iwriteline (int argc, lvar_t *argv) {
    IOwriteline ((int) Tgetnumber (argv[0].o), Tgetstring (argv[1].o));
    return SUCCEEDED;
}

/* math functions */

int Iatan (int argc, lvar_t *argv) {
    double x, y;

    y = Tgetnumber (argv[0].o), x = Tgetnumber (argv[1].o);
    rtno = Treal (180 * atan2 (y, x) / M_PI);
    return SUCCEEDED;
}

int Icos (int argc, lvar_t *argv) {
    rtno = Treal (cos (M_PI * Tgetnumber (argv[0].o) / 180.0));
    return SUCCEEDED;
}

int Isin (int argc, lvar_t *argv) {
    rtno = Treal (sin (M_PI * Tgetnumber (argv[0].o) / 180.0));
    return SUCCEEDED;
}

int Isqrt (int argc, lvar_t *argv) {
    rtno = Treal (sqrt (Tgetnumber (argv[0].o)));
    return SUCCEEDED;
}

#ifdef HAVEMSWIN
#define lrand48 rand
#endif

int Irandom (int argc, lvar_t *argv) {
    rtno = Treal ((Tgetnumber (argv[0].o) *
            (lrand48 () & 0xffff)) / (double) (0xffff));
    return SUCCEEDED;
}

/* conversion functions */

int Intos (int argc, lvar_t *argv) {
    double d;

    d = Tgetnumber (argv[0].o);
    if ((long) d == d)
        sprintf (bufp, "%ld", (long) d);
    else
        sprintf (bufp, "%f", d);
    rtno = Tstring (bufp);
    return SUCCEEDED;
}

int Iston (int argc, lvar_t *argv) {
    rtno = Treal ((double) atof (Tgetstring (argv[0].o)));
    return SUCCEEDED;
}

int Isplit (int argc, lvar_t *argv) {
    Tobj so, fo;
    char *sp, *sp2, *s;
    char fc, tc;
    long rtnm, rtni;

    if (Tgettype ((so = argv[0].o)) != T_STRING ||
            Tgettype ((fo = argv[1].o)) != T_STRING)
        return SUCCEEDED;
    sp = Tgetstring (so);
    s = Tgetstring (fo);
    if (s[0] == '\\' && s[1] == 'n')
        fc = '\n';
    else
        fc = s[0];
    rtno = Ttable (4);
    rtnm = Mpushmark (rtno);
    rtni = 0;
    if (s[0] == 0) {
        for (sp2 = sp; *sp2; sp2++) {
            tc = *(sp2 + 1), *(sp2 + 1) = '\000';
            Tinsi (rtno, rtni++, Tstring (sp2));
            *(sp2 + 1) = tc;
        }
        goto done;
    }
    if (fc == ' ')
        while (*sp == ' ')
            sp++;
    while (*sp) {
        for (sp2 = sp; *sp2 && *sp2 != fc; sp2++)
            ;
        tc = *sp2, *sp2 = '\000';
        Tinsi (rtno, rtni++, Tstring (sp));
        *sp2 = tc;
        if (fc == ' ' && *sp2)
            while (*(sp2 + 1) && *(sp2 + 1) == fc)
                sp2++;
        if (*sp2) {
            if (!*(sp2 + 1) && fc != ' ')
                Tinsi (rtno, rtni++, Tstring (""));
            sp = sp2 + 1;
        } else
            sp = sp2;
    }
done:
    Mpopmark (rtnm);
    return SUCCEEDED;
}

int Iconcat (int argc, lvar_t *argv) {
    Tobj ao;
    char buf2[50];
    char *s;
    int i, n, bufi;

    for (bufi = 0, i = 0; i < argc; i++) {
        ao = argv[i].o;
        switch (Tgettype (argv[i].o)) {
        case T_STRING:
            if (bufi + (n = strlen (Tgetstring (ao)) + 1) > bufn)
                growbufp (bufi + n);
            for (s = Tgetstring (ao); *s; s++)
                bufp[bufi++] = *s;
            break;
        case T_INTEGER:
            if (bufi + 50 > bufn)
                growbufp (bufi + 50);
            sprintf (buf2, "%d", Tgetinteger (ao));
            for (s = buf2; *s; s++)
                bufp[bufi++] = *s;
            break;
        case T_REAL:
            if (bufi + 50 > bufn)
                growbufp (bufi + 50);
            sprintf (buf2, "%f", Tgetreal (ao));
            for (s = buf2; *s; s++)
                bufp[bufi++] = *s;
            break;
        }
    }
    bufp[bufi] = '\000';
    rtno = Tstring (bufp);
    return SUCCEEDED;
}

int Itoint (int argc, lvar_t *argv) {
    rtno = Tinteger ((long) Tgetnumber (argv[0].o));
    return SUCCEEDED;
}

int Istrlen (int argc, lvar_t *argv) {
    rtno = Tinteger (strlen (Tgetstring (argv[0].o)));
    return SUCCEEDED;
}

/* script loading functions */

int Iload (int argc, lvar_t *argv) {
    Psrc_t src;
    char *fn;
    FILE *fp;
    Tobj co;

    if ((fn = Tgetstring (argv[0].o))) {
        if (fn[0] == '-' && fn[1] == '\000')
            fp = stdin;
        else {
            fp = NULL;
            if ((fn = buildpath (fn, FALSE)))
                fp = fopen (fn, "r");
        }
        if (fp) {
            src.flag = FILESRC, src.s = NULL, src.fp = fp;
            src.tok = -1, src.lnum = 1;
            while ((co = Punit (&src)))
                Eoktorun = TRUE, Eunit (co);
            if (fp != stdin)
                fclose (fp);
        } else
            return FAILED;
    }
    return SUCCEEDED;
}

int Irun (int argc, lvar_t *argv) {
    Psrc_t src;
    char *s;
    Tobj co;

    if ((s = Tgetstring (argv[0].o))) {
        src.flag = CHARSRC, src.s = s, src.fp = NULL;
        src.tok = -1, src.lnum = 1;
        while ((co = Punit (&src)))
            Eoktorun = TRUE, Eunit (co);
    }
    return SUCCEEDED;
}

/* mode setting functions */

int Imonitor (int argc, lvar_t *argv) {
    Tobj mo, io;
    char *ms;
    int ioi;

    if (Tgettype ((mo = argv[0].o)) != T_STRING ||
            (Tgettype ((io = argv[1].o)) != T_INTEGER &&
            Tgettype (io) != T_REAL))
        return FAILED;
    ms = Tgetstring (mo), ioi = Tgetnumber (io);
    if (ioi < 0 || ioi >= ion)
        return FAILED;
    if (Strcmp (ms, "on") == 0)
        FD_SET (fileno (iop[ioi].ifp), &inputfds);
    else if (Strcmp (ms, "off") == 0)
        FD_CLR (fileno (iop[ioi].ifp), &inputfds);
    else
        return FAILED;
    return SUCCEEDED;
}

int Iidlerun (int argc, lvar_t *argv) {
    Tobj mo;
    char *ms;
    int mode;

    if (Tgettype ((mo = argv[0].o)) != T_STRING)
        return SUCCEEDED;
    ms = Tgetstring (mo);
    if (Strcmp (ms, "on") == 0)
        mode = 1;
    else if (Strcmp (ms, "off") == 0)
        mode = 0;
    else
        return FAILED;
    idlerunmode = mode;
    return SUCCEEDED;
}

/* system functions */

int Isleep (int argc, lvar_t *argv) {
    Gsync ();
#ifndef HAVEMSWIN
    sleep ((unsigned) Tgetnumber (argv[0].o));
#endif
    return SUCCEEDED;
}

int Iecho (int argc, lvar_t *argv) {
    int i;

    for (i = 0; i < argc; i++) {
        switch (Tgettype (argv[i].o)) {
        case T_STRING:  printf ("%s", Tgetstring (argv[i].o));  break;
        case T_INTEGER: printf ("%d", Tgetinteger (argv[i].o)); break;
        case T_REAL:    printf ("%f", Tgetreal (argv[i].o));    break;
        case T_TABLE:   printf ("[\n"), Dtrace (argv[i].o, 4);  break;
        }
    }
    printf ("\n");
    return SUCCEEDED;
}

int Igetenv (int argc, lvar_t *argv) {
    char *s;

    if (!T_ISSTRING (argv[0].o))
        return FAILED;
    if (!(s = getenv (Tgetstring (argv[0].o))) || !*s)
        return SUCCEEDED;
    rtno = Tstring (s);
    return SUCCEEDED;
}

int Iputenv (int argc, lvar_t *argv) {
    if (!T_ISSTRING (argv[0].o) || !T_ISSTRING (argv[1].o))
        return FAILED;
    bufp[0] = 0;
    strcat (bufp, Tgetstring (argv[0].o));
    strcat (bufp, "=");
    strcat (bufp, Tgetstring (argv[1].o));
    putenv (bufp);
    return SUCCEEDED;
}

int Isystem (int argc, lvar_t *argv) {
    int i;

    bufp[0] = 0;
    strcat (bufp, Tgetstring (argv[0].o));
    for (i = 1; i < argc; i++)
        strcat (bufp, " "), strcat (bufp, Tgetstring (argv[i].o));
#ifndef HAVEMSWIN
    system (bufp);
#else
    {
            UINT handle;
            handle = WinExec (bufp, SW_SHOW);
    }
#endif
    return SUCCEEDED;
}

int Iexit (int argc, lvar_t *argv) {
    longjmp (exitljbuf, 1);
    return SUCCEEDED; /* NOT REACHED */
}

#ifdef HAVEDOT
/* dot related functions */

int Iparsegraphlabel (int argc, lvar_t *argv) {
    rtno = D2Lparsegraphlabel (argv[0].o, argv[1].o);
    return SUCCEEDED;
}

int Ireadgraph (int argc, lvar_t *argv) {
    int ioi;

    if ((ioi = Tgetnumber (argv[0].o)) < 0 || ioi >= ion)
        return SUCCEEDED;
    rtno = D2Lreadgraph (ioi);
    return SUCCEEDED;
}
#endif

static void growbufp (int newsize) {
    if (!(bufp = realloc (bufp,
            ((newsize + BUFINCR - 1) / BUFINCR) * BUFINCR * BUFSIZE)))
        panic (POS, "growbufp", "buf realloc failed");
    bufn = ((newsize + BUFINCR - 1) / BUFINCR) * BUFINCR;
}
