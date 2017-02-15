/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#define LEFTYVERSION "95a (01-27-95)"

#include "common.h"
#include "g.h"
#include "code.h"
#include "io.h"
#include "mem.h"
#include "tbl.h"
#include "parse.h"
#include "exec.h"
#include "str.h"
#include "display.h"
#include "internal.h"
#include "txtview.h"
#include "gfxview.h"

static Grect_t txtcoords = {
    { 1,  1 }, { 449, 599 },
};

#ifdef STATS
static time_t stime;
#endif

static int endflag = FALSE;

static char *exprstr;
static FILE *fp;

static int processinput (int);
static void processargs (int, char **);
static void processstr (char *);
static void printusage (void);

#ifndef HAVEMSWIN

int main (int argc, char **argv) {
    Tobj co;
    Psrc_t src;

#ifdef MTRACE
    extern int Mt_certify;
    Mt_certify = 1;
#endif
#ifdef STATS
    stime = time (NULL);
#endif

    idlerunmode = 0;
    exprstr = NULL;
    fp = NULL;
    init (argv[0]);
    Minit (GFXprune);
    Ginit ();
    FD_SET (Gxfd, &inputfds);

    Eerrlevel = 1;
    Estackdepth = 2;
    Eshowbody = 1;
    Eshowcalls = 1;

    argv++, argc--;
    processstr (leftyoptions);
    processargs (argc, argv);

    if (setjmp (exitljbuf))
        goto eop;

    Cinit ();
    IOinit ();
    Tinit ();
    Pinit ();
    Einit ();
    Sinit ();
    Dinit ();
    Iinit ();
    TXTinit (txtcoords);
    GFXinit ();

    if (exprstr) {
        src.flag = CHARSRC, src.s = exprstr, src.fp = NULL;
        src.tok = -1, src.lnum = 1;
        while ((co = Punit (&src)))
            Eunit (co);
    }
    if (fp) {
        src.flag = FILESRC, src.s = NULL, src.fp = fp;
        src.tok = -1, src.lnum = 1;
        while ((co = Punit (&src)))
            Eunit (co);
    }
    if (endflag)
        goto eop;

    TXTupdate ();

    Gneedredraw = FALSE;
    for (;;) {
        if (Gbuttonsdown > 0)
            GFXmove (), Gprocessevents (FALSE, G_ONEEVENT);
        else {
            if (Gneedredraw)
                GFXredraw (), Gneedredraw = FALSE;
            if (Mcouldgc) {
                if (!processinput (FALSE))
                    Mdogc (M_GCINCR);
            } else if (idlerunmode) {
                if (!processinput (FALSE))
                    GFXidle ();
            } else
                processinput (TRUE);
        }
        if (Erun)
            TXTupdate (), Erun = FALSE;
    }
eop:
#ifdef PARANOID
    GFXterm ();
    TXTterm ();
    Iterm ();
    Dterm ();
    Sterm ();
    Eterm ();
    Pterm ();
    Tterm ();
    IOterm ();
    Cterm ();
    Gterm ();
    Mterm ();
    FD_CLR (Gxfd, &inputfds);
    term ();
#endif
    printusage ();
    exit (0);
}

#else

HANDLE hinstance, hprevinstance;

int PASCAL WinMain (HANDLE hInstance, HANDLE hPrevInstance,
        LPSTR lpCmdLine, int nCmdShow) {
    Tobj co;
    Psrc_t src;
    char MSNEAR *argvs[] = { "lefty", "" };
    char **argv = &argvs[0];
    int argc = 1;

#ifdef STATS
    stime = time (NULL);
#endif

    hinstance = hInstance;
    hprevinstance = hPrevInstance;
    idlerunmode = 0;
    exprstr = NULL;
    fp = NULL;
    init (NULL);
    Ginit ();

    Eerrlevel = 1;
    Estackdepth = 2;
    Eshowbody = 1;
    Eshowcalls = 1;

    argv++, argc--;
    processstr (leftyoptions);
    processstr ((char *) lpCmdLine);

    if (setjmp (exitljbuf))
        goto eop;

#ifndef MSSUCKS
    FD_SET (Gdata.fd, &inputfds);
#endif

    Cinit ();
    IOinit ();
    Minit (GFXprune);
    Tinit ();
    Pinit ();
    Einit ();
    Sinit ();
    Dinit ();
    Iinit ();
    TXTinit (txtcoords);
    GFXinit ();

    if (exprstr) {
        src.flag = CHARSRC, src.s = exprstr, src.fp = NULL;
        src.tok = -1, src.lnum = 1;
        while ((co = Punit (&src)))
            Eunit (co);
    }
    if (fp) {
        src.flag = FILESRC, src.s = NULL, src.fp = fp;
        src.tok = -1, src.lnum = 1;
        while ((co = Punit (&src)))
            Eunit (co);
    }
    if (endflag)
        goto eop;

    TXTupdate ();

    Gneedredraw = FALSE;
    for (;;) {
        if (Gbuttonsdown > 0)
            GFXmove (), Gprocessevents (FALSE, G_ONEEVENT);
        else {
            if (Gneedredraw)
                GFXredraw (), Gneedredraw = FALSE;
            if (Mcouldgc) {
                if (!processinput (FALSE))
                    Mdogc (M_GCINCR);
            } else if (idlerunmode) {
                if (!processinput (FALSE))
                    GFXidle ();
            } else
                processinput (TRUE);
        }
        if (Erun)
            TXTupdate (), Erun = FALSE;
    }

eop:
#define PARANOID
#ifdef PARANOID
    GFXterm ();
    TXTterm ();
    Iterm ();
    Dterm ();
    Sterm ();
    Eterm ();
    Pterm ();
    Tterm ();
    Mterm ();
    IOterm ();
    Cterm ();
    Gterm ();
    FD_CLR (Gdata.fd, &inputfds);
    term ();
#endif
    printusage ();
    exit (0);
}

#endif

#ifndef HAVEMSWIN
static struct timeval longwait = { 1000, 0 };
static struct timeval zerowait = { 0, 0 };
#endif

static int processinput (int waitflag) {
    fd_set fdset;
    struct timeval *tzp;
    int n, rtn;
    long ioi;

    rtn = 0;
    while (Gprocessevents (FALSE, G_MANYEVENTS))
        rtn = 1;
#ifndef HAVEMSWIN
    tzp = (waitflag && !rtn) ? &longwait : &zerowait;
    fdset = inputfds;
    if ((n = select (FD_SETSIZE, &fdset, NULL, NULL, tzp)) <= 0)
        return rtn;
    rtn = 1;
    if (FD_ISSET (Gxfd, &fdset))
        Gprocessevents (TRUE, G_MANYEVENTS), n--;
    if (!n)
        return rtn;
    Gsync ();
    for (ioi = 0; n > 0 && ioi < ion; ioi++)
        if (iop[ioi].inuse && FD_ISSET (fileno (iop[ioi].ifp), &fdset))
            GFXmonitorfile (ioi), n--;
#endif
    return rtn;
}

static void processstr (char *buf) {
    char *words[100];
    char *s, *s1;
    int i;

    if (!(s = buf) || *s == 0)
        return;
    s = strdup (s);
    for (i = 0, s1 = s; *s1; ) {
        for (; *s1 && *s1 == ' '; s1++)
            ;
        if (!*s1)
            break;
        words[i++] = s1;
        for (; *s1 && *s1 != ' '; s1++)
            ;
        if (*s1)
            *s1 = '\000', s1++;
    }
    words[i] = NULL;
#ifdef HAVEMSWIN
    for (i = 0; words[i]; i++) {
        if (words[i][0] == '"') {
            words[i] = words[i] + 1;
            if (words[i][strlen (words[i]) - 1] == '"')
                words[i][strlen (words[i]) - 1] = 0;
        }
    }
#endif
    processargs (i, words);
    free (s);
}

static void processenv (void) {
    char *words[100];
    char *s, *s1;
    int i;

    if (!(s = getenv ("LEFTYOPTIONS")))
        return;
    s = strdup (s);
    for (i = 0, s1 = s; *s1; ) {
        for (; *s1 && *s1 == ' '; s1++)
            ;
        if (!*s1)
            break;
        words[i++] = s1;
        for (; *s1 && *s1 != ' '; s1++)
            ;
        if (*s1)
            *s1 = '\000', s1++;
    }
    words[i] = NULL;
#ifdef HAVEMSWIN
    for (i = 0; words[i]; i++) {
        if (words[i][0] == '"') {
            words[i] = words[i] + 1;
            if (words[i][strlen (words[i]) - 1] == '"')
                words[i][strlen (words[i]) - 1] = 0;
        }
    }
#endif
    processargs (i, words);
    free (s);
}

static void processargs (int argc, char *argv[]) {
    while (argc) {
        if (Strcmp (argv[0], "-x") == 0)
            endflag = TRUE;
        else if (Strcmp (argv[0], "-e") == 0)
            exprstr = argv[1], argv++, argc--;
        else if (Strcmp (argv[0], "-el") == 0)
            Eerrlevel = atoi (argv[1]), argv++, argc--;
        else if (Strcmp (argv[0], "-sd") == 0)
            Estackdepth = atoi (argv[1]), argv++, argc--;
        else if (Strcmp (argv[0], "-sb") == 0)
            Eshowbody = atoi (argv[1]), argv++, argc--;
        else if (Strcmp (argv[0], "-sc") == 0)
            Eshowcalls = atoi (argv[1]), argv++, argc--;
        else if (Strcmp (argv[0], "-df") == 0)
            Gdefaultfont = argv[1], argv++, argc--;
        else if (Strcmp (argv[0], "-w") == 0)
            warnflag = TRUE;
        else if (Strcmp (argv[0], "-ps") == 0)
            Gpscanvasname = argv[1], argv++, argc--;
        else if (Strcmp (argv[0], "-V") == 0)
            fprintf (stderr, "lefty version %s\n", LEFTYVERSION);
        else if (Strcmp (argv[0], "-") == 0)
            fp = stdin;
        else {
            if ((fp = fopen (argv[0], "r")) == NULL)
                panic (POS, "main", "cannot open input file: %s", argv[0]);
        }
        argv++, argc--;
    }
    if (Eerrlevel > 1)
        Gerrflag = TRUE;
}

static void printusage (void) {
#ifdef STATS
    struct rusage ru;

    getrusage (RUSAGE_SELF, &ru);
    Mreport ();
    printf ("user   time %13.3lf\n",
            ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1000000.0);
    printf ("system time %13.3lf\n",
            ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1000000.0);
    printf ("resident size  %10d\n", ru.ru_maxrss * getpagesize ());
    printf ("input            %8d\n", ru.ru_inblock);
    printf ("output           %8d\n", ru.ru_oublock);
    printf ("socket msgs sent %8d\n", ru.ru_msgsnd);
    printf ("socket msgs rcvd %8d\n", ru.ru_msgrcv);
    printf ("real time        %8d\n", time (NULL) - stime);
    fflush (stdout);
#endif
}
