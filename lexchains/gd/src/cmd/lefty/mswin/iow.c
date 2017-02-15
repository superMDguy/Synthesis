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
#include "io.h"
#include "mem.h"

io_t *iop;
int ion;

static char *shell;
static char *shbname;
static DWORD ddeid;
static HSZ readhsz;

void IOinit (void) {
    struct stat statbuf;
    char *sh;
    int ioi;

    if (!(shell = getenv ("SHELL")))
        shell = "/bin/sh";
    shbname = shell + strlen (shell) - 1;
    while (shbname >= shell && *shbname != '/')
        shbname--;
    if (*shbname == '/')
        shbname++;
    iop = Marrayalloc ((long) IOINCR * IOSIZE);
    ion = IOINCR;
    for (ioi = 0; ioi < ion; ioi++)
        iop[ioi].inuse = FALSE;
    for (ioi = 0; ioi < ion; ioi++)
        if (fstat (ioi, &statbuf) == 0) {
            iop[ioi].inuse = TRUE;
            iop[ioi].type = IO_FILE;
            iop[ioi].u.f.ifp = iop[ioi].u.f.ofp = fdopen (ioi, "r+");
        }
    Gnocallbacks = 1;
    IODDEinit ("leftydde");
    Gnocallbacks = 0;
}

void IOterm (void) {
    int ioi;

    for (ioi = 0; ioi < ion; ioi++)
        if (iop[ioi].inuse)
            IOclose (ioi, NULL);
    Gnocallbacks = 1;
    IODDEterm ();
    Gnocallbacks = 0;
    Marrayfree (iop), iop = NULL, ion = 0;
}

int IOopen (char *kind, char *name, char *mode, char *fmt) {
    io_t *p;
    iotype_t type;
    char *path, *command, *name2;
    char fmt2[100];
    UINT handle;
    int i;
    int count = 0;

    if (Strcmp (kind, "file") == 0)
        type = IO_FILE;
    else if (Strcmp (kind, "pipe") == 0)
        type = IO_PIPE;
    else
        return -1;

    for (i = 0; i < ion; i++)
        if (!iop[i].inuse)
            break;
    if (i == ion) {
        iop = Marraygrow (iop, (long) (ion + IOINCR) * IOSIZE);
        for (i = ion + IOINCR - 1; i >= ion; i--)
            iop[i].inuse = FALSE;
        i++, ion += IOINCR;
    }
    p = &iop[i];
    p->type = type;
    switch (type) {
    case IO_FILE:
        if (!(p->u.f.ifp = p->u.f.ofp = fopen (name, mode))) {
            path = buildpath (name, FALSE);
            if (!path || !(p->u.f.ifp = p->u.f.ofp = fopen (path, mode)))
                return -1;
        }
        break;
    case IO_PIPE:
        for (name2 = name + strlen (name) - 1; name2 > name; name2--)
            if (*(name2 - 1) == '\\')
                break;
        if (!fmt)
            fmt = "%e";
        sprintf (fmt2, "%s %s %s", fmt, "leftydde", name2);
        if (!(path = buildpath (name, TRUE)) ||
                !(command = buildcommand (path, NULL, -1, -1, fmt2)))
            return -1;
        handle = WinExec (command, SW_SHOW);
        Gnocallbacks = 1;
        if (IODDEopen (&p->u.p, name2, 1) == -1) {
            Gnocallbacks = 0;
            return -1;
        }
        Gnocallbacks = 0;
        break;
    }
    p->inuse = TRUE;
    return i;
}

int IOclose (int ioi, char *action) {
    io_t *p;

    if (ioi < 0 || ioi >= ion || !iop[ioi].inuse)
        return -1;

    p = &iop[ioi];
    switch (p->type) {
    case IO_FILE:
        fclose (p->u.f.ifp);
        fclose (p->u.f.ofp);
        break;
    case IO_PIPE:
        Gnocallbacks = 1;
        IODDEclose (&p->u.p);
        Gnocallbacks = 0;
        break;
    }
    p->inuse = FALSE;
    return 0;
}

int IOreadline (int ioi, char *bufp, int bufn) {
    io_t *p;
    int l;

    if (ioi < 0 || ioi >= ion || !iop[ioi].inuse)
        return -1;

    p = &iop[ioi];
    switch (p->type) {
    case IO_FILE:
        if (fgets (bufp, bufn, p->u.f.ifp) == NULL)
            return -1;
        break;
    case IO_PIPE:
        Gnocallbacks = 1;
        l = IODDEreadline (&p->u.p, bufp, bufn - 1);
        Gnocallbacks = 0;
        if (l <= 0)
            return -1;
        bufp[l] = 0;
        break;
    }

    l = strlen (bufp) - 1;
    while (bufp[l] == '\n' || bufp[l] == '\r')
        bufp[l--] = '\000';
    return 0;
}

int IOread (int ioi, char *bufp, int bufn) {
    io_t *p;
    int l;

    if (ioi < 0 || ioi >= ion || !iop[ioi].inuse)
        return -1;

    p = &iop[ioi];
    switch (p->type) {
    case IO_FILE:
        if ((l = read (fileno (p->u.f.ifp), bufp, bufn - 1)) <= 0)
            return -1;
        break;
    case IO_PIPE:
        Gnocallbacks = 1;
        l = IODDEread (&p->u.p, bufp, bufn - 1);
        Gnocallbacks = 0;
        if (l <= 0)
            return -1;
        break;
    }

    bufp[l] = '\000';
    return 0;
}

int IOwriteline (int ioi, char *bufp) {
    io_t *p;

    if (ioi < 0 || ioi >= ion || !iop[ioi].inuse)
        return -1;

    p = &iop[ioi];
    switch (p->type) {
    case IO_FILE:
        if (fputs (bufp, p->u.f.ofp) == EOF || fputs ("\n", p->u.f.ofp) == EOF)
            return -1;
        fflush (p->u.f.ofp);
        break;
    case IO_PIPE:
        Gnocallbacks = 1;
        IODDEwriteline (&p->u.p, bufp, strlen (bufp));
        Gnocallbacks = 0;
        break;
    }
    return 0;
}
