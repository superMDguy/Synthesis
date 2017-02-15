/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#include "common.h"

int warnflag;
char *leftypath, *leftyoptions, *shellpath;
jmp_buf exitljbuf;
int idlerunmode;
fd_set inputfds;

#if !defined(HAVEMSWIN) || defined(HAVEPOSIX)
#define WINSYS "LEFTYWINSYS=X11"
#define PATHDEL '/'
#define PATHSEP ':'
#else
#define WINSYS "LEFTYWINSYS=mswin"
#define PATHDEL '\\'
#define PATHSEP ';'
#endif

#ifdef HAVEMSWIN
extern int Gnocallbacks;
#else
static int Gnocallbacks;
#endif

static char *pathp;
#define PATHINCR 1024
#define PATHSIZE sizeof (char)
static char *cmdp;
#define CMDINCR 4096
#define CMDSIZE sizeof (char)

static char *lpathp;

int init (char *aout) {
    int i, j;

    if (!(pathp = malloc (PATHINCR * PATHSIZE)))
        panic (POS, "init", "pathp malloc failed");
    if (!(cmdp = malloc (CMDINCR * CMDSIZE)))
        panic (POS, "init", "cmdp malloc failed");
    shellpath = getenv ("PATH");
    if (!(leftypath = getenv ("LEFTYPATH"))) {
#if defined(HAVEMSWIN) && !defined(HAVEPOSIX)
        extern HANDLE hinstance;
        char buf[260];

        GetModuleFileName (hinstance, buf, 260);
        aout = buf;
#endif
        for (i = strlen (aout) - 1; i >= 0; i--) {
            if (aout[i] == PATHDEL) {
                if (i - 3 < 0 || tolower (aout[i - 3]) != 'b' ||
                        tolower (aout[i - 2]) != 'i' ||
                        tolower (aout[i - 1]) != 'n')
                    break;
                if (!(lpathp = malloc ((i + 7) * PATHSIZE)))
                    panic (POS, "init", "lpathp malloc failed");
                for (j = 0; j <= i; j++)
                    lpathp[j] = aout[j];
                lpathp[i - 3] = 'l';
                lpathp[i - 2] = 'i';
                lpathp[i - 1] = 'b';
                lpathp[j + 0] = 'l';
                lpathp[j + 1] = 'e';
                lpathp[j + 2] = 'f';
                lpathp[j + 3] = 't';
                lpathp[j + 4] = 'y';
                lpathp[j + 5] = 0;
                leftypath = lpathp;
                break;
            }
        }
        if (!leftypath)
            leftypath = LEFTYPATH;
    }
    if (!(leftyoptions = getenv ("LEFTYOPTIONS")))
        leftyoptions = "";
    putenv (WINSYS);
    return 0;
}

void term (void) {
    if (lpathp)
        free (lpathp);
    if (pathp)
        free (pathp);
    if (cmdp)
        free (cmdp);
}

/* given a file name, it looks for this file in LEFTYPATH
   (and if flag == TRUE in PATH)

   returns the first occurance of that file or NULL
*/
char *buildpath (char *file, int flag) {
    struct stat statbuf;
    char *s1, *s2;
    int mode, pathi, i;

    if (file && file[0] && (file[0] == '.' || file[0] == PATHDEL))
        return file;
#if !defined(HAVEMSWIN) || defined(HAVEPOSIX)
    mode = S_IRUSR | (flag ? S_IXUSR : 0);
#else
    mode = ~0;
#endif
    for (i = 0; i < 2; i++) {
        if (i == 0)
            s1 = leftypath;
        else
            s1 = shellpath;
        while (*s1) {
            pathi = 0;
            while (*s1 && *s1 != PATHSEP)
                if (pathi < PATHINCR)
                    pathp[pathi++] = *s1++;
#ifndef HAVEMSWIN
            if (pathi + 3 + strlen (file) >= PATHINCR)
                continue;
#else
            if (pathi + 7 + strlen (file) >= PATHINCR)
                continue;
#endif
            if (*s1)
                s1++;
            pathp[pathi++] = PATHDEL;
            for (s2 = file; *s2; s2++)
                pathp[pathi++] = *s2;
#ifdef HAVEMSWIN
            if (flag) {
                pathp[pathi++] = '.';
                pathp[pathi++] = 'e';
                pathp[pathi++] = 'x';
                pathp[pathi++] = 'e';
            }
#endif
            pathp[pathi] = '\000';
            if (stat (pathp, &statbuf) == 0 && (statbuf.st_mode & mode))
                return pathp;
        }
    }
    return NULL;
}

/* given a file name (path) and an optional format (fmt)
   it builds a shell command.

   %e is replaced by the command path
   %i      ...       the input channel descriptor
   %o      ...       the output channel descriptor
   %h      ...       the hostname

   returns the complete command string or NULL
*/
char *buildcommand (char *path, char *host, int infd, int outfd, char *fmt) {
    char buf[10];
    char *s1, *s2;
    int bufi;

    cmdp[0] = '\000';
    for (bufi = 0, s1 = fmt; *s1; s1++) {
        if (*s1 == '%') {
            if (*(s1 + 1) == 'e') {
                s1++;
                if (bufi + strlen (path) >= CMDINCR)
                    return NULL;
                for (s2 = path; *s2; s2++)
                    cmdp[bufi++] = *s2;
                continue;
            } else if (*(s1 + 1) == 'i') {
                if (infd == -1)
                    buf[0] = '%', buf[1] = 'd', buf[2] = '\000';
                else
                    sprintf (buf, "%d", infd);
                s1++;
                if (bufi + strlen (buf) >= CMDINCR)
                    return NULL;
                for (s2 = buf; *s2; s2++)
                    cmdp[bufi++] = *s2;
                continue;
            } else if (*(s1 + 1) == 'o') {
                if (outfd == -1)
                    buf[0] = '%', buf[1] = 'd', buf[2] = '\000';
                else
                    sprintf (buf, "%d", outfd);
                s1++;
                if (bufi + strlen (buf) >= CMDINCR)
                    return NULL;
                for (s2 = buf; *s2; s2++)
                    cmdp[bufi++] = *s2;
                continue;
            } else if (*(s1 + 1) == 'h') {
                s1++;
                if (bufi + strlen (host) >= CMDINCR)
                    return NULL;
                for (s2 = host; *s2; s2++)
                    cmdp[bufi++] = *s2;
                continue;
            }
        }
        if (bufi + 1 >= CMDINCR)
            return NULL;
        cmdp[bufi++] = *s1;
    }
    if (bufi + 1 >= CMDINCR)
        return NULL;
    cmdp[bufi] = '\000';
    return &cmdp[0];
}

/* varargs function for printing a warning */
void warning (char *file, int line, char *func, char *fmt, ...) {
    va_list args;

#if !defined(HAVEMSWIN) || defined(HAVEPOSIX)
    if (!warnflag)
        return;

    va_start(args, fmt);
    Gnocallbacks = TRUE;
    fprintf (stderr, "warning: (file %s, line %d, func %s) ", file, line, func);
    vfprintf (stderr, fmt, args);
    fprintf (stderr, "\n");
    Gnocallbacks = FALSE;
    va_end(args);
#else
    char buf[256];

    if (!warnflag)
        return;

    va_start(args, fmt);
    vsprintf (buf, fmt, args);
    Gnocallbacks = TRUE;
    MessageBox ((HWND) NULL, buf, "Lefty Warning", MB_APPLMODAL);
    Gnocallbacks = FALSE;
    va_end(args);
#endif
}

/* varargs function for printing an error message and aborting */
void panic (char *file, int line, char *func, char *fmt, ...) {
    va_list args;

#if !defined(HAVEMSWIN) || defined(HAVEPOSIX)
    va_start(args, fmt);
    Gnocallbacks = TRUE;
    fprintf (stderr, "panic: (file %s, line %d, func %s) ", file, line, func);
    vfprintf (stderr, fmt, args);
    fprintf (stderr, "\n");
    fflush (stdout);
    Gnocallbacks = FALSE;
    va_end(args);
#else
    char buf[256];

    va_start(args, fmt);
    vsprintf (buf, fmt, args);
    Gnocallbacks = TRUE;
    MessageBox ((HWND) NULL, buf, "Lefty PANIC", MB_APPLMODAL);
    Gnocallbacks = FALSE;
    va_end(args);
#endif
    abort ();
}

/* varargs function for printing an error message, and the
   error message corresponding to errno and aborting
*/
void panic2 (char *file, int line, char *func, char *fmt, ...) {
    va_list args;

#if !defined(HAVEMSWIN) || defined(HAVEPOSIX)
    va_start(args, fmt);
    Gnocallbacks = TRUE;
    fprintf (stderr, "panic: (file %s, line %d, func %s) ", file, line, func);
    vfprintf (stderr, fmt, args);
    fprintf (stderr, "\n");
#if !defined(HAVEMSWIN)
    perror ("");
#endif
    fflush (stdout);
    Gnocallbacks = FALSE;
    va_end(args);
#else
    char buf[256];

    va_start(args, fmt);
    vsprintf (buf, fmt, args);
    Gnocallbacks = TRUE;
    MessageBox ((HWND) NULL, buf, "Lefty PANIC", MB_APPLMODAL);
    Gnocallbacks = FALSE;
    va_end(args);
#endif
    abort ();
}

#if defined(HAVEMSWIN)
int printf (const char *fmt, ...) {
    va_list args;
    char buf[256];
    int l;

    va_start(args, fmt);
    vsprintf (buf, fmt, args);
    l = strlen (buf);
    if (buf[l - 1] == '\n')
        buf[l - 1] = 0;
    if (buf[0]) {
        Gnocallbacks = TRUE;
        MessageBox ((HWND) NULL, buf, "Lefty printf", MB_APPLMODAL);
        Gnocallbacks = FALSE;
    }
    va_end(args);
}
#endif
