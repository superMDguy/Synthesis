/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _COMMON_H
#define _COMMON_H
#ifdef HAVECS
#include <ast.h>
#else
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#ifdef HAVEVARARGS
#include <varargs.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#ifndef HAVEMSWIN
#include <sys/param.h>
#endif
#endif

#if !defined(HAVEMSWIN) || defined(HAVEPOSIX)
#include <signal.h>
#include <sys/wait.h>
#endif
#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <setjmp.h>
#ifndef HAVEMSWIN
#include <termio.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif
#ifdef STATS
#include <sys/resource.h>
#endif

#ifdef HAVEMSWIN
#include <windows.h>
#include <commdlg.h>
#include <ddeml.h>
#include <print.h>
#include <malloc.h>
#define MSNEAR __near
#define MSSUCKS
typedef int fd_set[1];
#define FD_SET(a, b)
#define FD_CLR(a, b)
#define FD_ISSET(a, b) 0
#define REALSTRCMP
#else
#define MSNEAR
#endif

#define POS __FILE__, __LINE__

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

#ifndef SUCCEEDED
#define SUCCEEDED 1
#define FAILED    0
#endif

#define CHARSRC 0
#define FILESRC 1

#define M_PI 3.14159265358979323846

#ifndef REALSTRCMP
#define Strcmp(s1, s2) ( \
    *(s1) == *(s2) ? ( \
        (*s1) ? strcmp ((s1) + 1, (s2) + 1) : 0 \
    ) : (*(s1) < *(s2) ? -1 : 1) \
)
#else
#define Strcmp(s1, s2) strcmp ((s1), (s2))
#endif

extern int warnflag;
extern char *leftypath, *leftyoptions, *shellpath;
extern jmp_buf exitljbuf;
extern int idlerunmode;
extern fd_set inputfds;

int init (char *);
void term (void);
char *buildpath (char *, int);
char *buildcommand (char *, char *, int, int, char *);
void warning (char *, int, char *, char *, ...);
void panic (char *, int, char *, char *, ...);
void panic2 (char *, int, char *, char *, ...);
#endif /* _COMMON_H */
