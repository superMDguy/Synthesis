/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#include <windows.h>
#include <ddeml.h>
#include "iodde.h"

static DWORD ddeid;
static HSZ namehsz;
static char *namestr;
static int convi;

#define CONV2DDESIZE 20
static iodde_t *conv2dde[CONV2DDESIZE];

static int findslot (int, iodde_t *, HCONV);

HDDEDATA CALLBACK __export
        IODDEcallback (UINT, UINT, HCONV, HSZ, HSZ, HDDEDATA, DWORD, DWORD);

void IODDEinit (char *name) {
    DdeInitialize (&ddeid, (PFNCALLBACK) IODDEcallback,
            APPCMD_FILTERINITS | APPCLASS_STANDARD | CBF_FAIL_SELFCONNECTIONS,
            0L);
    namehsz = DdeCreateStringHandle (ddeid, (namestr = name), NULL);
    DdeNameService (ddeid, namehsz, NULL, DNS_REGISTER | DNS_FILTERON);
}

void IODDEterm (void) {
    long i;

    for (i = 0; i < CONV2DDESIZE; i++)
        if (conv2dde[i])
            IODDEclose (conv2dde[i]);
    DdeNameService (ddeid, namehsz, NULL, DNS_UNREGISTER);
    DdeFreeStringHandle (ddeid, namehsz);
    DdeUninitialize (ddeid);
}

int IODDEopen (iodde_t *p, char *name, int flag) {
    MSG msg;
    UINT e;
    int i, count = 0;

    if ((i = findslot (1, NULL, 0)) == -1) {
        MessageBox (NULL, "out of conv2dde slots", "iodde lib", MB_APPLMODAL);
        return -1;
    }
    conv2dde[i] = p;

    convi = i;
    p->namehsz = DdeCreateStringHandle (ddeid, name, NULL);
    p->enabled = FALSE;
    p->lastop = IODDE_READ;
    p->bufi = p->bufj = 2; /* bytes 0 and 1 hold the string size */
    p->oh = p->ih = 0;
    if (flag == 1) {
        while (!(p->oh = DdeConnect (ddeid, p->namehsz, namehsz, NULL))) {
            e = DdeGetLastError (ddeid);
            if (count++ > 1000) {
                MessageBox (NULL, "cannot connect", "iodde lib", MB_APPLMODAL);
                return -1;
            }
        }
        count = 0;
        while (!(DdeClientTransaction ("a", 1, p->oh, p->namehsz, 1,
                XTYP_POKE, 30000, 0))) {
            if ((e = DdeGetLastError (ddeid)) != DMLERR_BUSY || count++ > 1000)
                return -1;
        }
    }
    while (!p->ih || p->bufi == p->bufj) {
        if (!GetMessage(&msg, (HWND) NULL, 0, 0)) {
            MessageBox (NULL, "exit code in GetMessage", "iodde lib",
                MB_APPLMODAL);
            exit (1);
        }
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    p->bufi = p->bufj = 2; /* reset after initial message */
    if (flag == 2) {
        while (!(p->oh = DdeConnect (ddeid, p->namehsz, namehsz, NULL))) {
            e = DdeGetLastError (ddeid);
            if (count++ > 1000) {
                MessageBox (NULL, "cannot connect", "iodde lib", MB_APPLMODAL);
                return -1;
            }
        }
        count = 0;
        while (!(DdeClientTransaction ("a", 1, p->oh, p->namehsz, 1,
                XTYP_POKE, 30000, 0))) {
            if ((e = DdeGetLastError (ddeid)) != DMLERR_BUSY || count++ > 1000)
                return -1;
        }
    }
    convi = -1;
    p->lastop = IODDE_WRITE;
    DdeEnableCallback (ddeid, p->ih, EC_DISABLE);
    return 0;
}

int IODDEclose (iodde_t *p) {
    int i;

    if ((i = findslot (1, p, 0)) == -1)
        return -1;
    DdeDisconnect (p->oh);
    DdeFreeStringHandle (ddeid, p->namehsz);
    conv2dde[i] = NULL;
    return 0;
}

long IODDEread (iodde_t *p, char *bufp, long bufn) {
    long i;

    if (p->lastop == IODDE_WRITE && p->bufj > 2)
        IODDEflush (p);
    p->lastop = IODDE_READ;
    if (bufn == 0)
        return 0;
    i = 0;
    for (;;) {
        while (i < bufn && p->bufi < p->bufj)
            bufp[i++] = p->buf[p->bufi++];
        if (i > 0 || !IODDEsuck (p))
            break;
    }
    return i;
}

long IODDEreadline (iodde_t *p, char *bufp, long bufn) {
    long i;

    if (p->lastop == IODDE_WRITE && p->bufj > 2)
        IODDEflush (p);
    p->lastop = IODDE_READ;
    if (bufn == 0)
        return 0;
    i = 0;
    for (;;) {
        while (i < bufn - 1 && p->bufi < p->bufj) {
            bufp[i] = p->buf[p->bufi++];
            if (bufp[i++] == '\n') {
                bufp[i] = 0;
                return i;
            }
        }
        if (i == bufn - 1 || !IODDEsuck (p))
            break;
    }
    return i;
}

long IODDEwrite (iodde_t *p, char *bufp, long bufn) {
    long i;

    if (p->lastop == IODDE_READ && p->bufj > 2)
        p->bufi = p->bufj = 2;
    p->lastop = IODDE_WRITE;
    i = 0;
    for (;;) {
        while (i < bufn && p->bufj < IODDEBUFSIZE)
            p->buf[p->bufj++] = bufp[i++];
        if (p->bufj == IODDEBUFSIZE)
            if (!IODDEflush (p))
                return 0;
        if (i == bufn)
            break;
    }
    return bufn;
}

long IODDEwriteline (iodde_t *p, char *bufp, long bufn) {
    long i;

    if (p->lastop == IODDE_READ && p->bufj > 2)
        p->bufi = p->bufj = 2;
    p->lastop = IODDE_WRITE;
    i = 0;
    for (;;) {
        while (i < bufn && p->bufj < IODDEBUFSIZE)
            p->buf[p->bufj++] = bufp[i++];
        if (p->bufj == IODDEBUFSIZE)
            if (!IODDEflush (p))
                return 0;
        if (i == bufn)
            break;
    }
    p->buf[p->bufj++] = '\n';
    if (p->bufj == IODDEBUFSIZE)
        if (!IODDEflush (p))
            return 0;
    return bufn;
}

long IODDEflush (iodde_t *p) {
    HDDEDATA handle;
    UINT e;
    int count = 0;

    if (p->lastop != IODDE_WRITE || p->bufj == 2)
        return 0;
    /* bytes 0 and 1 hold the size */
    p->buf[0] = p->bufj / 256, p->buf[1] = p->bufj % 256;
    handle = DdeCreateDataHandle (ddeid, p->buf, p->bufj, 0, p->namehsz, 1, 0);
    while (!(DdeClientTransaction (handle, -1, p->oh, p->namehsz, 1,
                XTYP_POKE, 30000, 0))) {
        e = DdeGetLastError (ddeid);
        if (count++ > 10) {
            p->bufi = p->bufj = 2;
            return 0;
        }
    }
    p->bufi = p->bufj = 2;
    return 1;
}

long IODDEsuck (iodde_t *p) {
    MSG msg;

    if (!p->ih || p->bufi != p->bufj || p->lastop != IODDE_READ)
        return 0;
    if (!p->enabled)
        DdeEnableCallback (ddeid, p->ih, EC_ENABLEALL);
    while (p->ih && p->bufi == p->bufj) {
        if (!GetMessage(&msg, (HWND) NULL, (UINT) NULL, (UINT) NULL)) {
            MessageBox (NULL, "1 exit code in GetMessage", "iodde lib",
                MB_APPLMODAL);
            exit (1);
        }
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    if (p->bufi == p->bufj)
        return 0;
    return 1;
}

static int findslot (int flag, iodde_t *p, HCONV conv) {
    int i;

    if (flag == 1) { /* search by pointer */
        for (i = 0; i < CONV2DDESIZE; i++)
            if (conv2dde[i] == p)
                return i;
    } else { /* search by conv */
        for (i = 0; i < CONV2DDESIZE; i++)
            if (conv2dde[i] && conv2dde[i]->ih == conv)
                return i;
    }
    return -1;
}

HDDEDATA CALLBACK __export IODDEcallback (UINT type, UINT fmt, HCONV hconv,
        HSZ hsz1, HSZ hsz2, HDDEDATA hdata, DWORD data1, DWORD data2) {
    iodde_t *p;
    int i;

    switch (type) {
    case XTYP_CONNECT:
        if (namehsz == hsz2)
            return TRUE;
        return FALSE;
    case XTYP_CONNECT_CONFIRM:
        if (namehsz == hsz2 && convi != -1)
            conv2dde[convi]->ih = hconv;
        return TRUE;
    case XTYP_POKE:
        if ((i = findslot (2, NULL, hconv)) == -1)
            return FALSE;
        p = conv2dde[i];
        if (p->bufi != p->bufj || p->lastop != IODDE_READ)
            return DDE_FBUSY;
        if (!DdeGetData (hdata, &p->buf[0], IODDEBUFSIZE, 0)) {
            MessageBox (NULL, "get data failed", "iodde lib", MB_APPLMODAL);
            DdeFreeDataHandle (hdata);
            return FALSE;
        }
        DdeFreeDataHandle (hdata);
        if (p->enabled)
            DdeEnableCallback (ddeid, p->ih, EC_DISABLE);
        p->bufi = 2;
        p->bufj = (int) p->buf[0] * 256 + (unsigned char) p->buf[1];
        return DDE_FACK;
    case XTYP_DISCONNECT:
        if ((i = findslot (2, NULL, hconv)) == -1)
            return FALSE;
        p = conv2dde[i];
        p->ih = 0;
        return DDE_FACK;
    default:
        return 0;
    }
}
