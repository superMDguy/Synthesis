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
#include "gcommon.h"
#include "mem.h"

#define WQU widget->u.q

int GQcreatewidget (Gwidget_t *parent, Gwidget_t *widget,
        int attrn, Gwattr_t *attrp) {
    DWORD wflags;
    PIXsize_t ps;
    int ai;

    wflags = WS_OVERLAPPEDWINDOW;
    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRUSERDATA:
            widget->udata = attrp[ai].u.u;
            break;
        default:
            Gerr (POS, G_ERRBADATTRID, attrp[ai].id);
            return -1;
        }
    }
    if (!(widget->w = CreateWindow ("QueryClass", "Lefty", wflags,
            0, 0, 300, 100, (HWND) NULL, (HMENU) NULL, hinstance, NULL))) {
        Gerr (POS, G_ERRCANNOTCREATE);
        return -1;
    }
    ShowWindow (widget->w, SW_HIDE);
    UpdateWindow (widget->w);
    wflags = WS_CHILDWINDOW | ES_MULTILINE | WS_HSCROLL | WS_VSCROLL;
    Gadjustwrect (widget, &ps);
    if (!(WQU->w = CreateWindow ("EDIT", "", wflags, 0, 0, ps.x, ps.y,
            widget->w, (HMENU) (widget - &Gwidgets[0]), hinstance, NULL))) {
        Gerr (POS, G_ERRCANNOTCREATE);
        return -1;
    };
    ShowWindow (WQU->w, SW_HIDE);
    UpdateWindow (WQU->w);
    return 0;
}

int GQsetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
    int ai;

    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRUSERDATA:
            widget->udata = attrp[ai].u.u;
            break;
        default:
            Gerr (POS, G_ERRBADATTRID, attrp[ai].id);
            return -1;
        }
    }
    return 0;
}

int GQgetwidgetattr (Gwidget_t *widget, int attrn, Gwattr_t *attrp) {
    int ai;

    for (ai = 0; ai < attrn; ai++) {
        switch (attrp[ai].id) {
        case G_ATTRUSERDATA:
            attrp[ai].u.u = widget->udata;
            break;
        default:
            Gerr (POS, G_ERRBADATTRID, attrp[ai].id);
            return -1;
        }
    }
    return 0;
}

int GQdestroywidget (Gwidget_t *widget) {
    DestroyWindow (widget->w);
    return 0;
}

int GQqueryask (Gwidget_t *widget, char *prompt,
        char *responsep, int responsen) {
    MSG msg;
    POINT p;

    GetCursorPos (&p);
    p.x -= 10, p.y -= 10;
    if (p.x < 0)
        p.x = 0;
    if (p.y < 0)
        p.y = 0;
    strcpy (&Gbufp[0], "Lefty: ");
    strcat (&Gbufp[0], prompt);
    SetWindowText (widget->w, &Gbufp[0]);
    SetWindowText (WQU->w, "");
    SetWindowPos (widget->w, (HWND) NULL, p.x, p.y, 0, 0,
            SWP_NOZORDER | SWP_NOSIZE);
    ShowWindow (widget->w, SW_SHOW);
    UpdateWindow (widget->w);
    ShowWindow (WQU->w, SW_SHOW);
    UpdateWindow (WQU->w);
    SetFocus (WQU->w);
    WQU->state = 2;
    while (WQU->state) {
        if (!GetMessage(&msg, widget->w, (UINT) NULL, (UINT) NULL))
            panic (POS, "GQqueryask", "exit code in GetMessage");
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    Gpopdownflag = TRUE;
    strncpy (responsep, &Gbufp[0], responsen);
    if (responsep[0] && responsep[strlen(responsep) - 1] == '\n')
        responsep[strlen(responsep) - 1] = 0;
    ShowWindow (widget->w, SW_HIDE);
    UpdateWindow (widget->w);
    ShowWindow (WQU->w, SW_HIDE);
    UpdateWindow (WQU->w);
    return 0;
}
