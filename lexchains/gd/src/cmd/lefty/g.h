/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#pragma prototyped
/* Lefteris Koutsofios - AT&T Bell Laboratories */

#ifndef _G_H
#define _G_H
#ifndef HAVEMSWIN
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/cursorfont.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#endif

/* general coordinate structures */

typedef struct Gxy_t {
    double x, y;
} Gxy_t;
typedef Gxy_t Gpoint_t;
typedef Gxy_t Gsize_t;
typedef struct Grect_t {
    Gxy_t o, c;
} Grect_t;

/* textline structure */
typedef struct Gtextline_t {
    char *p;
    int n, j;
    int w, h;
} Gtextline_t;

/* Color structure */

typedef struct Gcolor_t {
    int index;
    int r, g, b;
} Gcolor_t;
#define G_MAXCOLORS 256

/* event structures */

/* event consumption modes */
typedef enum {
    G_ONEEVENT, G_MANYEVENTS
} Geventmode_t;

/* event types and main structure */
typedef enum {
    G_MOUSE, G_KEYBD
} Getype_t;
#define G_DOWN 0
#define G_UP   1
#define G_MOVE 2
#define G_LEFT   0
#define G_MIDDLE 1
#define G_RIGHT  2
typedef struct Gevent_t {
    Getype_t type;
    int wi;
    int code;
    int data;
    Gpoint_t p;
} Gevent_t;

/* Widgets */

/* minimum widget sizes */
#define MINVWSIZE 100
#define MINTWSIZE 40
#define MINBWSIZE 40
#define MINLWSIZE 25
#define MINAWSIZE 25
#define MINSWSIZE 40
#define MINCWSIZE 100
#define MINPWSIZE 100

/* gfx attributes for the [p]canvas widget */

/* drawing styles */
#define G_SOLID       0
#define G_DASHED      1
#define G_DOTTED      2
#define G_LONGDASHED  3
#define G_SHORTDASHED 4

/* drawing modes */
#ifndef HAVEMSWIN
#define G_SRC GXcopy
#define G_XOR GXxor
#else
#define G_SRC R2_COPYPEN
#define G_XOR /* R2_XORPEN */ R2_NOT
#endif

/* gfx attributes and the attribute structure */
typedef enum {
    G_GATTRCOLOR = 1, G_GATTRWIDTH = 2, G_GATTRMODE = 4,
    G_GATTRFILL = 8, G_GATTRSTYLE = 16
} Ggattrflags_t;
typedef struct Ggattr_t {
    Ggattrflags_t flags;
    int color;
    int width;
    int mode;
    int fill;
    int style;
} Ggattr_t;

/* widget attributes structures */

typedef enum {
    G_ATTRTYPEINT, G_ATTRTYPELONG,
    G_ATTRTYPEFLOAT, G_ATTRTYPEDOUBLE,
    G_ATTRTYPETEXT,
    G_ATTRTYPEPOINT, G_ATTRTYPESIZE, G_ATTRTYPERECT,
    G_ATTRTYPECOLOR,
    G_ATTRTYPEFUNC,
    G_ATTRTYPEULONG
} Gwattrtype_t;
typedef struct Gwattrmap_t {
    int id;
    Gwattrtype_t type;
    char *name;
} Gwattrmap_t;
typedef struct Gwattr_t {
    int id;
    union {
        int i;
        long l;
        float f;
        double d;
        char *t;
        Gpoint_t p;
        Gsize_t s;
        Grect_t r;
        Gcolor_t c;
        void *func;
        unsigned long u;
    } u;
} Gwattr_t;
typedef struct Gwlist_t {
    int wid;
    char *wname;
    int *attrid;
} Gwlist_t;

/* atrtibute ids */
#define G_ATTRORIGIN       0
#define G_ATTRSIZE         1
#define G_ATTRBORDERWIDTH  2
#define G_ATTRNAME         3
#define G_ATTRTEXT         4
#define G_ATTRAPPENDTEXT   5
#define G_ATTRCURSOR       6
#define G_ATTRMODE         7
#define G_ATTRLAYOUT       8
#define G_ATTRZORDER       9
#define G_ATTRCOLOR       10
#define G_ATTRVIEWPORT    11
#define G_ATTRWINDOW      12
#define G_ATTRCHILDCENTER 13
#define G_ATTRNEWLINECB   14
#define G_ATTRRESIZECB    15
#define G_ATTRBUTTONCB    16
#define G_ATTREVENTCB     17
#define G_ATTRUSERDATA    18

/* array widget structs */
typedef struct Gawcarray_t {
#ifndef HAVEMSWIN
    Widget w;
#else
    HWND w;
#endif
    int flag;
    int ox, oy, sx, sy, bs;
} Gawcarray_t;
#define AWCARRAYINCR 10
#define AWCARRAYSIZE sizeof (Gawcarray_t)

/* global state structure */
#define G_AWHARRAY 1
#define G_AWVARRAY 2
typedef struct Gawdata_t {
    int type;
    int sx, sy;
    Gawcarray_t *carray;
    int cj, cn;
#ifdef HAVEMSWIN
    int batchmode, working;
#endif
} Gawdata_t;

/* widget callbacks */
typedef void (*Gtwnewlinecb) (int, char *);
typedef void (*Gbuttoncb) (int, void *);
typedef void (*Glabelcb) (Gevent_t *);
typedef void (*Gcanvascb) (Gevent_t *);
typedef void (*Gawordercb) (void *, Gawdata_t *);
typedef void (*Gawcoordscb) (int, Gawdata_t *);
typedef void (*Gviewcb) (Gevent_t *);

#define G_ARRAYWIDGET    0
#define G_BUTTONWIDGET   1
#define G_CANVASWIDGET   2
#define G_LABELWIDGET    3
#define G_MENUWIDGET     4
#define G_PCANVASWIDGET  5
#define G_QUERYWIDGET    6
#define G_SCROLLWIDGET   7
#define G_TEXTWIDGET     8
#define G_VIEWWIDGET     9
#define G_WTYPESIZE     10

/* predefined widgets */

/* --- array --- */
typedef struct Gaw_t {
    Gawcoordscb func;
    int mode;
#ifdef HAVEMSWIN
    Gawdata_t data;
#endif
} Gaw_t;
#define AWSIZE sizeof (Gaw_t)

/* --- button --- */
typedef struct Gbw_t {
    Gbuttoncb func;
} Gbw_t;
#define BWSIZE sizeof (Gbw_t)

/* --- canvas --- */
typedef struct Gcw_t {
    int needredraw;
    int buttonsdown;
    char bstate[3];
    struct Gcwcolor_t {
        int inuse;
#ifndef HAVEMSWIN
        XColor color;
#else
        PALETTEENTRY color;
#endif
    } colors[G_MAXCOLORS];
    Ggattr_t gattr, defgattr;
    Grect_t wrect;
    Gsize_t vsize;
    Grect_t clip;
    Gcanvascb func;
#ifndef HAVEMSWIN
    Window window;
    Colormap cmap;
    GC gc;
    Pixmap grays[17];
    XFontStruct *font;
#else
    HPALETTE cmap;
    HDC gc;
    HBRUSH grays[17];
    HFONT font;
    int ncolor;
#endif
} Gcw_t;
#define CWSIZE sizeof (Gcw_t)

/* --- label --- */
typedef struct Glw_t {
    Glabelcb func;
} Glw_t;
#define LWSIZE sizeof (Glw_t)

/* --- menu --- */
typedef struct Gmw_t {
    int count;
} Gmw_t;
#define MWSIZE sizeof (Gmw_t)

/* --- postscript --- */
typedef struct Gpw_t {
#ifndef HAVEMSWIN
    FILE *fp;
    struct Gpwcolor_t {
        int inuse;
        int r, g, b;
        double nr, ng, nb;
    } colors[G_MAXCOLORS];
    Ggattr_t gattr, defgattr;
    Grect_t wrect;
    Gsize_t vsize;
#else
    struct Gpwcolor_t {
        int inuse;
        PALETTEENTRY color;
    } colors[G_MAXCOLORS];
    Ggattr_t gattr, defgattr;
    Grect_t wrect;
    Gsize_t vsize;
    HPALETTE cmap;
    HDC gc;
    HBRUSH grays[17];
    HFONT font;
    int ncolor;
#endif
} Gpw_t;
#define PWSIZE sizeof (Gpw_t)

/* --- query --- */
typedef struct Gqw_t {
#ifndef HAVEMSWIN
    Widget w;
#else
    HWND w;
#endif
    int state;
} Gqw_t;
#define QWSIZE sizeof (Gqw_t)

/* --- scroll --- */
typedef struct Gsw_t {
    int dummy;
} Gsw_t;
#define SWSIZE sizeof (Gsw_t)

/* --- text --- */
typedef struct Gtw_t {
    Gtwnewlinecb func;
} Gtw_t;
#define TWSIZE sizeof (Gtw_t)

/* --- view --- */
typedef struct Gvw_t {
    Gviewcb func;
    int closing;
} Gvw_t;
#define VWSIZE sizeof (Gvw_t)

/* the main widget structure */
typedef struct Gwidget_t {
    int type;
    int inuse;
    int pwi;
#ifndef HAVEMSWIN
    Widget w;
#else
    HWND w;
#endif
    union {
        Gaw_t *a;
        Gbw_t *b;
        Gcw_t *c;
        Glw_t *l;
        Gmw_t *m;
        Gpw_t *p;
        Gqw_t *q;
        Gsw_t *s;
        Gtw_t *t;
        Gvw_t *v;
    } u;
    unsigned long udata;
} Gwidget_t;
#define WIDGETINCR 20
#define WIDGETSIZE sizeof (Gwidget_t)

/* global array of widgets */
extern Gwidget_t *Gwidgets;
extern int Gwidgetn;

extern Gwlist_t MSNEAR Gwlist[];
extern Gwattrmap_t MSNEAR Gwattrmap[];

extern char *Gdefaultfont;
extern int Gneedredraw;
extern int Gbuttonsdown;
extern int Gerrflag;
extern char *Gpscanvasname;

extern int Gxfd;

#ifdef HAVEMSWIN
extern int Gnocallbacks;
#endif

/* functions returning an int
   return -1 if there's an error and
   also set the Gerrno variable

   the rendering functions may return +1
   if the graphical object is completely hidden
*/
int Ginit (void);
int Gterm (void);
int Gcreatewidget (int, int, int, Gwattr_t *);
int Gsetwidgetattr (int, int, Gwattr_t *);
int Ggetwidgetattr (int, int, Gwattr_t *);
int Gdestroywidget (int);
int Gqueryask (int, char *, char *, int);
int Gmenuaddentries (int, int, char **);
int Gmenudisplay (int, int);
int Gsync (void);
int Gresetbstate (int);
int Gcanvasclear (int);
int Gsetgfxattr (int, Ggattr_t *);
int Ggetgfxattr (int, Ggattr_t *);
int Garrow (int, Gpoint_t, Gpoint_t, Ggattr_t *);
int Gline (int, Gpoint_t, Gpoint_t, Ggattr_t *);
int Gbox (int, Grect_t, Ggattr_t *);
int Gpolygon (int, int, Gpoint_t *, Ggattr_t *);
int Gsplinegon (int, int, Gpoint_t *, Ggattr_t *);
int Garc (int, Gpoint_t, Gsize_t, double, double, Ggattr_t *);
int Gtext (int, char *, Gpoint_t, char *, double, char *, Ggattr_t *);
int Ggettextsize (int, char *, char *, double, Gsize_t *);
int Ggetmousecoords (int, Gpoint_t *, int *);

int Gprocessevents (int, Geventmode_t);

int Gaworder (Gwidget_t *, void *, Gawordercb);
int Gawsetmode (Gwidget_t *, int);
int Gawgetmode (Gwidget_t *);
void Gawdefcoordscb (int, Gawdata_t *);

Gwidget_t *newwidget (int);
Gwidget_t *findwidget (unsigned long, int);
void Gerr (char *, int, int, ...);

/* error messages */
#define G_ERRBADATTRID          1
#define G_ERRBADATTRVALUE       2
#define G_ERRBADCOLORINDEX      3
#define G_ERRBADPARENTWIDGETID  4
#define G_ERRBADWIDGETID        5
#define G_ERRBADWIDGETTYPE      6
#define G_ERRCANNOTCREATE       7
#define G_ERRCANNOTGETATTR      8
#define G_ERRCANNOTOPENFILE     9
#define G_ERRCANNOTSETATTR1    10
#define G_ERRCANNOTSETATTR2    11
#define G_ERRINITFAILED        12
#define G_ERRNOCHILDWIDGET     13
#define G_ERRNOPARENTWIDGET    14
#define G_ERRNOSUCHCURSOR      15
#define G_ERRNOTACANVAS        16
#define G_ERRNOTIMPLEMENTED    17
#define G_ERRNOTSUPPORTED      18

extern int Gerrno;
#endif /* _G_H */
