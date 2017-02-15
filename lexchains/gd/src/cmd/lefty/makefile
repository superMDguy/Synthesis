all:	lefty

include ../../Config.mk
include ../../makearch/$(ARCH)

DOT2L = dot2l
WM = x11

LEFTYLIBDIR= $(LIBDIR)/lefty
DEFINES=-DLEFTYPATH=\"$(LEFTYLIBDIR)\" -DHAVEDOT

INCS =	-I. \
	-I$(DOT2L) \
	-I$(X11INC) \
	-I$(WM)

DOT2LEFTYSRC = $(DOT2L)/dotparse.y $(DOT2L)/dot2l.c \
	$(DOT2L)/dotlex.c $(DOT2L)/dottrie.c
DOT2LEFTYOBJ = dotparse.o dot2l.o dotlex.o dottrie.o

WMSRC = $(WM)/gcommon.c $(WM)/garray.c $(WM)/gbutton.c $(WM)/gcanvas.c \
		$(WM)/glabel.c $(WM)/gmenu.c $(WM)/gpcanvas.c $(WM)/gquery.c \
		$(WM)/gscroll.c $(WM)/gtext.c $(WM)/gview.c
WMOBJ = gcommon.o garray.o gbutton.o gcanvas.o \
		glabel.o gmenu.o gpcanvas.o gquery.o \
		gscroll.o gtext.o gview.o

SRCS = lefty.c gfxview.c txtview.c internal.c display.c str.c exec.c parse.c \
	lex.c $(DOT2LEFTYSRC) tbl.c mem.c code.c io.c g.c common.c malloc.c $(WMSRC)

OBJS = lefty.o gfxview.o txtview.o internal.o display.o str.o exec.o parse.o \
	lex.o $(DOT2LEFTYOBJ) tbl.o mem.o code.o io.o g.o common.o malloc.o $(WMOBJ)

lefty: $(OBJS)
	$(CC) $(LDFLAGS) -L$(X11LIB) $(OBJS) -lXaw -lXmu -lXt -lXext -lX11 -lm \
		$(LEFTYLIBS) -o lefty

# lefty.static is not completely static because we don't want to
# bind libc unnecessarily.  we mostly wanted to bind things like X11
# and tcl that are likely to cause headaches when porting binaries.
lefty.static : lefty.static.$(ARCH)

lefty.static.linux: $(OBJS)
	$(CC) -o lefty $(OBJS) \
		$(X11LIB)/libXaw.a $(X11LIB)/libXmu.a $(X11LIB)/libXt.a \
		$(X11LIB)/libXext.a $(X11LIB)/libX11.a $(X11LIB)/libSM.a \
		$(X11LIB)/libICE.a -lm

lefty.static.sol.sun4: $(OBJS)
	$(CC) -o lefty $(OBJS) \
		$(X11LIB)/libXaw.a $(X11LIB)/libXmu.a $(X11LIB)/libXt.a \
		$(X11LIB)/libX11.a $(X11LIB)/libXext.a \
		-ldl /lib/libsocket.a /lib/libnsl.a /lib/libw.a /lib/libintl.a -lm

lefty.static.sun4: $(OBJS)
	$(CC) -o lefty $(OBJS) \
		$(X11LIB)/libXaw.a $(X11LIB)/libXmu.a $(X11LIB)/libXt.a \
		$(X11LIB)/libX11.a $(X11LIB)/libXext.a -lm

dot2l.o:	$(DOT2L)/dot2l.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

dotlex.o:	$(DOT2L)/dotlex.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

dottrie.o:	$(DOT2L)/dottrie.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

gcommon.o:	$(WM)/gcommon.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

garray.o:	$(WM)/garray.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

gbutton.o:	$(WM)/gbutton.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

gcanvas.o:	$(WM)/gcanvas.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

glabel.o:	$(WM)/glabel.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

gmenu.o:	$(WM)/gmenu.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

gpcanvas.o:	$(WM)/gpcanvas.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

gquery.o:	$(WM)/gquery.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

gscroll.o:	$(WM)/gscroll.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

gtext.o:	$(WM)/gtext.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?

gview.o:	$(WM)/gview.c
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $?


dotparse.c: $(DOT2L)/dotparse.y
	$(YACC) -d $(DOT2L)/dotparse.y
	$(MV) y.tab.c dotparse.c
	$(MV) y.tab.h dotparse.h

dotparse.o: dotparse.c

install: lefty
	$(INSTALL) lefty $(BINDIR)
	$(INSTALL) lefty.1 $(MANDIR)/lefty.$(MANEXT)
	$(INSTALL) lefty.ps $(DOCDIR)
	$(CP) -r lib/lefty $(LIBDIR)
	$(SED) "s%LEFTYLIBDIR%$(LEFTYLIBDIR)%" < dotty.sh > dotty
	chmod +x dotty
	$(INSTALL) dotty $(BINDIR)

clean:
	$(RM) lefty core *.[oa] dotparse.[ch]

.c.o:
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $*.c
