all:	libtcldot.a libtkdot.a tcldot tkdot

include ../../Config.mk
include ../../makearch/$(ARCH)

INCS =	-I. \
	-I../../lib/libgraph \
	-I../../lib/libdict \
	-I../../lib/libgd \
	-I../dot \
	-I$(TCL_INCDIR) \
	-I$(X11INC)

LIBS =	-L../dot -ldot \
	-L../../lib/libgraph -lgraph \
	-L../../lib/libdict -ldict \
	-L../../lib/libgd -lgd

TCLLIBS = $(LIBS) -L$(TCL_LIBDIR) -ltcl7.5 -lm

TKLIBS = $(LIBS) -L$(TCL_LIBDIR) -ltk4.1 -ltcl7.5 -L$(X11LIB) -lX11 -lm

OBJS =	dotgraph.o handles.o tkgen.o gdCmd.o

tcldot:	tclAppInit.o libtcldot.a
	$(CC) $(LDFLAGS) tclAppInit.o libtcldot.a $(TCLLIBS) -o tcldot

tkdot:	tkAppInit.o libtkdot.a
	$(CC) $(LDFLAGS) tkAppInit.o libtkdot.a $(TKLIBS) -o tkdot

libtcldot.a : $(OBJS) tclrender.o
	$(AR) cr libtcldot.a $(OBJS) tclrender.o
	$(RANLIB) libtcldot.a

libtkdot.a : $(OBJS) tkrender.o
	$(AR) cr libtkdot.a $(OBJS) tkrender.o
	$(RANLIB) libtkdot.a

colxlate.o : colortbl.h
psgen.o : ps.h

install: tcldot tkdot $(OBJS) libtcldot.a libtkdot.a
	$(INSTALL) tcldot $(BINDIR)
	$(INSTALL) tkdot $(BINDIR)
	$(INSTALL) libtcldot.a $(LIBDIR)
	$(RANLIB) $(LIBDIR)/libtcldot.a
	$(INSTALL) libtkdot.a $(LIBDIR)
	$(RANLIB) $(LIBDIR)/libtkdot.a
	$(LD) -o $(LIBDIR)/libtcldot.so.$(VERSION) $(OBJS) tclrender.o
	$(LD) -o $(LIBDIR)/libtkdot.so.$(VERSION) $(OBJS) tkrender.o
	$(INSTALL) tcldot.1 $(MANDIR)/tcldot.$(MANEXT)
	$(INSTALL) tcldot.html $(DOCDIR)

clean:
	$(RM) *.[ao] core tcldot tkdot lib*.so.*

.c.o:
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $*.c

tkdot.static : tkdot.static.$(ARCH)

tkdot.static.sol.sun4: $(OBJS)
	$(CC) -o tkdot $ tkAppInit.o libtkdot.a \
		$(LIBS) $(TCL_LIBDIR)/libtk.a  \
		$(TCL_LIBDIR)/libtcl.a $(X11LIB)/libX11.a $(X11LIB)/libXext.a \
		$(X11LIB)/libX11.a -ldl /lib/libsocket.a /lib/libnsl.a /lib/libw.a \
		/lib/libintl.a -lm

tkdot.static.sun4: $(OBJS)
	$(CC) -o tkdot $ tkAppInit.o libtkdot.a \
		$(LIBS) $(TCL_LIBDIR)/libtk.a  \
		$(TCL_LIBDIR)/libtcl.a $(X11LIB)/libX11.a $(X11LIB)/libXext.a \
		$(X11LIB)/libX11.a -ldl -lm

tkdot.static.sgi: $(OBJS)
	$(CC) -o tkdot $ tkAppInit.o libtkdot.a \
		$(LIBS) $(TCL_LIBDIR)/libtk.a  \
		$(TCL_LIBDIR)/libtcl.a $(X11LIB)/libX11.a $(X11LIB)/libXext.a \
		$(X11LIB)/libX11.a -ldl -lm
