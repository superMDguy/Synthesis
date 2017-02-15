all:	libdot.a dot

include ../../Config.mk
include ../../makearch/$(ARCH)

INCS =	-I. \
	-I../../lib/libgraph \
	-I../../lib/libdict \
	-I../../lib/libgd

LIBS =	-L../../lib/libgraph -lgraph \
	-L../../lib/libdict -ldict \
	-L../../lib/libgd -lgd \
	-lm

OBJS = acyclic.o colxlate.o conc.o emit.o fastgr.o flat.o hpglgen.o \
       input.o mifgen.o mincross.o ns.o output.o position.o postproc.o psgen.o \
       rank.o routespl.o shapes.o splines.o timing.o utils.o globals.o \
       class1.o class2.o cluster.o decomp.o gifgen.o ismapgen.o picgen.o

HFILES = dot.h const.h globals.h hacks.h macros.h procs.h structs.h

OFILES = dot.o malloc.o

dot:	$(OFILES) libdot.a
	$(CC) $(LDFLAGS) $(OFILES) libdot.a $(LIBS) -o dot

libdot.a : $(OBJS)
	$(AR) cr libdot.a $(OBJS)
	$(RANLIB) libdot.a

$(OFILES) : $(HFILES)
colxlate.o : colortbl.h
psgen.o : ps.h

ps.h : ps.lib
	$(AWK) -f awk/stringize.awk ps.lib > ps.h

colortbl.h : color_names
	$(SED) s/_//g color_names | $(SORT) > color_lib
	$(AWK) -f awk/colortbl.awk color_lib > colortbl.h
	$(RM) color_lib

install: dot $(OBJS) libdot.a
	$(INSTALL) dot $(BINDIR)
	$(INSTALL) libdot.a $(LIBDIR)
	$(RANLIB) $(LIBDIR)/libdot.a
	$(LD) -o $(LIBDIR)/libdot.so.$(VERSION) $(OBJS)
	$(INSTALL) dot.1 $(MANDIR)/dot.$(MANEXT)
	$(INSTALL) dotguide.ps $(DOCDIR)

clean:
	$(RM) *.[ao] core dot colortbl.h ps.h lib*.so.*

.c.o:
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $*.c
