all:	neato

include ../../Config.mk
include ../../makearch/$(ARCH)

INCS =  -I. \
        -I../../lib/libgraph \
        -I../../lib/libdict \
        -I../../lib/libgd

LIBS =  -L../../lib/libgraph -lgraph \
        -L../../lib/libdict -ldict \
        -L../../lib/libgd -lgd \
        -lm

HDRS = colortbl.h const.h dot.h globals.h graphinfo.h hacks.h macros.h \
	neato.h neato_const.h procs.h ps.h structs.h

OBJS = colxlate.o emit.o gifgen.o hpglgen.o ismapgen.o input.o malloc.o \
	mifgen.o neato.o output.o psgen.o shapes.o solve.o utils.o splines.o

neato:	$(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o neato

install: neato 
	$(INSTALL) neato $(BINDIR)
	$(INSTALL) neato.1 $(MANDIR)/neato.$(MANEXT)

$(OBJS) : $(HDRS)
colxlate.o : colortbl.h
psgen.o : ps.h

ps.h : ps.lib
	$(AWK) -f awk/stringize.awk ps.lib > ps.h

colortbl.h : color_names
	$(SED) s/_//g color_names | $(SORT) > color_lib
	$(AWK) -f awk/colortbl.awk color_lib > colortbl.h
	$(RM) color_lib

clean:
	$(RM) core *.[oa] neato colortbl.h ps.h

.c.o:
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $*.c
