all:	libdict.a

include ../../Config.mk
include ../../makearch/$(ARCH)

INCS =	-I.

OBJS =	dtclose.o dtdisc.o dtflatten.o dthash.o dtmethod.o dtopen.o dtsize.o \
	dtrestore.o dtstat.o dttree.o dtview.o dtwalk.o dtextract.o

libdict.a: $(OBJS)
	$(AR) cr libdict.a $(OBJS)
	$(RANLIB) libdict.a

install: $(OBJS) libdict.a
	$(INSTALL) libdict.a $(LIBDIR)
	$(RANLIB) $(LIBDIR)/libdict.a
	$(LD) -o $(LIBDIR)/libdict.so.$(VERSION) $(OBJS)
	$(INSTALL) dict.3 $(LIBMANDIR)/libdict.$(LIBMANEXT)

clean:
	$(RM) *.[ao] core lib*.so.*

.c.o:
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $*.c

