all:	libgd.a

include ../../Config.mk
include ../../makearch/$(ARCH)

INCS = 	-I.

LIBS =	-lm

OBJS =	gd.o gdfontl.o gdfonts.o

libgd.a: $(OBJS)
	$(AR) cr libgd.a ${OBJS}
	$(RANLIB) libgd.a

gddemo: gddemo.o libgd.a
	$(CC) $(LDFLAGS) gddemo.o $(LIBS) -o gddemo

giftogd: giftogd.o libgd.a
	$(CC) $(LDFLAGS) giftogd.o libgd.a $(LIBS) -o giftogd

install: $(OBJS) libgd.a
	$(INSTALL) libgd.a $(LIBDIR)
	$(RANLIB) $(LIBDIR)/libgd.a
	$(LD) -o $(LIBDIR)/libgd.so.$(VERSION) $(OBJS)
	$(INSTALL) gd.html $(DOCDIR)

clean:
	$(RM) *.[ao] core gddemo giftogd lib*.so.*

.c.o:
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $*.c

