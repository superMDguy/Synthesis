all:	libgraph.a

include ../../Config.mk
include ../../makearch/$(ARCH)

INCS =	-I. \
	-I../libdict

OBJS =	attribs.o graph.o node.o edge.o graphio.o \
	lexer.o parser.o refstr.o trie.o

libgraph.a: $(OBJS)
	$(AR) cr libgraph.a $(OBJS)
	$(RANLIB) libgraph.a

# this is a home-made tool
#trie.c : trie_input
#	triegen  < trie_input > trie.c

lexer.o : parser.o

parser.c : parser.y
	$(YACC) -dv parser.y
	$(SED) 1s/#include.*// < y.tab.c | $(SED) "s/yy/ag/g" > parser.c
	$(SED) "s/yy/ag/g" < y.tab.h > parser.h
	$(RM) y.tab.c y.tab.h

install: $(OBJS) libgraph.a
	$(INSTALL) libgraph.a $(LIBDIR)
	$(RANLIB) $(LIBDIR)/libgraph.a
	$(LD) -o $(LIBDIR)/libgraph.so.$(VERSION) $(OBJS)
	$(INSTALL) graph.3 $(LIBMANDIR)/libgraph.$(LIBMANEXT)

clean:
	$(RM) *.[oa] core parser.[ch] lib*.so.*

.c.o :
	$(CC) -c $(CCFLAGS) $(INCS) $(DEFINES) $*.c
