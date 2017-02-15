SUBDIRS = lib cmd

include Config.mk
include makearch/$(ARCH)

all clean:
	(for i in ${SUBDIRS}; do (cd $$i; make $@); done)

install:
	-$(MKDIR) -p $(BINDIR) $(LIBDIR) $(MANDIR) $(LIBMANDIR) $(DOCDIR)
	(for i in ${SUBDIRS}; do (cd $$i; make $@); done)
