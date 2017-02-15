SUBDIRS = libdict libgraph libgd

all clean install:
	(for i in ${SUBDIRS}; do (cd $$i; make $@); done)
