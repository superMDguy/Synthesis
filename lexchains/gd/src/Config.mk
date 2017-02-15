# select an architecture from makearch/
ARCH=sol.sun4

# specify installation targets
PREFIX=/proj/corpora2/lexchains/gd/

LIBDIR=$(PREFIX)/lib
BINDIR=$(PREFIX)/bin

MANDIR=$(PREFIX)/man/man1
MANEXT=1

LIBMANDIR=$(PREFIX)/man/man3
LIBMANEXT=3

# postscipt and html docs
DOCDIR=$(PREFIX)/doc

# location of libtcl.a and libtk.a
TCL_LIBDIR = /usr/local/lib
# location of tcl.h and tk.h
TCL_INCDIR = /usr/local/include

# version number on shared libraries
VERSION=1.0
