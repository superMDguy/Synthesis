# select an architecture from makearch/
ARCH=sun4

# specify installation targets
PREFIX=/users/silver2/projects/clips/lingtools/gd

LIBDIR=$(PREFIX)/lib
BINDIR=$(PREFIX)/bin

MANDIR=$(PREFIX)/man/man1
MANEXT=1

LIBMANDIR=$(PREFIX)/man/man3
LIBMANEXT=3

# postscipt and html docs
DOCDIR=$(PREFIX)/doc

# location of libtcl.a and libtk.a
TCL_LIBDIR = /usr/local/tcl/lib
# location of tcl.h and tk.h
TCL_INCDIR = /usr/local/tcl/include

# version number on shared libraries
VERSION=1.0
