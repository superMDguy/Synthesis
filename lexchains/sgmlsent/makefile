sgmlsent: sgmlsent.o is_abbrev.o is_funct.o catch_trash.o \
           docify.o getfields.o
	cc -g -o sgmlsent sgmlsent.o is_funct.o is_abbrev.o catch_trash.o \
	docify.o getfields.o

is_abbrev.o: is_abbrev.c features.h
	cc -c is_abbrev.c
is_funct.o: is_funct.c features.h
	cc -c is_funct.c
catch_trash.o: catch_trash.c features.h
	cc -c catch_trash.c
sgmlsent.o: sgmlsent.c
	cc -c sgmlsent.c
