#include	<sfio.h>
#include	<dict.h>

/*	This program reads a list of words, 1 per line, from sfstdin.
**	It removes any redundancies, then outputs the new list in
**	the alphabetic order.
*/

#define ulong	unsigned long
#define reg	register

_BEGIN_EXTERNS_
extern Void_t*	malloc _ARG_((int));
extern int	memcpy _ARG_((Void_t*, Void_t*, int));
extern void	free _ARG_((Void_t*));
_END_EXTERNS_

/* compare two strings by their alphabetic order */
#if __STD_C
static alphacmp(Dict_t* dict, reg char* s1, reg char* s2, Dtdisc_t* disc)
#else
static alphacmp(dict,s1,s2,disc)
Dict_t*		dict;
reg char*	s1;
reg char*	s2;
Dtdisc_t*	disc;
#endif
{
	reg int	c1, c2;

	while((c1 = *s1++) != 0)
	{	if((c2 = *s2++) == 0)
			return 1;

		if(c1 >= 'A' && c1 <= 'Z')
		{	if(c2 >= 'a' && c2 <= 'z')
			{	c2 = 'A' + (c2 - 'a');
				return c1 <= c2 ? -1 : 1;
			}
		}
		else if(c1 >= 'a' && c1 <= 'z')
		{	if(c2 >= 'A' && c2 <= 'Z')
			{	c2 = 'a' + (c2 - 'A');
				return c1 >= c2 ? 1 : -1;
			}
		}

		if((c1 -= c2) != 0)
			return c1;
	}

	return *s2 ? -1 : 0;
}

/* make a copy of a string */
#if __STD_C
static Void_t* newstring(Dict_t* dict, reg Void_t* s, Dtdisc_t* disc)
#else
static Void_t* newstring(dict,s,disc)
Dict_t*		dict;
reg Void_t*	s;
Dtdisc_t*	disc;
#endif
{
	reg Void_t*	news;

	if(!(news = malloc(sfslen())) )
		return NIL(Void_t*);
	memcpy(news, s, sfslen());

	return (Void_t*)news;
}

/* free a string */
#if __STD_C
static void delstring(Dict_t* dict, reg Void_t* s, Dtdisc_t* disc)
#else
static void delstring(dict,s,disc)
Dict_t*		dict;
reg Void_t*	s;
Dtdisc_t*	disc;
#endif
{
	free(s);
}

static Dtdisc_t	Disc = {0, 0, newstring, delstring, NIL(Dtcompar_f), NIL(Dthash_f)};

main()
{
	reg char*	s;
	reg Dict_t*	dict;
	reg Dtlink_t*	link;

	/* create a dictionary, use hashing and strcmp for speed */
	if(!(dict = dtopen(&Disc,Dthash)) )
		return -1;

	/* read strings and insert them into dict */
	while((s = sfgetr(sfstdin,'\n',1)) )
	{	if(!s[0])
			continue;
		dtinsert(dict,s);
	}

	/* change comparison function to using alphabetic order */
	Disc.comparf = alphacmp;
	if(!dtdisc(dict,&Disc))
		return -1;

	/* Order objects */
	if(!dtmethod(dict,Dttree,-1) )
		return -1;

	/* now output the words */
	for(link = dtflatten(dict); link; link = dtlink(dict,link))
		sfprintf(sfstdout,"%s\n",(char*)dtobj(dict,link));

	return 0;
}
