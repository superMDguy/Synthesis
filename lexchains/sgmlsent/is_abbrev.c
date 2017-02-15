#include <stdio.h>
#include <ctype.h>

char *title_list[] = {
"capt.",
"cmdr.",
"col.",
"cpl.",
"dr.",
"gen.",
"maj.",
"major-gen.",
"mr.",
"mrs.",
"ms.",
"prof.",
"sgt.",
"sr.",
"sra.",
"st.",
"ste.",
""
};
char *abbrev_list[] = {
"a.m.",
"adm.",
"apr.",
"aug.",
"ave.",
"capt.",
"cie.",
"cmdr.",
"co.",
"col.",
"corp.",
"cpl.",
"dec.",
"dr.",
"e.g.",
"etc.",
"feb.",
"fr.",
"gen.",
"gov.",
"i.e.",
"inc.",
"jan.",
"jr.",
"jul.",
"lt.",
"ltd.",
"maj.",
"major-gen.",
"mr.",
"mrs.",
"ms.",
"mt.",
"nov.",
"oct.",
"p.m.",
"prof.",
"rd.",
"rep.",
"rev.",
"sen.",
"sept.",
"sgt.",
"sr.",
"sra.",
"st.",
"ste.",
""
};
#define MAXABBREV 64
static int n_abbrev = -1;
static int maxabbrev = MAXABBREV;
is_abbrev(s)
register char *s;
{
char new[MAXABBREV];
int i;
register char *ptr1, *ptr2;
char **ptr;

        if(strlen(s) > maxabbrev) return 0;
        lowerize(s,new);

	if(n_abbrev < 0){
		n_abbrev = 0;
		for(ptr = abbrev_list; **ptr; ptr++){
		  if((i=strlen(*ptr)) > maxabbrev)maxabbrev = i;
		  n_abbrev++;
		}
	}
	if(is_in(new,abbrev_list,n_abbrev))return(1);
	return(0);
}
#define MAXTITLE 16
static int n_title = -1;
static int maxtitle = MAXTITLE;
is_title(s)
register char *s;
{
char new[MAXTITLE];
int i;
register char *ptr1, *ptr2;
char **ptr;

        if(strlen(s) > maxtitle) return 0;
        lowerize(s,new);

	if(n_title < 0){
		n_title = 0;
		for(ptr = title_list; **ptr; ptr++){
		  if((i=strlen(*ptr)) > maxtitle)maxtitle = i;
		  n_title++;
		}
	}
	if(is_in(new,title_list,n_title))return(1);
	return(0);
}

is_in(s,l,n)
register char *s;
register char *l[];
register int n;
{
register int b1=0, b2=n-1, h, r;
	if(n < 0){
		fprintf(stderr,"is_in: n %d\n",n);
		exit(2);
	}
   while(1){
	if(b2 == b1)return(strcmp(s,l[b1])==0);
	if(b2 == b1+1){
		if(strcmp(s,l[b1])==0)return(1);
		else if(strcmp(s,l[b2])==0)return(1);
		else return(0);
	}
	h = (b1+b2)/2;
	r = strcmp(s,l[h]);
	if(r == 0)return(1);
	else if(r < 0)b2 = h;
	else b1 = h;
  }
}

lowerize(s,n)
  char *s, *n;
{
register char *p1, *p2;

	for(p1=s,p2=n;*p1;p1++){
	  if(isupper(*p1))*p2++ = tolower(*p1);
	  else *p2++ = *p1;
	}
	*p2 = '\0';
}
