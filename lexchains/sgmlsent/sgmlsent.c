/*
 * S G M L S E N T
 * 
 * Do sentence divisions within sgml <X> </X> tags,
 *    passing everything else through unchanged...
 *  Default tag is <p> but can be changed using -p flag
 *
 * note: 9/2/91 -- need to watch out for titles after open paren,
 *   e.g. His father (Mr. Jones) was  ....
 */
#include <stdio.h>
#include <ctype.h>
/*#include <libc.h>*/
#include "features.h"
#define NFIELDS 100000
#define MAXWORD 128
char inbuf[NFIELDS*8];
char linebuf[NFIELDS*8];
char *fields[NFIELDS];
int feats[NFIELDS];
int debug = 0;
main(ac,av)
int ac;
char **av;
{
register char *ptr, *f, *e, *out;
register int i, nfields, last, length;
char startpat[256], endpat[256], *pattern="p";
int spacecount;
int silent = 0;
extern char *optarg;
extern int optind;
int c,chars;

char *outfile ;                     /* yuval */

  while((c=getopt(ac,av,"Dp:o:s"))!=EOF)
	switch(c){
		case 'D':
			debug = 1;
			break;
		case 'p':
			pattern = optarg;
			break;
	        case 'o':                       /* yuval */
		        outfile = optarg ;
			freopen(outfile, "w", stdout) ;
			break;
		default:
			fprintf(stderr,
		       	 "usage: %s [-D] [-p tag] [-o output file] <input\n",av[0]);
			exit(2);
        }

  setfields(" \t\n");
  sprintf(startpat,"<%s>",pattern);
  sprintf(endpat,"</%s>",pattern);
  if(debug)fprintf(stderr,"patterns are %s %s\n",startpat,endpat);

  while(chars = streamfind(stdin,startpat,endpat,inbuf,NFIELDS*8,1,stdout)){
    spacecount = 0;
    inbuf[chars-strlen(endpat)] = '\0';
    ptr = inbuf+strlen(startpat);

    while(*ptr && isspace(*ptr))ptr++; /* skip leading spaces */
    for(out=linebuf;*ptr;){
      if(isspace(*ptr)){ /* collapse other multiple spaces to 1 */
	*out++ = *ptr++;
	while(isspace(*ptr))ptr++;
      }

      else *out++ = *ptr++;
    }
    *out = '\0';
    nfields = getmfields(linebuf,fields,NFIELDS);
    if(nfields == 0){
      printf("%s",endpat);
      continue;
    }
    if(debug){
      for(i=0;i<nfields;i++){
	printf("|%s|",fields[i]);
      }
      printf("\n\n");
    }
    printf("\n<s> ");
    for(i=0;i<nfields;i++){
      f = fields[i];
      length = strlen(f);
      e = f + length  -1;
      feats[i] = cap_test(f);
      if(i>3 && (feats[i]&IS_DOT) && (feats[i-1]&IS_DOT) &&
	 (feats[i-2]&IS_DOT) && (feats[i-3]&END_DOT))feats[i] |= S_FINAL;
      if((length < MAXWORD) && (*e == '.') && is_abbrev(f))
		feats[i] |= IS_ABBREV;
      if((feats[i] & IS_ABBREV) && is_title(f))
	        feats[i] |= IS_TITLE;
      if(length < MAXWORD && (is_funct(f) ||
	 (*f=='"' && is_funct(f+1))))feats[i] |= IS_FUNCT;
      if(i==0)continue;
      if((i > 1) && (feats[i-1]&(RIGHT_QUOTE|LEFT_QUOTE|THREE_DOTS)))
		last = i-2;
      else last = i-1;
/* catch special cases */
      if(*(fields[i])=='@')feats[last] |= S_FINAL;
      else if((feats[last]&END_DOT) && catch_trash(fields[last],f))continue;
/* easy case for sentence end: period at end of non-abbrev, next
	word is FIRST_UPPER or ALL_UPPER */
	
      else if( (feats[last]&END_DOT) && (feats[i]&(FIRST_UPPER|ALL_UPPER)) &&
	 !(feats[last]&(ALL_UPPER_AND_DOT|IS_ABBREV))
	  ){
		feats[last] |= S_FINAL;
      }
/* harder case: period at end of apparent abbrev. or initials :
	if next word is function word and FIRST_UPPER, go ahead and break */

      else if( (feats[last]&(ALL_UPPER_AND_DOT|IS_ABBREV)) &&
	       (!(feats[last]&IS_TITLE)) &&
               (feats[i]&IS_FUNCT) &&
               ((feats[i]&FIRST_UPPER))){
                   feats[last] |= S_FINAL;
      }
    }

    for(i=0;i<nfields;i++){
       printf("%s",fields[i]);
       if(debug)printf(" [%x]",feats[i]);
       if(feats[i] & S_FINAL){
                if( (i<(nfields-1)) && (feats[i+1]&(RIGHT_QUOTE|THREE_DOTS)) ){
                  	printf(" %s",fields[i+1]);i++;
                }
		printf(" </s>\n");
		if(i<nfields)printf("<s> ");
       }
       else putchar(' ');
     }
    printf("</s>\n%s",endpat);
  }
}

cap_test(input)
char *input;
{
  int result;
  register int lowercount = 0, uppercount = 0,
	 punctcount = 0, dotcount = 0, digitcount = 0, l;
  register char *f = input, *e, *p;

  result = 0;
  l = strlen(f);
  e = input+l-1;
  if(strcmp(f,"...")==0 || strcmp(f,". . .")==0)return(THREE_DOTS);
  if(strcmp(f,"``")==0)return(LEFT_QUOTE);
  if(strcmp(f,"''")==0)return(RIGHT_QUOTE);
  if((strcmp(f,".")==0))
    return(END_DOT|IS_DOT);
  while(*f){
     if(islower(*f))lowercount++;
     else if(isupper(*f) || *f=='&')uppercount++;
     else if(ispunct(*f)){
	punctcount++;
	if(*f == '.')dotcount++;
     }
     else if(isdigit(*f))digitcount++;
     f++;
  }
  if(lowercount == l)result = ALL_LOWER;
  else if(uppercount == l){
     result = ALL_UPPER;
     if(l == 1)result |= FIRST_UPPER;
  }
  else if(punctcount == l)result = ALL_PUNCT;
  else if((digitcount == (l-1)) && *e == '.'){
	result = (DIGITS_PLUS_PERIOD|END_DOT);
  }
  else {
	if(l == (dotcount + uppercount)) result |= ALL_UPPER_AND_DOT;
        if(isupper(*input)) result |= FIRST_UPPER;
	else if(l>1 && (*input == '"') && isupper(*(input+1)))
	  result |= FIRST_UPPER;

	p = e;
	while(strchr(")\"'",*p))p--;
        if(*p == '.' || *p == '?' || *p == '!')result |= END_DOT;

        if(ispunct(*e))result |= END_PUNCT;
  }
  return(result);
}

