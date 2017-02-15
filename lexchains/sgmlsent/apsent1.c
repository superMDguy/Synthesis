#include <stdio.h>
#include <ctype.h>
#include "features.h"
#define NFIELDS 100000
#define MAXWORD 128
char inbuf[NFIELDS*8];
char linebuf[NFIELDS*8];
char *fields[NFIELDS];
int feats[NFIELDS];
int debug = 0, tokenize=0;
main(ac,av)
int ac;
char **av;
{
register char *ptr, *f, *e, *out;
register int i, nfields, last, length;
int spacecount;
extern char *optarg;
extern int optind;
int c;

  while((c=getopt(ac,av,"Dt"))!=EOF)
	switch(c){
		case 'D':
			debug = 1;
			break;
		      case 't':
			tokenize =1 ;
			break;
		default:
			fprintf(stderr,"usage: %s [-Dt] <input\n");
			exit(2);
        }

  while(get_to_tab(inbuf)){
    spacecount = 0;
    ptr = inbuf;
    while(*ptr && isspace(*ptr))ptr++;
    for(out=linebuf;*ptr;){
      if(isspace(*ptr)){
	if(spacecount){
          ptr++;
          continue;
        }
	else {
	   spacecount++;
           *out++ = *ptr++;
           continue;
	}
      }
      else spacecount = 0;
      if(*ptr == '\140' && *(ptr+1) == '\140'){
        *out++ = '\140';*out++ = '\140';
        *out++ = ' ';
        ptr += 2;
      }
      else if(*ptr == '\047' && *(ptr+1) == '\047'){
        *out++ = ' ';
        *out++ = '\047';*out++ = '\047';
        ptr += 2;
      }

      else if(strchr(".!?,;",*ptr) &&
	 (islower(*(ptr-1)) || isdigit(*(ptr-1))) &&
	 (isupper(*(ptr+1)) || *(ptr+1)=='\047') 
			){
        *out++ = *ptr++;
        *out++ = ' ';
      }
      else *out++ = *ptr++;
    }
    *out = '\0';
    nfields = getmfields(linebuf,fields,NFIELDS);
    if(debug){
      for(i=0;i<nfields;i++){
	fprintf(stderr,"|%s|",fields[i]);
      }
      fprintf(stderr,"\n\n");
    }
    for(i=0;i<nfields;i++){
      f = fields[i];
      length = strlen(f);
      e = f + length  -1;
      feats[i] = cap_test(f);
      if((length < MAXWORD) && (*e == '.') && is_abbrev(f))
		feats[i] |= IS_ABBREV;
      if((feats[i] & IS_ABBREV) && is_title(f))
	        feats[i] |= IS_TITLE;
      if((length < MAXWORD) && is_funct(f))feats[i] |= IS_FUNCT;
      if(i==0)continue;
      if((i > 1) && (feats[i-1]&(RIGHT_QUOTE|LEFT_QUOTE|THREE_DOTS)))
		last = i-2;
      else last = i-1;
/* catch special cases */
      if((feats[last]&END_DOT) && catch_trash(fields[last],f))continue;
/* easy case for sentence end: period at end of non-abbrev, next
	word is FIRST_UPPER or ALL_UPPER */
	
      if( (feats[last]&END_DOT) && (feats[i]&(FIRST_UPPER|ALL_UPPER)) &&
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
      if(tokenize){
	if(i==nfields-1 ||
	   ((feats[i]&END_PUNCT) && !(feats[i]&END_DOT))||
	   ((feats[i]&S_FINAL) && (feats[i]&END_DOT) && !(feats[i]&IS_ABBREV))){
	  for(ptr=fields[i];*(ptr+1);ptr++)putchar(*ptr);
	  putchar('\n');
	}
	else printf("%s\n",fields[i]); 
      }
       else {
	 printf("%s",fields[i]);
	 if(debug)printf(" [%x]",feats[i]);
	 if(feats[i] & S_FINAL){
	   if( (i<(nfields-1)) && (feats[i+1]&(RIGHT_QUOTE|THREE_DOTS)) ){
	     printf(" %s",fields[i+1]);i++;
	   }
	   putchar('\n');
	 }
	 else putchar(' ');
       }
    }
    if(!tokenize){putchar('\n');putchar('\n');}
  }
  
}

get_to_tab(s)
char *s;
{
register char *ptr = s, *end = s + NFIELDS*8;
register int c;
int nlcount = 0;


	while((c=getchar()) != EOF){
		switch(c){
			case '\n':
				*ptr++ = ' ';
				nlcount++;
				if((ptr-s) && (nlcount >=2)){
				  nlcount = 0;
				  *(ptr-1)='\0';
				  return(ptr-s);
				}
				  
				break;
			case '	':
				*ptr = '\0';
				nlcount = 0;
				if(ptr-s){
				  return(ptr-s);
				}
				else break;
			default:
				*ptr++ = c;
				if(ptr > end){
				  fprintf(stderr,"get_to_tab: too long\n");
				  exit(2);
				}
				nlcount = 0;
				break;
		}
	}
        *ptr = '\0';
        return(ptr-s);
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
  if(strcmp(f,"...")==0)return(THREE_DOTS);
  if(strcmp(f,"``")==0)return(LEFT_QUOTE);
  if(strcmp(f,"''")==0)return(RIGHT_QUOTE);
  while(*f){
     if(islower(*f))lowercount++;
     else if(isupper(*f))uppercount++;
     else if(ispunct(*f)){
	punctcount++;
	if(*f == '.')dotcount++;
     }
     else if(isdigit(*f))digitcount++;
     f++;
  }
  if(lowercount == l)result = ALL_LOWER;
  if(digitcount != 0)result |= HAS_DIGITS;
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

	p = e;
	while(strchr(")\"'",*p))p--;
        if(*p == '.' || *p == '?' || *p == '!')result |= END_DOT;

        if(ispunct(*e))result |= END_PUNCT;
  }
  return(result);
}
