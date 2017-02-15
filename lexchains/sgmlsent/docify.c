#include <stdio.h>
#include <string.h>
/*
 * F I N D
 *
 * get units within specified (e.g. SGML) tags
 * from text stream
 * or within character array...
 *
 */

#define LOOKING 0
#define COLLECTING 1

int
streamfind(infd,starttag,endtag,buf,n,write,outfd)
     FILE *infd, *outfd;
     char *starttag,*endtag;
     char *buf;
     int n;
     int write; /* copy uncollected stuff to outfd? */
{
  int c,count=strlen(starttag);
  int state = LOOKING;
  char *p = starttag, *o = buf+count;
  strcpy(buf,starttag);

  while((c=getc(infd))!=EOF){
    switch(state){
    case LOOKING:
      if(write)putc(c,outfd);
      if(c == *p++){
	if(!*p){
	  p = endtag;
	  state=COLLECTING;
       }
      }
      else {
	p=starttag;
      }
      break;
    case COLLECTING:
      *o++ = c;
      if(++count >= n){
	buf[n-1] = '\0';
	fprintf(stderr,"too big (%d) in |%s|\n",count,buf);
	exit(2);
      }
      if(c == *p++){
	if(!*p){
	  *o ='\0';
	  return(count);
	}
      }
      else {
	p = endtag;
      }
      break;
    }
  }
  return(0); /* reached EOF without finding what we wanted */
}

int
arrayfind(instart,inend,starttag,endtag,outstart,outend)
     char *instart, *inend,
       *starttag, *endtag,
       **outstart, **outend;
{
  
  register char *p,*q;

  for(p=instart,q=starttag;p<=inend;){
    if(*p++ == *q++){
      if(!*q){
	*outstart = p-strlen(starttag);
	q=endtag;
	while(p<=inend){
	  if(*p++ == *q++){
	    if(!*q){
	      *outend = p-1;
	      return(1);
	    }
	  }
	  else {q = endtag;}
	}
      }
    }
    else {q = starttag;}
  }
  return(0);
}
