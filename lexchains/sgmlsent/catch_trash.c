catch_trash(l,w)
char *l, *w;
{

  if(strcmp(l,"Ph.")==0 && strcmp(w,"D.")==0)return(1);
  else if(strcmp(l,"Minneapolis-St.")==0 && strcmp(w,"Paul")==0)return(1);
  else return(0);
}
