/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include	"dot.h"

dot_write(g)
graph_t	*g;
{
	switch (Output_lang) {
		case POSTSCRIPT:
		case HPGL:
		case MIF:
		case GIF:
		case ISMAP:
			emit_graph(g); break;
		case ATTRIBUTED_DOT:
			attach_attrs(g);
			agwrite(g,Output_file); break;
		case CANONICAL_DOT:
			agwrite(g,Output_file); break;
		case PLAIN:		
			attach_attrs(g); write_plain(g,Output_file); break;
	}
	fflush(Output_file);
}

static char *rectbufp;
static int set_record_rects (n, f)
	node_t *n;
	field_t *f;
{
	int             i;

	if (f->n_flds == 0) {
		sprintf(rectbufp, "%d,%d,%d,%d ",
				f->b.LL.x + n->u.coord.x,
				f->b.LL.y + n->u.coord.y,
				f->b.UR.x + n->u.coord.x,
				f->b.UR.y + n->u.coord.y);
		while (*rectbufp) rectbufp++;
	}
	for (i = 0; i < f->n_flds; i++)
		set_record_rects (n, f->fld[i]);
}

static attrsym_t *safe_dcl(g,obj,name,def,fun)
graph_t		*g;
void		*obj;
char		*name,*def;
attrsym_t	*(*fun)();	
{
	attrsym_t	*a = agfindattr(obj,name);
	if (a == NULL) a = fun(g,name,def);
	return a;
}

attach_attrs(g)
graph_t	*g;
{
	int		i,j;
	char	buf[BUFSIZ],*p;
	node_t	*n;
	edge_t	*e;
	point	pt;

	safe_dcl(g,g->proto->n,"pos","",agnodeattr);
	safe_dcl(g,g->proto->n,"rects","",agnodeattr);
	N_width = safe_dcl(g,g->proto->n,"width","",agnodeattr);
	N_height = safe_dcl(g,g->proto->n,"height","",agnodeattr);
	safe_dcl(g,g->proto->e,"pos","",agedgeattr);
	if (g->u.has_edge_labels) safe_dcl(g,g->proto->e,"lp","",agedgeattr);
	if (g->u.label) {
		safe_dcl(g,g,"lp","",agraphattr);
		pt = g->u.label->p;
		sprintf(buf,"%d,%d",pt.x,pt.y);
		agset(g,"lp",buf);
	}
	safe_dcl(g,g,"bb","",agraphattr);
	for (n = agfstnode(g); n; n = agnxtnode(g,n)) {
		sprintf(buf,"%d,%d",n->u.coord.x,n->u.coord.y);
		agset(n,"pos",buf);
		sprintf(buf,"%.2f",PS2INCH(n->u.ht));
		agxset(n,N_height->index,buf);
		sprintf(buf,"%.2f",PS2INCH(n->u.lw + n->u.rw));
		agxset(n,N_width->index,buf);
		if (strcmp (n->u.shape->name, "record") == 0) {
			buf[0] = '\000', rectbufp = &buf[0];
			set_record_rects (n, n->u.shape_info);
			if (rectbufp > &buf[0]) /* get rid of last space */
				*(--rectbufp) = '\000';
			agset(n,"rects",buf);
		}
		for (e = agfstout(g,n); e; e = agnxtout(g,e)) {
			p = buf;
if (e->u.spl == NULL)
	{fprintf(stderr,"lost spline of %s %s\n",e->tail->name,e->head->name); continue;}
			for (i = 0; i < e->u.spl->size; i++) {
				if (i > 0) *p++ = ';';
				if (e->u.spl->list[i].sflag) {
					sprintf (p, "s,%d,%d ",e->u.spl->list[i].sp.x,e->u.spl->list[i].sp.y);
					while (*p) p++;
				}
				if (e->u.spl->list[i].eflag) {
					sprintf (p, "e,%d,%d ",e->u.spl->list[i].ep.x,e->u.spl->list[i].ep.y);
					while (*p) p++;
				}
				for (j = 0; j < e->u.spl->list[i].size; j++) {
					if (j > 0) *p++ = ' ';
					pt = e->u.spl->list[i].list[j];
					sprintf(p,"%d,%d",pt.x,pt.y);
					while (*p) p++;
				}
				*p = '\0';
			}
			agset(e,"pos",buf);
			if (e->u.label) {
				pt = e->u.label->p;
				sprintf(buf,"%d,%d",pt.x,pt.y);
				agset(e,"lp",buf);
			}
		}
	}
	rec_attach_bb(g);
}

rec_attach_bb(g)
graph_t	*g;
{
	int		c;
	char	buf[32];

	sprintf(buf,"%d,%d,%d,%d", g->u.bb.LL.x, g->u.bb.LL.y,
		g->u.bb.UR.x, g->u.bb.UR.y);
	agset(g,"bb",buf);
	for (c = 1; c <= g->u.n_cluster; c++) rec_attach_bb(g->u.clust[c]);
}

write_plain(g,f)
graph_t		*g;
FILE		*f;
{
	int			i;
	node_t		*n;
	edge_t		*e;
	bezier		bz;
	char		buf[SMALLBUF],buf1[SMALLBUF];

	setup_graph(g);
	fprintf(f,"graph %.3lf",g->u.drawing->scale); printptf(f,g->u.bb.UR); fprintf(f,"\n");
	for (n = agfstnode(g); n; n = agnxtnode(g,n)) {
		fprintf(f,"node %s ",agstrcanon(n->name,buf)); printptf(f,n->u.coord);
		fprintf(f," %.3lf %.3lf %s %s %s %s\n",
			n->u.width,n->u.height,agstrcanon(n->u.label->text,buf),
			late_nnstring(n,N_style,"solid"),
			n->u.shape->name,
			late_nnstring(n,N_color,DEFAULT_COLOR));
	}
	for (n = agfstnode(g); n; n = agnxtnode(g,n)) {
		for (e = agfstout(g,n); e; e = agnxtout(g,e)) {
			bz = e->u.spl->list[0];
			fprintf(f,"edge %s %s %d",agstrcanon(e->tail->name,buf),
				agstrcanon(e->head->name,buf1),bz.size);
			for (i = 0; i < bz.size; i++) printptf(f,bz.list[i]);
			if (e->u.label) {
				fprintf(f," %s",agstrcanon(e->u.label->text,buf));
				printptf(f,e->u.label->p);
			}
			fprintf(f," %s %s\n",late_nnstring(e,E_style,"solid"),
				late_nnstring(e,E_color,DEFAULT_COLOR));
		}
	}
	fprintf(f,"stop\n");
}

printptf(f,pt)
FILE		*f;
point		pt;
{
	fprintf(f," %.3lf %.3lf",PS2INCH(pt.x),PS2INCH(pt.y));
}
