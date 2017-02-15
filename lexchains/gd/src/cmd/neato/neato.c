/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#define EXTERN
#include	"neato.h"

char		*Version = "neato version 92b 01-22-94";

void	scan_graph(),shortest_path();
void	solve_model(),move_node();
node_t	*choose_node();


static graph_t			*G;		/* graph being drawn	  */
static int				nG;		/* n_nodes(G)			  */
double 				Epsilon;	/* defined in input_graph */
double 				Nodesep = 1.0;
double 				Nodefactor = 1.0;
int					MaxIter;

#ifdef DOS
#define srand48 srand
double drand48()
{
	double d;
	d = rand();
	d = d / RAND_MAX;
	return d;
}
#endif

double fpow32(x)
double	x;
{
    x = sqrt(x);
    return x * x * x;
}

double distvec(p0,p1,vec)
double	*p0,*p1,*vec;
{
	int		k;
	double dist = 0.0;

	for (k = 0 ; k < NDIM ; k++) {
		vec[k] = p0[k] - p1[k];
		dist += (vec[k]*vec[k]);
	}
	dist = sqrt(dist);
	return dist;
}

char *zmalloc(n)
int		n;
{
	char		*rv;
	rv = malloc(n);
	if (rv == NULL) {fprintf(stderr,"out of memory\n"); abort();}
	memset(rv,0,n);
	return rv;
}

double **
new_array(m, n, ival)
int		m,n;
double	ival;
{
	double		**rv;
	int			i,j;

	rv = N_NEW(m,double*);
	for (i = 0; i < m; i++) {
		rv[i] = N_NEW(n,double);
		for (j = 0; j < n; j++) rv[i][j] = ival;
	}
	return rv;
}

double ***
new_3array(m, n, p, ival)
int		m,n,p;
double	ival;
{
	double		***rv;
	int			i,j,k;

	rv = N_NEW(m,double**);
	for (i = 0; i < m; i++) {
		rv[i] = N_NEW(n,double*);
		for (j = 0; j < n; j++) {
			rv[i][j] = N_NEW(p,double);
			for (k = 0;  k < p; k++) rv[i][j][k] = ival;
		}
	}
	return rv;
}

void
free_array(ptr, m, n)
double	**ptr;
int		m,n;
{
	int			i;
	for (i = 0; i < m; i++) free(ptr[i]);
	free(ptr);
	n = n;
}

neato_write(g)
graph_t	*g;
{
	spline_edges(g);
	dot_write(g);
}

void
intr(s)
int		s;
{
	if (G) neato_write(G);
	terminate();
	exit(1);
}

double
floatattr(obj,index,defval)
void	*obj;
int		index;
double	defval;
{
	double	val;
	if (index < 0) return defval;
	if (sscanf(agxget(obj,index),"%lf",&val) < 1) return defval;
	return val;
}

static int	 Tflag;
void
toggle(s)
int		s;
{
	Tflag = !Tflag;
	signal (SIGUSR1, toggle);
}

main(argc,argv)
int		argc;
char	**argv;
{
	initialize(argc,argv);
	signal (SIGUSR1, toggle);
	signal (SIGINT, intr);
	while (G = input_graph()) {
		scan_graph();
		shortest_path();
		initial_positions();
		diffeq_model();
		solve_model();
		final_energy();
		neato_write(G);
	}
	terminate();
}

void
scan_graph()
{
	int		i,lenx,wtx;
	char	*str;
	float	wt,len;
	node_t	*np,*xp;
	edge_t	*ep;

	if (Reduce) {
		for (np = agfstnode(G); np; np = xp) {
			xp = agnxtnode(G,np);
			ep = agfstedge(G,np);
			if (ep == NULL) agdelete(G,np);
			else {if (agnxtedge(G,ep,np) == NULL) prune(np,ep);}
		}
	}
	nG = agnnodes(G);
	if (str = agget(G,"maxiter")) MaxIter = atoi(str);
	else MaxIter = MAXINT;
	lenx = agindex(G->proto->e,"len");
	wtx = agindex(G->proto->e,"w");
	G->u.nlist = N_NEW(nG + 1,node_t*);
	for (i = 0, np = agfstnode(G); np; np = agnxtnode(G,np)) {
		G->u.nlist[i] = np;
		np->u.id = i++;
		np->u.heapindex = -1;
		for (ep = agfstout(G,np); ep; ep = agnxtout(G,ep)) {
			wt = floatattr(ep,wtx,1.0);
			len = floatattr(ep,lenx,1.0);
			ep->u.factor = wt;
			ep->u.dist = len;
		}
	}
	G->u.dist = new_array(nG,nG,MYHUGE);
	G->u.spring = new_array(nG,nG,1.0);
	G->u.sum_t = new_array(nG,nG,1.0);
	G->u.t = new_3array(nG,nG,NDIM,0.0);
}

jitter3d(np)
node_t	*np;
{
	int		k;
	for (k = 2; k < NDIM; k++) np->u.pos[k] = nG*drand48();
}

randompos(np)
node_t	*np;
{
	np->u.pos[0] = nG * drand48();
	np->u.pos[1] = nG * drand48();
	if (NDIM > 2) jitter3d(np);
}

int user_pos(symptr,np)
attrsym_t	*symptr;
node_t		*np;
{
	double		*pvec;
	char		*p,c;

	if (symptr == NULL) return FALSE;
	pvec = np->u.pos;
	p = agxget(np,symptr->index);
	if (p[0]) {
		c = '\0';
		if (sscanf(p,"%lf,%lf%c",pvec,pvec+1,&c) >= 2) {
			if (NDIM > 2) jitter3d(np);
			if (c == '!') np->u.pinned = TRUE;
			return TRUE;
		}
		else fprintf(stderr,"node %s, position %s, expected two floats\n",
			np->name,p);
	}
	return FALSE;
}

initial_positions()
{
	int			i;
	unsigned	seed;
	double		a,da;
	node_t		*np;
	attrsym_t	*symptr;
	char		*p;

	symptr = agfindattr(G->proto->n,"pos");

	seed = 1;
	if (p = agget(G,"start")) {
		if (sscanf(p,"%d",&seed) < 1) 
			if (!strcmp(p,"regular")) {
				a = 0.0;
				da = (2 * PI) / nG;
				for (i = 0; np = G->u.nlist[i]; i++) {
					if (user_pos(symptr,np)) continue;
					np->u.pos[0] = nG * Spring_coeff * cos(a);
					np->u.pos[1] = nG * Spring_coeff * sin(a);
					a = a + da;
					if (NDIM > 2) jitter3d(np);
				}
			}
			else 
#ifdef DOS
			seed = (unsigned) time(NULL);
#else
			seed = (unsigned) getpid() ^ (unsigned) time(NULL);
#endif
	}
	srand48(seed);
	for (i = 0; np = G->u.nlist[i]; i++) {
		if (user_pos(symptr,np)) continue;
		randompos(np);
	}
}

diffeq_model()
{
	int			i,j,k;
	char		*p,c;
	double		dist,**D,**K,del[NDIM],f;
	node_t		*vi,*vj;
	edge_t		*e;


	/* init springs */
	K = G->u.spring;
	D = G->u.dist;
	for (i = 0; i < nG; i++) {
		for (j = 0; j < i; j++) {
			f = Spring_coeff / (D[i][j]*D[i][j]);
			if (e = agfindedge(G,G->u.nlist[i],G->u.nlist[j]))
				f = f * e->u.factor;
			K[i][j] = K[j][i] = f;
		}
	}

	/* init differential equation solver */
    for (i = 0; i < nG; i++)
        for (k = 0 ; k < NDIM ; k++)
            G->u.sum_t[i][k] = 0.0 ;

    for (i = 0; vi = G->u.nlist[i]; i++) {
        for (j = 0; j < nG; j++) {
            if (i == j) continue;
            vj = G->u.nlist[j];
			dist = distvec(vi->u.pos,vj->u.pos,del);
            for (k = 0 ; k < NDIM ; k++) {
                G->u.t[i][j][k] =
					G->u.spring[i][j]*(del[k] - G->u.dist[i][j]*del[k]/dist);
                G->u.sum_t[i][k] += G->u.t[i][j][k];
            }
        }
    }
}

void
solve_model()
{
	node_t	*np;
	while (np = choose_node()) {
		move_node(np);
	}
}

void update_arrays(g,i)
graph_t *g;
int     i;
{
    int     j,k;
    double  del[NDIM],dist,old;
    node_t  *vi,*vj;

    vi = g->u.nlist[i];
    for (k = 0 ; k < NDIM ; k++ ) G->u.sum_t[i][k] = 0.0;
    for (j = 0; j < nG; j++) {
        if (i == j) continue;
        vj = g->u.nlist[j];
		dist = distvec(vi->u.pos,vj->u.pos,del);
        for (k = 0 ; k < NDIM ; k++) {
            old = G->u.t[i][j][k];
            G->u.t[i][j][k] =
				G->u.spring[i][j]*(del[k] - G->u.dist[i][j]*del[k]/dist);
            G->u.sum_t[i][k] += G->u.t[i][j][k];
            old = G->u.t[j][i][k];
            G->u.t[j][i][k] = - G->u.t[i][j][k];
            G->u.sum_t[j][k] += (G->u.t[j][i][k] - old);
        }
    }
}

void
D2E(g, n, M)
graph_t *g;
int     n;
double  M[NDIM][NDIM];
{
    int     i,l,k;
    node_t  *vi,*vn;
    double  scale,sq, t[NDIM];
    double  **K = g->u.spring;
    double  **D = g->u.dist;

    vn = g->u.nlist[n];
    for (l = 0 ; l < NDIM ; l++)
        for (k = 0 ; k < NDIM ; k++)
            M[l][k] = 0.0;
    for (i = 0; i < nG; i++) {
        if (n == i) continue;
        vi = g->u.nlist[i];
        sq = 0.0;
        for (k = 0 ; k < NDIM ; k++) {
            t[k] = vn->u.pos[k] - vi->u.pos[k];
            sq += (t[k]*t[k]);
        }
        scale = 1 / fpow32(sq);
        for (k = 0 ; k < NDIM ; k++) {
            for (l = 0 ; l < k ; l++)
                M[l][k] += K[n][i] * D[n][i]*t[k]*t[l]*scale;
            M[k][k] += K[n][i] *(1.0 - D[n][i]*(sq-(t[k]*t[k]))*scale);
        }
    }
    for (k = 1 ; k < NDIM ; k++)
        for (l = 0 ; l < k ; l++)
            M[k][l] = M[l][k];
}

node_t *
choose_node()
{
    int         i,k;
    double      m,max;
    node_t      *choice,*np;
	static int	cnt = 0;

	if (G->u.move >= MaxIter) return NULL;
    max = 0.0;
    choice = NULL;
    for (i = 0; i < nG; i++) {
		np = G->u.nlist[i];
		if (np->u.pinned) continue;
        for (m = 0.0, k = 0 ; k < NDIM ; k++ )
			m += (G->u.sum_t[i][k] * G->u.sum_t[i][k]);
		/* could set the color=energy of the node here */
        if (m > max) {choice = np; max = m;}
    }
	max = sqrt(max);
    if (max < Epsilon) choice = NULL;
	else {
		if (Verbose && (++cnt % 100 == 0)) {
			fprintf(stderr,"%.3lf ",max);
			if (cnt % 1000 == 0) fprintf(stderr,"\n");
		}
	}
    return choice;
}

void
move_node(n)
node_t  *n;
{
    int     i,m;
    double  a[NDIM][NDIM],b[NDIM],c[NDIM],sum=0;
    edge_t  *e;

    m = n->u.id;
    D2E(G,m,a);
    for (i = 0 ; i < NDIM ; i++) c[i] = - G->u.sum_t[m][i];
    solve(a,b,c,NDIM);
    for (i = 0 ; i < NDIM ; i++) { n->u.pos[i] += b[i]; sum += fabs(b[i]); }
	sum = sqrt(sum);
	G->u.move++;
    update_arrays(G,m);
	avoid_cycling(n,b);
	if (Tflag) fprintf(stderr,"%s %.3lf\n",n->name,sum);
}

int
prune(leaf,e)
node_t	*leaf;
edge_t	*e;
{
	node_t	*other;
	edge_t	*ep;

	if ((other = e->tail) == leaf) other = e->head;
	agdelete(G,leaf);
	ep = agfstedge(G,other);
	if ((ep == NULL) || (agnxtedge(G,ep,other) == NULL)) prune(other,ep);
}

avoid_cycling(n,b)
node_t	*n;
double	*b;
{
	static double	b1[NDIM];
	static node_t	*v;
	static int		ctr  = 0;
	double			sum = 0.0;
	int				i;

	if (v == n) {
		for (i = 0; i < NDIM; i++) {sum += (b1[i] + b[i]); b1[i] = b[i];}
		if (fabs(sum) < .001) {if (ctr++ > 5) G->u.move = MaxIter;}
		else ctr = 0;
	}
	v = n;
}

static node_t		**Heap;
static int			Heapsize;
static node_t		*Src;

void
heapup(v)
node_t		*v;
{
	int		i,par;
	node_t	*u;

	for (i = v->u.heapindex; i > 0; i = par) {
		par = (i - 1) / 2;
		u = Heap[par];
		if (u->u.dist <= v->u.dist) break;
		Heap[par] = v; v->u.heapindex = par;
		Heap[i] = u; u->u.heapindex = i;
	}
}

void
heapdown(v)
node_t		*v;
{
	int		i,left,right,c;
	node_t	*u;

	i = v->u.heapindex;
	while ((left = 2 * i + 1) < Heapsize) {
		right = left + 1;
		if ((right < Heapsize) && (Heap[right]->u.dist < Heap[left]->u.dist))
			c = right;
		else c = left;
		u = Heap[c];
		if (v->u.dist <= u->u.dist) break;
		Heap[c] = v; v->u.heapindex = c;
		Heap[i] = u; u->u.heapindex = i;
		i = c;
	}
}


enqueue(v)
node_t		*v;
{
	int		i;

	assert(v->u.heapindex < 0);
	i = Heapsize++;
	v->u.heapindex = i;
	Heap[i] = v;
	if (i > 0) heapup(v);
}

node_t	*
dequeue()
{
	int		i;
	node_t	*rv,*v;

	if (Heapsize == 0) return NULL;
	rv = Heap[0];
	i = --Heapsize;
	v = Heap[i];
	Heap[0] = v;
	v->u.heapindex = 0;
	if (i > 1) heapdown(v);
	rv->u.heapindex = -1;
	return rv;
}

void
shortest_path()
{
	node_t		*v;

	Heap = N_NEW(nG+1,node_t*);
	for (v = agfstnode(G); v; v = agnxtnode(G,v)) s1(v);
	free(Heap);
}

s1(node)
node_t	*node;
{
	node_t		*v,*u;
	edge_t		*e;
	int			t;
	double		f;

	for (t = 0; v = G->u.nlist[t]; t++) v->u.dist = MYHUGE;
	Src = node;
	Src->u.dist = 0;
	Src->u.hops = 0;
	enqueue(Src);

	while (v = dequeue()) {
		if (v != Src) make_spring(Src,v,v->u.dist);
		for (e = agfstedge(G,v); e; e = agnxtedge(G,e,v)) {
			if ((u = e->head) == v) u = e->tail;
			f = v->u.dist + e->u.dist;
			if (u->u.dist > f) {
				u->u.dist = f;
				if (u->u.heapindex >= 0) heapup(u);
				else {
					u->u.hops = v->u.hops + 1;
					enqueue(u);
				}
			}
		}
	}
}

make_spring(u,v,f)
node_t	*u,*v;
double	f;
{
	int		i,j;

	i = u->u.id;
	j = v->u.id;
	G->u.dist[i][j] = G->u.dist[j][i] = f;
}

allow_edits(nsec)
int		nsec;
{
#ifdef INTERACTIVE
	static int		onetime = TRUE;
	static FILE		*fp;
	static fd_set	fd;
	static struct timeval tv;

	char			buf[256],name[256];
	double			x,y;
	node_t			*np;

	if (onetime) {
		fp = fopen("/dev/tty","r");
		if (fp == NULL) exit(1);
		setbuf(fp,NULL);
		tv.tv_usec = 0;
		onetime = FALSE;
	}
	tv.tv_sec = nsec;
	FD_ZERO(&fd);
	FD_SET(fileno(fp),&fd);
	if (select(32,&fd,(fd_set*)0,(fd_set*)0,&tv) > 0) {
		fgets(buf,sizeof(buf),fp);
		switch(buf[0]) {
		case 'm':	/* move node */
			if (sscanf(buf+1,"%s %lf%lf",name,&x,&y) == 3) {
				np = getnode(G,name);
				if (np) {
					np->u.pos[0] = x;
					np->u.pos[1] = y;
					diffeq_model();
				}
			}
			break;
		case 'q':
			return FALSE;
		default:
			fprintf(stderr,"unknown command '%s', ignored\n",buf);
		}
		return TRUE;
	}
#endif
	return FALSE;
}

final_energy()
{
	int			i,j,d;
	double		e = 0.0,t0,t1;
	node_t		*ip,*jp;

	if (Verbose) {
		for (i = 0; i < nG - 1; i++) {
			ip = G->u.nlist[i];
			for (j = i+1; j < nG; j++) {
				jp = G->u.nlist[j];
				for (t0 = 0.0, d = 0; d < NDIM; d++) {
					t1 = (ip->u.pos[d] - jp->u.pos[d]);
					t0 = t1 * t1;
				}
				e = e + .5 * G->u.spring[i][j] *
					(t0 + G->u.dist[i][j] * G->u.dist[i][j]
					- 2.0 * G->u.dist[i][j] * sqrt(t0));
			}
		}
		fprintf(stderr,"final e = %lf\n",e);
	}
}
