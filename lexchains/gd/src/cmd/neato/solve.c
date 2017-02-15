/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/* solves the system ab=c using gauss reduction */
#include <math.h>
#include <malloc.h>
void
solve(a,b,c,n)
double *a,*b,*c; /*a[n][n],b[n],c[n]*/
int n;
{
	double *asave,*csave;
	double amax,dum,pivot;
	register int i,ii,j;
	register int k,m,mp;
	register int istar,ip;
	register int nm,nsq,t;

	nsq = n * n;
	asave = (double *) malloc(sizeof(double)*nsq);
	csave = (double *) malloc(sizeof(double)*n);

	for (i = 0; i < n; i++) csave[i] = c[i];
	for (i = 0; i < nsq; i++) asave[i] = a[i];
	/* eliminate ith unknown */
	nm=n-1;
	for (i = 0; i < nm; i++)
	{
		/* find largest pivot */
		amax=0.;
		for (ii = i; ii < n; ii++)
		{
			dum = fabs(a[ii * n + i]);
			if (dum < amax) continue;
      		istar=ii;
      		amax=dum;
		}
		/* return if pivot is too small */
      	if (amax < 1.e-10) goto bad;
		/* switch rows */
		for (j = i; j < n; j++)
		{
			t = istar * n + j;
			dum=a[t];
			a[t]=a[i * n + j];
			a[i * n + j] = dum;
		}
		dum=c[istar];
		c[istar]=c[i];
		c[i]=dum;
		/*pivot*/
		ip=i + 1;
		for (ii = ip; ii < n; ii++)
		{
			pivot=a[ii * n + i]/a[i * n + i];
			c[ii]=c[ii]-pivot*c[i];
			for (j = 0; j < n; j++)
				a[ii * n + j]=a[ii * n + j]-pivot*a[i * n + j];
		}
	}
	/* return if last pivot is too small */
	if(fabs(a[nsq - 1]) < 1.e-10) goto bad;
	b[n - 1]=c[n - 1]/a[nsq - 1];
	/* back substitute */
	for (k = 0; k < nm; k++)
	{
		m = n - k - 2;
		b[m]=c[m];
		mp=m+1;
		for (j = mp; j < n; j++)
			b[m]=b[m]-a[m*n+j]*b[j];
		b[m]=b[m]/a[m*n+m];
	}
	/* restore original a,c */
	for (i = 0; i < n; i++)
		c[i]=csave[i];
	for (i = 0; i < nsq; i++)
		a[i]=asave[i];
	free(asave); free(csave);
	return;
bad:
	printf("ill-conditioned\n");
	free(asave); free(csave);
	return;
}
