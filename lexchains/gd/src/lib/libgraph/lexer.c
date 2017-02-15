/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/*
 * Written by Stephen North.  3/1/93 release.
 * the lexer.  should probably use flex instead.
 */

#include "libgraph.h"
#include "parser.h"
#include "triefa.c"

static FILE		*Lexer_fp;
static char		*LexPtr,*TokenBuf;
static int		LineBufSize;
static uchar	In_comment;
static uchar	Comment_start;
static int		Line_number;

aglexinit(fp)
     FILE *fp;
{
	Lexer_fp = fp;
	LexPtr = NULL;
	if (AG.linebuf == NULL) {
		LineBufSize = BUFSIZ;
		AG.linebuf = N_NEW(LineBufSize,char);
		TokenBuf = N_NEW(LineBufSize,char);
	}
}

/* skip leading white space and comments in a string p */
static char *
skip_wscomments(p)
     char *p;
{
	do {
		while (isspace(*p)) p++;
		while (In_comment && p[0]) {
			while (p[0] && (p[0] != '*')) p++;
			if (p[0]) {
				if (p[1] == '/') {In_comment = FALSE; p += 2; break;}
				else p++;
			}
		}
		if (p[0] == '/') {
			if (p[1] == '/') while (*p) p++;	/* skip to end of line */
			else {
				if (p[1] == '*') {
					In_comment = TRUE; Comment_start = Line_number;
					p += 2; continue;
				}
				else break;	/* return a slash */
			}
		}
		else {if (!isspace(*p)) break;}
	} while (p[0]);
	return p;
}

/* scan an unquoted token and return the position after its terminator */
static char *
scan_token(p, token)
     char *p;
     char *token;
{
	char 	*q;

	q = token;
	if (p == '\0') return NULL;
	while (isalnum(*p) || (*p == '_')) {
		*q++ = *p++;
	}
	*q = '\0';
	return p;
}

static char *
scan_num(p, token)
     char *p;
     char *token;
{
	char 	*q,*z;
	int		saw_rp = FALSE;
	int		saw_digit = FALSE;

	z = p;
	q = token;
	if (*z == '-') *q++ = *z++;
	if (*z == '.') {saw_rp = TRUE; *q++ = *z++;}
	while (isdigit(*z)) {saw_digit = TRUE; *q++ = *z++;}
	if ((*z == '.') && (saw_rp == FALSE)) {
		saw_rp = TRUE; *q++ = *z++;
		while (isdigit(*z)) {saw_digit = TRUE; *q++ = *z++;}
	}
	*q = '\0';
	if (saw_digit && *z && (isalpha(*z)))
		agerror("badly formed number %s",p);

	if (saw_digit == FALSE) z = NULL;
	return z;
}

/* scan a quoted string and return the position after its terminator */
static char *
quoted_string(p, token)
     char *p;
     char *token;
{
	char		quote,*q;

	quote = *p++;
	q = token;
	while ((*p) && (*p != quote)) {
		if (*p == '\\') {
			if (*(p+1) == quote) p++;
			else {if (*(p+1) == '\\') *q++ = *p++;}
		}
		*q++ = *p++;
	}
	if (*p == '\0') agerror("string ran past end of line","");
	else p++;
	*q = 0;
	return p;
}

int myaglex()
 {		/* for debugging */
	int rv = aglex();
	fprintf(stderr,"returning %d\n",rv);
	if (rv == T_symbol) fprintf(stderr,"string val is %s\n",aglval.str);
	return rv;
}

static char *
lex_gets()
{
	char	*clp;
	int		len,curlen;

	curlen = 0;

	do {
		/* make sure there is room for at least another SMALLBUF worth */
		if (curlen + SMALLBUF >= LineBufSize) {
			LineBufSize += BUFSIZ;
			AG.linebuf = realloc(AG.linebuf,LineBufSize);
			TokenBuf = realloc(TokenBuf,LineBufSize);
		}

			/* off by one so we can back up in LineBuf */
		clp = fgets(AG.linebuf + curlen + 1, LineBufSize-curlen-1 , Lexer_fp);
		if (clp == NULL) break;

		if (clp[0] == '#') {
			/* comment line or cpp line sync */
			if (sscanf(clp+1,"%d",&Line_number) == 0) Line_number++;
			clp[0] = 0;
			continue;
		}

		Line_number++;
		if ((len = strlen(clp)) > 1) {
			if (clp[len - 2] == '\\') {
				len = len - 2;
				clp[len] = '\0';
			}
		}
		curlen += len;
	} while (clp[len-1] != '\n');

	if (curlen > 0) return AG.linebuf + 1;
	else return NULL;
}

int agtoken(p)
     char *p;
{
	TFA_Init();
	while (*p) {
		TFA_Advance(*p++);
	}
	return TFA_Definition();
}

int aglex()
{
	int		token;
	char	*tbuf,*p;
	static char *handle;

	agstrfree(handle); handle = NULL;
	/* if the parser has accepted a graph, reset and return EOF */
	if (AG.accepting_state) {
		AG.accepting_state = FALSE;
		return EOF;
	}

	/* get a nonempty lex buffer */
	do {
		if ((LexPtr == NULL) || (LexPtr[0] == '\0'))
			if ((LexPtr = lex_gets()) == NULL) {
				if (In_comment) fprintf(stderr,"warning, nonterminated comment in line %d\n",Comment_start);
				return EOF;
			}
		LexPtr = skip_wscomments(LexPtr);
	} while (LexPtr[0] == '\0');

	tbuf = TokenBuf;

	/* scan quoted strings */
	if (LexPtr[0] == '\"') {
		LexPtr = quoted_string(LexPtr,tbuf);
		handle = aglval.str = agstrdup(tbuf);
		return T_symbol;
	}

	/* scan edge operator */
	if (AG.edge_op && (strncmp(LexPtr,AG.edge_op,strlen(AG.edge_op)) == 0)) {
		LexPtr += strlen(AG.edge_op);
		return T_edgeop;
	}

	/* scan numbers */
	if (p = scan_num(LexPtr,tbuf)) {
		LexPtr = p;
		handle = aglval.str = agstrdup(tbuf);
		return T_symbol;
	}
	else {
		if (ispunct(LexPtr[0]) && (LexPtr[0] != '_'))
			return *LexPtr++;
		else LexPtr = scan_token(LexPtr,tbuf);
	}

	/* scan other tokens */
	token = agtoken(tbuf);
	if (token == -1) {
		handle = aglval.str = agstrdup(tbuf);
		token = T_symbol;
	}
	return token;
}

static void error_context()
{
	char *p,*q;

	if (LexPtr == NULL) return;
	fprintf(stderr,"context: ");
	for (p = LexPtr - 1; (p > AG.linebuf) && (isspace(*p) == FALSE); p--);
	for (q = AG.linebuf; q < p; q++) fputc(*q,stderr);
	fputs(" >>> ",stderr);
	for (; q < LexPtr; q++) fputc(*q,stderr);
	fputs(" <<< ",stderr);
	fputs(LexPtr,stderr);
}

void agerror(fmt, s)
char		*fmt,*s;
{
	if (AG.syntax_errors++) return;
	fprintf(stderr, "graph parser: ");
	fprintf(stderr, fmt, s);
	fprintf(stderr, " near line %d\n",Line_number);
	error_context();
}
