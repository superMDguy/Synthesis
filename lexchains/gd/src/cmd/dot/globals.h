/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#ifndef EXTERN
#define EXTERN extern
#endif

EXTERN char		**Files;			/* from command line */
EXTERN char		**Lib;				/* from command line */
EXTERN boolean	Verbose;			/* print informative messages */
EXTERN int		Output_lang;		/* POSTSCRIPT, DOT, etc. */
EXTERN codegen_t	*CodeGen;
EXTERN FILE		*Output_file;
extern char		*Version;			/* from dot.c */

EXTERN int		Show_boxes;			/* emit code for correct box coordinates */
EXTERN int		CL_type;			/* NONE, LOCAL, GLOBAL */
EXTERN boolean	Concentrate;		/* if parallel edges should be merged */

EXTERN attrsym_t	
					*N_height, *N_width, *N_shape, *N_color,
					*N_fontsize, *N_fontname, *N_fontcolor,
					*N_label, *N_style, *N_showboxes,
					*N_sides,*N_peripheries,*N_orientation,
					*N_skew,*N_distortion,*N_fixed,*N_layer,
					*N_group,*N_comment;

EXTERN	attrsym_t	*E_weight, *E_minlen, *E_color,
					*E_fontsize, *E_fontname, *E_fontcolor,
					*E_label, *E_dir, *E_style, *E_decorate,
					*E_showboxes,*E_arrowsz,*E_constr,*E_layer,
					*E_comment;
