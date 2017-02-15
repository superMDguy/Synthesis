/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include <structs.h>
#include <graph.h>
#include <tcl.h>

extern codegen_t *CodeGen, TK_CodeGen;
extern FILE    *Output_file;

render(interp, g, canvasHandle)
    Tcl_Interp     *interp;
    Agraph_t       *g;
    char           *canvasHandle;
{
    Output_file = (FILE *) interp;
    CodeGen = &TK_CodeGen;
    emit_graph(g);
}
