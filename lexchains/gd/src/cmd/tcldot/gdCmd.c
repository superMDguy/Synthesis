/* 
 * gdCmd.c - Interface gd to Tcl.
 * 
 * Author:	Spencer W. Thomas
 * 		Information Technology and Networking
 * 		University of Michigan
 * Date:	Fri Aug 19 1994
 * Copyright (c) 1994, University of Michigan
 */

#include <stdio.h>
#include <tcl.h>
#include "gd.h"
#include "gdfonts.h"
#include "gdfontl.h"

/* Fonts table. */
static struct {
    char *fontname;
    gdFontPtr *fontp;
} fontTbl[] = {
    "small",	&gdFontSmall,
    "large",	&gdFontLarge
};

void *GDHandleTable = 0;

static int tclGdCreateCmd(), tclGdDestroyCmd(), tclGdWriteCmd(),
    tclGdColorCmd(), tclGdInterlaceCmd(), tclGdSetCmd(), tclGdLineCmd(),
    tclGdRectCmd(), tclGdArcCmd(), tclGdFillCmd(), tclGdSizeCmd(),
    tclGdFontsCmd(), tclGdTextCmd(), tclGdCopyCmd(), tclGdGetCmd(),
    tclGdBrushCmd(), tclGdStyleCmd(), tclGdTileCmd(), tclGdPolygonCmd();
static int tclGdColorNewCmd(), tclGdColorExactCmd(),
    tclGdColorClosestCmd(), tclGdColorFreeCmd(),
    tclGdColorTranspCmd(), tclGdColorGetCmd();

typedef  struct {
    char *cmd;
    int (*f)();
    int minargs, maxargs;
    int subcmds;
    int ishandle;
    char *usage;
} cmdOptions;

static cmdOptions subcmdVec[] = {
    {"create",		tclGdCreateCmd,		2, 2,	0, 0,
    	"width height"},
    {"createFromGIF",	tclGdCreateCmd, 	1, 1,	0, 0,
    	"filehandle"},
    {"createFromGD",	tclGdCreateCmd, 	1, 1,	0, 0,
    	"filehandle"},
    {"createFromXBM",	tclGdCreateCmd, 	1, 1,	0, 0,
    	"filehandle"},

    {"destroy",		tclGdDestroyCmd,	1, 1,	0, 1,
    	"gdhandle"},

    {"writeGIF",	tclGdWriteCmd,		2, 2,	0, 1,
    	"gdhandle filehandle"},
    {"writeGD",		tclGdWriteCmd,		2, 2,	0, 1,
    	"gdhandle filehandle"},

    {"interlace",	tclGdInterlaceCmd,	1, 2,	0, 1,
	 "gdhandle ?on-off?"},

    {"color",		tclGdColorCmd,		2, 5,	1, 1,
    	"option values..."},
    {"brush",		tclGdBrushCmd,		2, 2,	0, 2,
    	"gdhandle brushhandle"},
    {"style",		tclGdStyleCmd,		2, 999,	0, 1,
    	"gdhandle color..."},
    {"tile",		tclGdTileCmd,		2, 2,	0, 2,
    	"gdhandle tilehandle"},

    {"set",		tclGdSetCmd,		4, 4,	0, 1,
	"gdhandle color x y"},
    {"line",		tclGdLineCmd,		6, 6,	0, 1,
    	"gdhandle color x1 y1 x2 y2"},
    {"rectangle",	tclGdRectCmd,		6, 6,	0, 1,
    	"gdhandle color x1 y1 x2 y2"},
    {"fillrectangle",	tclGdRectCmd,		6, 6,	0, 1,
    	"gdhandle color x1 y1 x2 y2"},
    {"arc",		tclGdArcCmd,		8, 8, 	0, 1,
    	"gdhandle color cx cy width height start end"},
    {"fillarc",		tclGdArcCmd,		8, 8, 	0, 1,
    	"gdhandle color cx cy width height start end"},
    {"polygon",		tclGdPolygonCmd,	2, 999, 0, 1,
    	"gdhandle color x1 y1 x2 y2 x3 y3 ..."},
    {"fillpolygon",	tclGdPolygonCmd,	3, 999, 0, 1,
    	"gdhandle color x1 y1 x2 y2 x3 y3 ..."},
    {"fill",		tclGdFillCmd,		4, 5,	0, 1,
    	"gdhandle color x y ?bordercolor?"},
    {"text",		tclGdTextCmd,		6, 6,	0, 1,
    	"gdhandle color fontname x y string"},
    {"textup",		tclGdTextCmd,		6, 6,	0, 1,
    	"gdhandle color fontname x y string"},

    {"copy",		tclGdCopyCmd,		8, 10,	0, 2,
    	"desthandle srchandle destx desty srcx srcy destw desth ?srcw srch?"},

    {"get",		tclGdGetCmd,		3, 3,	0, 1,
    	"gdhandle x y"},
    {"size",		tclGdSizeCmd,		1, 1,	0, 1,
    	"gdhandle"},
    {"fonts",		tclGdFontsCmd,		0, 0,	0, 0,
    	""}
};

static cmdOptions colorCmdVec[] = {
    {"new",		tclGdColorNewCmd,	5, 5,	1, 1,
    	"gdhandle red green blue"},
    {"exact",		tclGdColorExactCmd,	5, 5,	1, 1,
    	"gdhandle red green blue"},
    {"closest",		tclGdColorClosestCmd,	5, 5,	1, 1,
    	"gdhandle red green blue"},
    {"free",		tclGdColorFreeCmd,	3, 3,	1, 1,
    	"gdhandle color"},
    {"transparent",	tclGdColorTranspCmd,	2, 3,	1, 1,
    	"gdhandle ?color?"},
    {"get",		tclGdColorGetCmd,	2, 3,	1, 1,
    	"gdhandle ?color?"}
};

/*
 * Helper function to interpret color index values.
 */
static int
tclGd_GetColor( Tcl_Interp *interp, char *arg, int *color )
{
    int nlist, retval = TCL_OK;
    char **theList;

    /* Assume it's an integer, check other cases on failure. */
    if ( Tcl_GetInt( interp, arg, color ) == TCL_OK )
	return TCL_OK;
    else
    {
	Tcl_ResetResult( interp );
	Tcl_SplitList( interp, arg, &nlist, &theList );
	if (nlist < 1 || nlist > 2)
	    retval = TCL_ERROR;
	else
	{
	    switch ( theList[0][0] ) {
	    case 'b':
		*color = gdBrushed;
		if ( nlist == 2 )
		    if ( theList[1][0] == 's' )
			*color = gdStyledBrushed;
		    else
			retval = TCL_ERROR;
		break;

	    case 's':
		*color = gdStyled;
		if ( nlist == 2 )
		    if ( theList[1][0] == 'b' )
			*color = gdStyledBrushed;
		    else
			retval = TCL_ERROR;
		break;

	    case 't':
		*color = gdTiled;
		break;

	    default:
		retval = TCL_ERROR;
	    }
	}
    }
    if ( retval == TCL_ERROR )
	sprintf( interp->result, "Malformed special color value" );

    return retval;
}

/*
 * GD composite command:
 *
 * gd create <width> <height>
 * 	Return a handle to a new gdImage that is width X height.
 * gd createFromGIF <filehandle>
 * gd createFromGD <filehandle>
 * gd createFromXBM <filehandle>
 * 	Return a handle to a new gdImage created by reading a GIF
 * 	(resp. GD or XBM) image from the file open on filehandle.
 *
 * gd destroy <gdhandle>
 * 	Destroy the gdImage referred to by gdhandle.
 *
 * gd writeGIF <gdhandle> <filehandle>
 * gd writeGD  <gdhandle> <filehandle>
 * 	Write the image in gdhandle to filehandle as a GIF (resp. GD)
 * 	file.
 *
 * gd color new <gdhandle> <red> <green> <blue>
 * 	Allocate a new color with the given RGB values.  Return the color
 * 	index.  Returns -1 on failure.
 * gd color exact <gdhandle> <red> <green> <blue>
 * gd color closest <gdhandle> <red> <green> <blue>
 * 	Find a color in the image that exactly matches (resp., is closest to)
 * 	the given RGB color.  Returns the index (or -1 if no exact match).
 * gd color free <gdhandle> <index>
 * 	Free the color at the given index for reuse.
 * gd color transparent <gdhandle> <index>
 * 	Mark the color index as the transparent background color.
 * gd color get <gdhandle> [<index>]
 * 	Return the RGB value at <index>, or {} if it is not allocated.
 * 	If <index> is not specified, return a list of {index R G B} values
 * 	for all allocated colors.
 * gd color gettransparent <gdhandle>
 * 	Return the transparent color index.
 * 
 * gd brush <gdhandle> <brushhandle>
 * 	Set the brush image to be used for brushed lines.  Transparent
 * 	pixels in the brush will not change the image when the brush
 * 	is applied. 
 * gd style <gdhandle> <index> ...
 * 	Set the line style to the list of color indices.  This is interpreted
 * 	in one of two ways.  For a simple styled line, each color index is
 * 	applied to points along the line in turn.  The transparent index
 * 	value may be used to leave gaps in the line.  For a styled, brushed
 * 	line, a 0 (or the transparent index) means not to fill the pixel,
 * 	and a non-zero value means to apply the brush.
 * gd tile <gdhandle> <tilehandle>
 * 	Set the tile image to be used for tiled fills.  Transparent pixels in
 * 	the tile will not change the underlying image during tiling.
 * 
 * In all drawing functions, the color index is a number, or may be one of the
 * strings styled, brushed, tiled, "styled brushed" or "brushed styled".  The
 * style, brush, or tile currently in effect will be used.  Brushing and
 * styling apply to lines, tiling to filled areas.
 * 
 * gd set <gdhandle> <index> <x> <y>
 * 	Set the pixel at (x,y) to index.
 * gd line <gdhandle> <index> <x1> <y1> <x2> <y2>
 * 	Draw a line in color <index> from (x1,y1) to (x2,y2).
 * gd rectangle <gdhandle> <index> <x1> <y1> <x2> <y2>
 * gd fillrectangle <gdhandle> <index> <x1> <y1> <x2> <y2>
 * 	Draw the outline of (resp. fill) a rectangle in color <index>
 * 	with corners at (x1,y1) and (x2,y2).
 * gd arc <gdhandle> <index> <cx> <cy> <width> <height> <start> <end>
 * gd fillarc <gdhandle> <index> <cx> <cy> <width> <height> <start> <end>
 * 	Draw an arc, or filled segment, in color <index>, centered at (cx,cy)
 *      in a rectangle width x height, starting at start degrees and ending 
 *      at end degrees.	Start must be > end.
 * gd polygon <gdhandle> <index> <x1> <y1> ...
 * gd fillpolygon <gdhandle> <index> <x1> <y1> ...
 * 	Draw the outline of, or fill, a polygon specified by the x, y
 * 	coordinate list.  
 * 
 * gd fill <gdhandle> <index> <x> <y>
 * gd fill <gdhandle> <index> <x> <y> <borderindex>
 * 	Fill with color index, starting from (x,y) within a region
 * 	of pixels all the color of the pixel at (x,y) (resp., within a
 * 	border colored borderindex).
 *
 * gd size <gdhandle>
 * 	Returns a list {width height} of the image.
 *
 * gd fonts
 * 	Returns a list of available fonts: {fontname width height}
 *
 * gd text <gdhandle> <fontname> <index> <x> <y> <string>
 * gd textup <gdhandle> <fontname> <index> <x> <y> <string>
 * 	Draw text with font fontname, in color index, with upper left
 * 	corner at (x,y).  textup draws text rotated 90 degrees, running up.
 *
 * gd copy <desthandle> <srchandle> <destx> <desty> <srcx> <srcy> <w> <h>
 * gd copy <desthandle> <srchandle> <destx> <desty> <srcx> <srcy> \
 * 		<destw> <desth> <srcw> <srch>
 * 	Copy a subimage from srchandle(srcx, srcy) to
 * 	desthandle(destx, desty), size w x h.  Or, resize the subimage
 * 	in copying from srcw x srch to destw x desth.
 * 
 */
int
gdCmd( ClientData clientData, Tcl_Interp *interp, int argc, char *argv[] )
{
    int argi, subi;
    /* Check for subcommand. */
    if ( argc < 2 )
    {
	interp->result = "wrong # args: should be \"gd option ...\"";
	return TCL_ERROR;
    }
    
    /* Find the subcommand. */
    for (subi = 0; subi < (sizeof subcmdVec) / (sizeof subcmdVec[0]); subi++)
    {
	if ( strcmp( subcmdVec[subi].cmd, argv[1] ) == 0 )
	{
	    /* Check arg count. */
	    if ( argc - 2 < subcmdVec[subi].minargs ||
		 argc - 2 > subcmdVec[subi].maxargs )
	    {
		Tcl_AppendResult( interp, "wrong # args: should be \"gd ",
				  subcmdVec[subi].cmd, " ",
				  subcmdVec[subi].usage, "\"", 0 );
		return TCL_ERROR;
	    }

	    /* Check for valid handle(s). */
	    if ( subcmdVec[subi].ishandle > 0 ) {
		/* Are any handles allocated? */
		if ( GDHandleTable == 0 ) {
		    sprintf( interp->result, "no such handle%s: ",
			     subcmdVec[subi].ishandle > 1 ? "s" : "" );
		    for ( argi = 2 + subcmdVec[subi].subcmds;
			  argi < 2 + subcmdVec[subi].subcmds +
			  	subcmdVec[subi].ishandle;
			  argi++ ) {
			Tcl_AppendResult( interp, argv[argi], " ", 0 );
		    }
		    return TCL_ERROR;
		}
		/* Check each handle to see if it's a valid handle. */
		if ( 2 + subcmdVec[subi].subcmds + subcmdVec[subi].ishandle
		     > argc ) {
		    interp->result = "GD handle(s) not specified";
		    return TCL_ERROR;
		}
		for ( argi = 2 + subcmdVec[subi].subcmds;
		      argi < (2 + subcmdVec[subi].subcmds +
			      subcmdVec[subi].ishandle);
		      argi++ ) {
		    if (! Tcl_HandleXlate( interp, GDHandleTable, argv[argi] ))
			return TCL_ERROR;
		}
	    }
	    
	    /* Call the subcommand function. */
	    return (*subcmdVec[subi].f)( interp, argc, argv );
	}
    }

    /* If we get here, the option doesn't match. */
    Tcl_AppendResult( interp, "bad option \"", argv[1], "\": should be ", 0 );
    for (subi = 0; subi < (sizeof subcmdVec) / (sizeof subcmdVec[0]); subi++)
	Tcl_AppendResult( interp, (subi > 0 ? ", " : ""),
			   subcmdVec[subi].cmd, 0 );

    return TCL_ERROR;
}

static int
tclGdCreateCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    int w, h;
    gdImagePtr im;
    FILE *f;
    int *style;

    if ( strcmp(argv[1], "create") == 0 ) {
	if ( Tcl_GetInt( interp, argv[2], &w ) != TCL_OK )
	    return TCL_ERROR;
	if ( Tcl_GetInt( interp, argv[3], &h ) != TCL_OK )
	    return TCL_ERROR;
	im = gdImageCreate( w, h );
	if ( im == NULL )
	{
	    sprintf( interp->result, "GD unable to allocate %d X %d image",
		     w, h );
	    return TCL_ERROR;
	}
    } else {
	if ( Tcl_GetOpenFile(interp, argv[2], 0, 1, &f ) != TCL_OK )
	    return TCL_ERROR;
	/* Read GIF, XBM, or GD file? */
	if ( argv[1][11] == 'I' ) {
	    im = gdImageCreateFromGif( f );
	} else if ( argv[1][10] == 'X' ) {
	    im = gdImageCreateFromXbm( f );
	} else {
	    im = gdImageCreateFromGd( f );
	}
	if ( im == NULL ) {
	    interp->result = "GD unable to read image file";
	    return TCL_ERROR;
	}
    }

    /* Turn the pointer into a handle. */
    if ( GDHandleTable == 0 )
    {
	GDHandleTable = (void *)Tcl_HandleTblInit( "gd", sizeof(gdImagePtr), 1 );
	if ( GDHandleTable == 0 )
	{
	    interp->result = "unable to create table for GD handles.";
	    return TCL_ERROR;
	}
    }
	
    *(gdImagePtr *)(Tcl_HandleAlloc( GDHandleTable, interp->result)) = im;
    return TCL_OK;
}

static int
tclGdDestroyCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    void *hdl;

    /* Get the handle, and the image pointer. */
    hdl = (void *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );
    im = *(gdImagePtr *)hdl;
    /* Release the handle, destroy the image. */
    Tcl_HandleFree( GDHandleTable, hdl );
    gdImageDestroy(im);

    return TCL_OK;
}

static int
tclGdWriteCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    FILE *f;
    
    /* Get the image pointer. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );

    /* Get the file reference. */
    if ( Tcl_GetOpenFile(interp, argv[3], 1, 1, &f ) != TCL_OK )
	return TCL_ERROR;

    /* Do it. */
    if (argv[1][6] == 'I') {
	gdImageGif( im,f );
    } else {
	gdImageGd( im, f );
    }
    return TCL_OK;
}

static int
tclGdInterlaceCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    int on_off;

    /* Get the image pointer. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );

    if (argc == 4) {
	/* Get the on_off values. */
	if ( Tcl_GetBoolean( interp, argv[3], &on_off ) != TCL_OK )
	    return TCL_ERROR;

	/* Do it. */
	gdImageInterlace( im, on_off );
    } else {
	/* Get the current state. */
	sprintf(interp->result, "%d", gdImageGetInterlaced( im ) );
    }

    return TCL_OK;
}


static int
tclGdColorCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    int subi, i, nsub, args[3];


    if ( argc >= 3 ) {
	nsub = (sizeof colorCmdVec) / (sizeof colorCmdVec[0]);
	/* Find the subcommand. */
	for (subi = 0; subi < nsub; subi++)
	{
	    if ( strcmp( colorCmdVec[subi].cmd, argv[2] ) == 0 )
	    {
		/* Check arg count. */
		if ( argc - 2 < colorCmdVec[subi].minargs ||
		     argc - 2 > colorCmdVec[subi].maxargs )
		{
		    Tcl_AppendResult( interp,
				      "wrong # args: should be \"gd color ",
				      colorCmdVec[subi].cmd, " ",
				      colorCmdVec[subi].usage, "\"", 0 );
		    return TCL_ERROR;
		}

		/* Get the image pointer. */
		im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable,
						     argv[3] );
	    
		/* Parse off integer arguments.
		 * 1st 4 are gd color <opt> <handle>
		 */
		for ( i = 0; i < argc - 4; i++ ) {
		    if ( Tcl_GetInt( interp, argv[i+4], &args[i] ) != TCL_OK )
			return TCL_ERROR;
		    if ( args[i] < 0 || args[i] > 255 ) {
			interp->result = "argument out of range 0-255";
			return TCL_ERROR;
		    }
		}

		/* Call the subcommand function. */
		return (*colorCmdVec[subi].f)( interp, im, argc - 4, args );
	    }
	}
    }

    /* If we get here, the option doesn't match. */
    if ( argc > 2 ) 
	Tcl_AppendResult( interp, "bad option \"", argv[2], "\": ", 0 );
    else
	Tcl_AppendResult( interp, "wrong # args: ", 0 );
    Tcl_AppendResult( interp, "should be ", 0 );
    for (subi = 0; subi < nsub; subi++)
	Tcl_AppendResult( interp, (subi > 0 ? ", " : ""),
			   colorCmdVec[subi].cmd, 0 );

    return TCL_ERROR;
}

static int
tclGdColorNewCmd( Tcl_Interp *interp, gdImagePtr im, int argc, int args[] )
{
    int color;

    color = gdImageColorAllocate( im, args[0], args[1], args[2] );

    sprintf( interp->result, "%d", color );
    return TCL_OK;
}

static int
tclGdColorExactCmd( Tcl_Interp *interp, gdImagePtr im, int argc, int args[] )
{
    int color;

    color = gdImageColorExact( im, args[0], args[1], args[2] );

    sprintf( interp->result, "%d", color );
    return TCL_OK;
}

static int
tclGdColorClosestCmd( Tcl_Interp *interp, gdImagePtr im, int argc, int args[] )
{
    int color;

    color = gdImageColorClosest( im, args[0], args[1], args[2] );

    sprintf( interp->result, "%d", color );
    return TCL_OK;
}

static int
tclGdColorFreeCmd( Tcl_Interp *interp, gdImagePtr im, int argc, int args[] )
{
    int color;

    gdImageColorDeallocate( im, args[0] );

    return TCL_OK;
}

static int
tclGdColorTranspCmd( Tcl_Interp *interp, gdImagePtr im, int argc, int args[] )
{
    int color;

    if ( argc > 0 ) 
	gdImageColorTransparent( im, args[0] );
    else
	sprintf( interp->result, "%d", gdImageGetTransparent(im) );

    return TCL_OK;
}

static int
tclGdColorGetCmd( Tcl_Interp *interp, gdImagePtr im, int argc, int args[] )
{
    char buf[30];
    int i;

    /* IF one arg, return the single color, else return list of all colors. */
    if ( argc == 1 )
    {
	if ( args[0] >= gdImageColorsTotal(im) || im->open[args[0]] )
	    return TCL_OK;	/* No such color */
	sprintf( interp->result, "%d %d %d",
		 gdImageRed(im,args[0]),
		 gdImageGreen(im,args[0]),
		 gdImageBlue(im,args[0]) );
    } else {
	for ( i = 0; i < gdImageColorsTotal(im); i++ )
	{
	    if ( im->open[i] )
		continue;
	    sprintf( buf, "%d %d %d %d", i,
		     gdImageRed(im,i),
		     gdImageGreen(im,i),
		     gdImageBlue(im,i) );
	    Tcl_AppendElement( interp, buf );
	}
    }

    return TCL_OK;
}

static int 
tclGdBrushCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im, imbrush;
    
    /* Get the image pointers. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );
    imbrush = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[3] );

    /* Do it. */
    gdImageSetBrush( im, imbrush );

    return TCL_OK;
}

static int 
tclGdTileCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im, tile;
    
    /* Get the image pointers. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );
    tile = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[3] );

    /* Do it. */
    gdImageSetTile( im, tile );

    return TCL_OK;
}


static int
tclGdStyleCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    int ncolor, *colors = NULL, i;
    char **colorArgv = argv+3;	/* By default, colors are listed in argv. */
    int retval = TCL_OK;
    
    /* Get the image pointer. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );

    /* Figure out how many colors in the style list and allocate memory. */
    ncolor = argc - 3;
    /* If only one argument, treat it as a list. */
    if ( ncolor == 1 )
	if ( Tcl_SplitList( interp, argv[3], &ncolor, &colorArgv ) != TCL_OK )
	    return TCL_ERROR;

    colors = (int *)calloc( ncolor, sizeof(int) );
    if ( colors == NULL )
    {
	sprintf( interp->result, "Memory allocation failed" );
	retval = TCL_ERROR;
	goto out;
    }
    /* Get the color values. */
    for ( i = 0; i < ncolor; i++ )
	if ( Tcl_GetInt( interp, colorArgv[i], &colors[i] ) != TCL_OK )
	{
	    retval = TCL_ERROR;
	    break;
	}

    /* Call the Style function if no error. */
    if ( retval == TCL_OK )
	gdImageSetStyle( im, colors, ncolor );

 out:
    /* Free the colors. */
    if (colors != NULL)
	free((char *)colors);

    if ( colorArgv != argv + 3 )
	free( (char *)colorArgv );

    return retval;
}

static int
tclGdSetCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    int color, x, y;
    
    /* Get the image pointer. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );

    /* Get the color, x, y values. */
    if ( tclGd_GetColor( interp, argv[3], &color ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[4], &x ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[5], &y ) != TCL_OK )
	return TCL_ERROR;

    /* Call the Set function. */
    gdImageSetPixel( im, x, y, color );

    return TCL_OK;
}

static int
tclGdLineCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    int color, x1, y1, x2, y2;
    
    /* Get the image pointer. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );

    /* Get the color, x, y values. */
    if ( tclGd_GetColor( interp, argv[3], &color ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[4], &x1 ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[5], &y1 ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[6], &x2 ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[7], &y2 ) != TCL_OK )
	return TCL_ERROR;

    /* Call the appropriate Line function. */
    gdImageLine( im, x1, y1, x2, y2, color );

    return TCL_OK;
}

static int
tclGdRectCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    int color, x1, y1, x2, y2;
    
    /* Get the image pointer. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );

    /* Get the color, x, y values. */
    if ( tclGd_GetColor( interp, argv[3], &color ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[4], &x1 ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[5], &y1 ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[6], &x2 ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[7], &y2 ) != TCL_OK )
	return TCL_ERROR;

    /* Call the appropriate rectangle function. */
    if ( argv[1][0] == 'r' ) 
	gdImageRectangle( im, x1, y1, x2, y2, color );
    else
	gdImageFilledRectangle( im, x1, y1, x2, y2, color );

    return TCL_OK;
}

static int
tclGdArcCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    int color, cx, cy, width, height, start, end;
    
    /* Get the image pointer. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );

    /* Get the color, x, y values. */
    if ( tclGd_GetColor( interp, argv[3], &color ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[4], &cx ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[5], &cy ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[6], &width ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[7], &height ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[8], &start ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[9], &end ) != TCL_OK )
	return TCL_ERROR;

    /* Call the appropriate arc function. */
    if ( argv[1][0] == 'a' ) 
        gdImageArc( im, cx, cy, width, height, start, end, color );
    else
        gdImageFilledArc( im, cx, cy, width, height, start, end, color );

    return TCL_OK;
}

static int
tclGdPolygonCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    int color, npoints, i;
    char **pointArgv = &argv[4];
    gdPointPtr points = NULL;
    int retval = TCL_OK;
    
    /* Get the image pointer. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );

    /* Get the color, x, y values. */
    if ( tclGd_GetColor( interp, argv[3], &color ) != TCL_OK )
	return TCL_ERROR;

    /* Figure out how many points in the list and allocate memory. */
    npoints = argc - 4;
    /* If only one argument, treat it as a list. */
    if ( npoints == 1 )
	if ( Tcl_SplitList( interp, argv[4], &npoints, &pointArgv ) != TCL_OK )
	    return TCL_ERROR;

    /* Error check size of point list. */
    if ( npoints % 2 != 0 )
    {
	sprintf( interp->result, "Number of coordinates must be even" );
	retval = TCL_ERROR;
	goto out;
    }

    /* Divide by 2 to get number of points, and final error check. */
    npoints /= 2;
    if ( npoints < 3 )
    {
	sprintf( interp->result, "Must specify at least 3 points." );
	retval = TCL_ERROR;
	goto out;
    }

    points = (gdPointPtr)calloc( npoints, sizeof(gdPoint) );
    if ( points == NULL )
    {
	sprintf( interp->result, "Memory allocation failed" );
	retval = TCL_ERROR;
	goto out;
    }

    /* Get the point values. */
    for ( i = 0; i < npoints; i++ )
	if ( Tcl_GetInt( interp, pointArgv[i*2], &points[i].x ) != TCL_OK ||
	     Tcl_GetInt( interp, pointArgv[i*2+1], &points[i].y ) != TCL_OK )
	{
	    retval = TCL_ERROR;
	    break;
	}

    /* Call the appropriate polygon function. */
    if ( argv[1][0] == 'p' ) 
	gdImagePolygon( im, points, npoints, color );
    else
	gdImageFilledPolygon( im, points, npoints, color );

 out:
    /* Free the points. */
    if ( pointArgv != argv + 3 )
	free( (char *)pointArgv );
    if ( points != NULL )
	free( (char *)points );

    return TCL_OK;
}

static int
tclGdFillCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    int color, x, y, border;
    
    /* Get the image pointer. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );

    /* Get the color, x, y and possibly bordercolor values. */
    if ( tclGd_GetColor( interp, argv[3], &color ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[4], &x ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[5], &y ) != TCL_OK )
	return TCL_ERROR;

    /* Call the appropriate fill function. */
    if ( argc - 2 == 5 ) {
	if ( Tcl_GetInt( interp, argv[6], &border ) != TCL_OK )
	    return TCL_ERROR;
	gdImageFillToBorder( im, x, y, border, color );
    } else {
	gdImageFill( im, x, y, color );
    }

    return TCL_OK;
}

static int
tclGdTextCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    int color, x, y, i;
    gdFontPtr font;
    
    /* Get the image pointer. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );

    /* Get the font */
    for ( i = 0; i < (sizeof fontTbl) / (sizeof fontTbl[0]); i++ ) {
	if ( strcmp(argv[3], fontTbl[i].fontname) == 0 ) {
	    font = *fontTbl[i].fontp;
	    break;
	}
    }
    if ( i >= (sizeof fontTbl) / (sizeof fontTbl[0]) ) {
	Tcl_AppendResult( interp, "invalid font specified: ", argv[3], 0 );
	return TCL_ERROR;
    }

    /* Get the color, x, y values. */
    if ( tclGd_GetColor( interp, argv[4], &color ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[5], &x ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[6], &y ) != TCL_OK )
	return TCL_ERROR;

    /* Call the appropriate text function. */
    if ( argv[1][4] == 'u' ) {
	gdImageStringUp( im, font, x, y, argv[7], color );
    } else {
	gdImageString( im, font, x, y, argv[7], color );
    }

    return TCL_OK;
}

static int
tclGdCopyCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr imdest, imsrc;
    int destx, desty, srcx, srcy, destw, desth, srcw, srch;
    
    /* Get the image pointer. */
    imdest = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );
    imsrc = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[3] );

    /* Get the x, y, etc. values. */
    if ( Tcl_GetInt( interp, argv[4], &destx ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[5], &desty ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[6], &srcx ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[7], &srcy ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[8], &destw ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[9], &desth ) != TCL_OK )
	return TCL_ERROR;
    
    /* Call the appropriate copy function. */
    if ( argc - 2 == 10 ) {
	if ( Tcl_GetInt( interp, argv[10], &srcw ) != TCL_OK )
	    return TCL_ERROR;
	if ( Tcl_GetInt( interp, argv[11], &srch ) != TCL_OK )
	    return TCL_ERROR;

	gdImageCopyResized( imdest, imsrc, destx, desty, srcx, srcy,
			    destw, desth, srcw, srch);
    }
    else
	gdImageCopy( imdest, imsrc, destx, desty, srcx, srcy, destw, desth );

    return TCL_OK;
}

static int
tclGdGetCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    int color, x, y;
    
    /* Get the image pointer. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );

    /* Get the x, y values. */
    if ( Tcl_GetInt( interp, argv[3], &x ) != TCL_OK )
	return TCL_ERROR;
    if ( Tcl_GetInt( interp, argv[4], &y ) != TCL_OK )
	return TCL_ERROR;

    /* Call the Get function. */
    color = gdImageGetPixel( im, x, y );

    sprintf( interp->result, "%d", color );

    return TCL_OK;
}

static int
tclGdSizeCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;

    /* Get the image pointer. */
    im = *(gdImagePtr *)Tcl_HandleXlate( interp, GDHandleTable, argv[2] );

    sprintf( interp->result, "%d %d", gdImageSX(im), gdImageSY(im) );

    return TCL_OK;
}

static int
tclGdFontsCmd( Tcl_Interp *interp, int argc, char *argv[] )
{
    gdImagePtr im;
    gdFontPtr fp;
    char buf[30];
    int i;

    for ( i = 0; i < (sizeof fontTbl) / (sizeof fontTbl[0]); i++ ) {
	fp = *fontTbl[i].fontp;
	sprintf( buf, "%s %d %d", fontTbl[i].fontname, fp->w, fp->h );
	Tcl_AppendElement( interp, buf );
    }

    return TCL_OK;
}

/* 
 * Initialize the package.
 */
void
Gdtcl_Init( Tcl_Interp *interp ) 
{
    Tcl_CreateCommand( interp, "gd", gdCmd,
                       (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
}

