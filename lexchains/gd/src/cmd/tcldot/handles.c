/*
 * handles.c --
 *
 * adapted from tclXhandles.c tclXutil.c and tclExtend.h
 */
 
/*
 *
 * tclXhandles.c --
 *
 * Tcl handles.  Provides a mechanism for managing expandable tables that are
 * addressed by textual handles.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1994 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tclXhandles.c,v 4.0 1994/07/16 05:27:04 markd Rel $
 *-----------------------------------------------------------------------------
 */

#include "tcl.h"

#ifndef FALSE
#define FALSE (0)
#endif

#ifndef TRUE
#define TRUE (1)
#endif

#define ISSPACE(c) (isspace ((unsigned char) c))

/*
 * Variable set to contain the alignment factor (in bytes) for this machine.
 * It is set on the first table initialization.
 */
static int entryAlignment = 0;

/*
 * Marco to rounded up a size to be a multiple of (void *).  This is required
 * for systems that have alignment restrictions on pointers and data.
 */
#define ROUND_ENTRY_SIZE(size) \
    ((((size) + entryAlignment - 1) / entryAlignment) * entryAlignment)

/*
 * This is the table header.  It is separately allocated from the table body,
 * since it must keep track of a table body that might move.  Each entry in the
 * table is preceded with a header which has the free list link, which is a
 * entry index of the next free entry.  Special values keep track of allocated
 * entries.
 */

#define NULL_IDX      -1
#define ALLOCATED_IDX -2

typedef unsigned char ubyte_t;
typedef ubyte_t *ubyte_pt;

typedef struct {
    int      useCount;          /* Keeps track of the number sharing       */
    int      entrySize;         /* Entry size in bytes, including overhead */
    int      tableSize;         /* Current number of entries in the table  */
    int      freeHeadIdx;       /* Index of first free entry in the table  */
    ubyte_pt bodyPtr;           /* Pointer to table body                   */
    int      baseLength;        /* Length of handleBase.                   */
    char     handleBase [1];    /* Base handle name.  MUST BE LAST FIELD!  */
    } tblHeader_t;
typedef tblHeader_t *tblHeader_pt;

typedef struct {
    int freeLink;
  } entryHeader_t;
typedef entryHeader_t *entryHeader_pt;

#define ENTRY_HEADER_SIZE (ROUND_ENTRY_SIZE (sizeof (entryHeader_t)))

/*
 * This macro is used to return a pointer to an entry, given its index.
 */
#define TBL_INDEX(hdrPtr, idx) \
    ((entryHeader_pt) (hdrPtr->bodyPtr + (hdrPtr->entrySize * idx)))

/*
 * This macros to convert between pointers to the user and header area of
 * an table entry.
 */
#define USER_AREA(entryPtr) \
 (void *) (((ubyte_pt) entryPtr) + ENTRY_HEADER_SIZE);
#define HEADER_AREA(entryPtr) \
 (entryHeader_pt) (((ubyte_pt) entryPtr) - ENTRY_HEADER_SIZE);

/*
 * Function prototypes.
 */

void 
Tcl_HandleFree _ANSI_ARGS_((void *headerPtr,
                        void *entryPtr));

void *
Tcl_HandleTblInit _ANSI_ARGS_((CONST char *handleBase,
                               int         entrySize,
                               int         initEntries));

void *
Tcl_HandleXlate _ANSI_ARGS_((Tcl_Interp  *interp,
                             void *headerPtr,
                             CONST  char *handle));


static void
LinkInNewEntries _ANSI_ARGS_((tblHeader_pt tblHdrPtr,
                              int          newIdx,
                              int          numEntries));

static void
ExpandTable _ANSI_ARGS_((tblHeader_pt tblHdrPtr,
                         int          neededIdx));

entryHeader_pt
AllocEntry _ANSI_ARGS_((tblHeader_pt  tblHdrPtr,
                        int          *entryIdxPtr));

static int
Tcl_HandleDecode _ANSI_ARGS_((Tcl_Interp   *interp,
                          tblHeader_pt  tblHdrPtr,
                          CONST char   *handle));
static int
StrToUnsigned _ANSI_ARGS_((CONST char *string,
			int         base,
			unsigned   *unsignedPtr));

/*=============================================================================
 * LinkInNewEntries --
 *   Build free links through the newly allocated part of a table.
 *   
 * Parameters:
 *   o tblHdrPtr (I) - A pointer to the table header.
 *   o newIdx (I) - Index of the first new entry.
 *   o numEntries (I) - The number of new entries.
 *-----------------------------------------------------------------------------
 */
static void
LinkInNewEntries (tblHdrPtr, newIdx, numEntries)
    tblHeader_pt tblHdrPtr;
    int          newIdx;
    int          numEntries;
{
    int            entIdx, lastIdx;
    entryHeader_pt entryPtr;
    
    lastIdx = newIdx + numEntries - 1;

    for (entIdx = newIdx; entIdx < lastIdx; entIdx++) {
        entryPtr = TBL_INDEX (tblHdrPtr, entIdx);
        entryPtr->freeLink = entIdx + 1;
    }
    entryPtr = TBL_INDEX (tblHdrPtr, lastIdx);
    entryPtr->freeLink = tblHdrPtr->freeHeadIdx;
    tblHdrPtr->freeHeadIdx = newIdx;

}

/*=============================================================================
 * ExpandTable --
 *   Expand a handle table, doubling its size.
 * Parameters:
 *   o tblHdrPtr (I) - A pointer to the table header.
 *   o neededIdx (I) - If positive, then the table will be expanded so that
 *     this entry is available.  If -1, then just expand by the number of 
 *     entries specified on table creation.  MUST be smaller than this size.
 *-----------------------------------------------------------------------------
 */
static void
ExpandTable (tblHdrPtr, neededIdx)
    tblHeader_pt tblHdrPtr;
    int          neededIdx;
{
    ubyte_pt oldbodyPtr = tblHdrPtr->bodyPtr;
    int      numNewEntries;
    int      newSize;
    
    if (neededIdx < 0)
        numNewEntries = tblHdrPtr->tableSize;
    else
        numNewEntries = (neededIdx - tblHdrPtr->tableSize) + 1;
    newSize = (tblHdrPtr->tableSize + numNewEntries) * tblHdrPtr->entrySize;

    tblHdrPtr->bodyPtr = (ubyte_pt) ckalloc (newSize);
    memcpy (tblHdrPtr->bodyPtr, oldbodyPtr, 
            (tblHdrPtr->tableSize * tblHdrPtr->entrySize));
    LinkInNewEntries (tblHdrPtr, tblHdrPtr->tableSize, numNewEntries);
    tblHdrPtr->tableSize += numNewEntries;
    ckfree (oldbodyPtr);
    
}

/*=============================================================================
 * AllocEntry --
 *   Allocate a table entry, expanding if necessary.
 *
 * Parameters:
 *   o headerPtr (I) - A pointer to the table header.
 *   o entryIdxPtr (O) - The index of the table entry is returned here.
 * Returns:
 *    The a pointer to the entry.
 *-----------------------------------------------------------------------------
 */
entryHeader_pt
AllocEntry (headerPtr, entryIdxPtr)
    tblHeader_pt headerPtr;
    int          *entryIdxPtr;
{
    tblHeader_pt  tblHdrPtr = headerPtr;
    entryHeader_pt entryPtr;
    int            entryIdx;

    if (tblHdrPtr->freeHeadIdx == NULL_IDX)
        ExpandTable (tblHdrPtr, -1);

    entryIdx = tblHdrPtr->freeHeadIdx;    
    entryPtr = TBL_INDEX (tblHdrPtr, entryIdx);
    tblHdrPtr->freeHeadIdx = entryPtr->freeLink;
    entryPtr->freeLink = ALLOCATED_IDX;
    
    *entryIdxPtr = entryIdx;
    return USER_AREA (entryPtr);
    
}

/*=============================================================================
 * Tcl_HandleDecode --
 *   Decode handle into an entry number.
 *
 * Parameters:
 *   o interp (I) - A error message may be returned in result.
 *   o tblHdrPtr (I) - A pointer to the table header.
 *   o handle (I) - Handle to decode.
 * Returns:
 *   The entry index decoded from the handle, or a negative number if an error
 *   occured.
 *-----------------------------------------------------------------------------
 */
static int
Tcl_HandleDecode (interp, tblHdrPtr, handle)
    Tcl_Interp   *interp;
    tblHeader_pt  tblHdrPtr;
    CONST char   *handle;
{
    unsigned entryIdx;

    if ((strncmp (tblHdrPtr->handleBase, (char *) handle, 
             tblHdrPtr->baseLength) != 0) ||
             !StrToUnsigned (&handle [tblHdrPtr->baseLength], 10, 
                                 &entryIdx)) {
        Tcl_AppendResult (interp, "invalid ", tblHdrPtr->handleBase,
                          " handle: ", handle, (char *) NULL);
        return -1;
    }
    return entryIdx;

}

/*=============================================================================
 * Tcl_HandleTblInit --
 *   Create and initialize a Tcl dynamic handle table.  The use count on the
 *   table is set to one.
 * Parameters:
 *   o handleBase(I) - The base name of the handle, the handle will be returned
 *     in the form "baseNN", where NN is the table entry number.
 *   o entrySize (I) - The size of an entry, in bytes.
 *   o initEntries (I) - Initial size of the table, in entries.
 * Returns:
 *   A pointer to the table header.  
 *-----------------------------------------------------------------------------
 */
void *
Tcl_HandleTblInit (handleBase, entrySize, initEntries)
    CONST char *handleBase;
    int         entrySize;
    int         initEntries;
{
    tblHeader_pt tblHdrPtr;
    int          baseLength = strlen ((char *) handleBase);

    /*
     * It its not been calculated yet, determine the entry alignment required
     * for this machine.
     */
    if (entryAlignment == 0) {
        entryAlignment = sizeof (void *);
        if (sizeof (long) > entryAlignment)
            entryAlignment = sizeof (long);
        if (sizeof (double) > entryAlignment)
            entryAlignment = sizeof (double);
    }

    /*
     * Set up the table entry.
     */
    tblHdrPtr = (tblHeader_pt) ckalloc (sizeof (tblHeader_t) + baseLength + 1);

    tblHdrPtr->useCount = 1;
    tblHdrPtr->baseLength = baseLength;
    strcpy (tblHdrPtr->handleBase, (char *) handleBase);

    /* 
     * Calculate entry size, including header, rounded up to sizeof (void *). 
     */
    tblHdrPtr->entrySize = ENTRY_HEADER_SIZE + ROUND_ENTRY_SIZE (entrySize);
    tblHdrPtr->freeHeadIdx = NULL_IDX;
    tblHdrPtr->tableSize = initEntries;
    tblHdrPtr->bodyPtr =
        (ubyte_pt) ckalloc (initEntries * tblHdrPtr->entrySize);
    LinkInNewEntries (tblHdrPtr, 0, initEntries);

    return (void *) tblHdrPtr;

}

/*=============================================================================
 * Tcl_HandleAlloc --
 *   Allocate an entry and associate a handle with it.
 *
 * Parameters:
 *   o headerPtr (I) - A pointer to the table header.
 *   o handlePtr (O) - Buffer to return handle in. It must be big enough to
 *     hold the name.
 * Returns:
 *   A pointer to the allocated entry (user part).
 *-----------------------------------------------------------------------------
 */
void *
Tcl_HandleAlloc (headerPtr, handlePtr)
    void     *headerPtr;
    char     *handlePtr;
{
    tblHeader_pt   tblHdrPtr = (tblHeader_pt)headerPtr;
    entryHeader_pt entryPtr;
    int            entryIdx;
    entryPtr = AllocEntry ((tblHeader_pt) headerPtr, &entryIdx);
    sprintf (handlePtr, "%s%d", tblHdrPtr->handleBase, entryIdx);
     
    return entryPtr;
}

/*=============================================================================
 * Tcl_HandleXlate --
 *   Translate a handle to a entry pointer.
 *
 * Parameters:
 *   o interp (I) - A error message may be returned in result.
 *   o headerPtr (I) - A pointer to the table header.
 *   o handle (I) - The handle assigned to the entry.
 * Returns:
 *   A pointer to the entry, or NULL if an error occured.
 *-----------------------------------------------------------------------------
 */
void *
Tcl_HandleXlate (interp, headerPtr, handle)
    Tcl_Interp *interp;
    void *headerPtr;
    CONST char *handle;
{
    tblHeader_pt   tblHdrPtr = (tblHeader_pt)headerPtr;
    entryHeader_pt entryPtr;
    int            entryIdx;
    
    if ((entryIdx = Tcl_HandleDecode (interp, tblHdrPtr, handle)) < 0)
        return NULL;
    entryPtr = TBL_INDEX (tblHdrPtr, entryIdx);

    if ((entryIdx >= tblHdrPtr->tableSize) ||
            (entryPtr->freeLink != ALLOCATED_IDX)) {
        Tcl_AppendResult (interp, tblHdrPtr->handleBase, " is not open",
                          (char *) NULL);
        return NULL;
    }     

    return USER_AREA (entryPtr);
 
}

/*=============================================================================
 * Tcl_HandleFree --
 *   Frees a handle table entry.
 *
 * Parameters:
 *   o headerPtr (I) - A pointer to the table header.
 *   o entryPtr (I) - Entry to free.
 *-----------------------------------------------------------------------------
 */
void
Tcl_HandleFree (headerPtr, entryPtr)
    void *headerPtr;
    void *entryPtr;
{
    tblHeader_pt   tblHdrPtr = (tblHeader_pt)headerPtr;
    entryHeader_pt freeentryPtr;

    freeentryPtr = HEADER_AREA (entryPtr);
    freeentryPtr->freeLink = tblHdrPtr->freeHeadIdx;
    tblHdrPtr->freeHeadIdx = (((ubyte_pt) entryPtr) - tblHdrPtr->bodyPtr) /
                           tblHdrPtr->entrySize;
    
}

/*
 *-----------------------------------------------------------------------------
 *
 * StrToUnsigned --
 *      Convert an Ascii string to an unsigned int of the specified base.
 *
 * Parameters:
 *   o string (I) - String containing a number.
 *   o base (I) - The base to use for the number 8, 10 or 16 or zero to decide
 *     based on the leading characters of the number.  Zero to let the number
 *     determine the base.
 *   o unsignedPtr (O) - Place to return the converted number.  Will be 
 *     unchanged if there is an error.
 *
 * Returns:
 *      Returns 1 if the string was a valid number, 0 invalid.
 *-----------------------------------------------------------------------------
 */
static int
StrToUnsigned (string, base, unsignedPtr)
    CONST char *string;
    int         base;
    unsigned   *unsignedPtr;
{
    char          *end;
    unsigned long  num;

    num = strtoul (string, &end, base);
    while ((*end != '\0') && ISSPACE(*end)) {
        end++;
    }
    if ((end == string) || (*end != 0))
        return FALSE;
    *unsignedPtr = num;
    return TRUE;

}
