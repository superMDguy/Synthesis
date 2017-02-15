/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

#include "hacks.h"
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <signal.h>
#include <string.h>
#include <assert.h>

#ifdef DOS
#include <limits.h>
#include <stdlib.h>
#endif

#include "macros.h"
#include "const.h"
#include "neato_const.h"
#include "structs.h"
#include "graph.h"
#include "globals.h"
#include "procs.h"
