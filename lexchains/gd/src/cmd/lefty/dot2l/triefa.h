/*
 * Copyright (c) AT&T Corp. 1994, 1995.
 * This code is licensed by AT&T Corp.  For the
 * terms and conditions of the license, see
 * http://www.research.att.com/orgs/ssr/book/reuse
 */

/* File - TrieFA.h
 *
 *    The data types for the generated trie-baseed finite automata.
 */

struct TrieState {  /* An entry in the FA state table */
    short def;         /*     If this state is an accepting state then */
                       /*    this is the definition, otherwise -1.     */
    short trans_base;  /* The base index into the transition table.*/
    long  mask;        /* The transition mask.                     */
};

struct TrieTrans {  /* An entry in the FA transition table. */
    short c;           /* The transition character (lowercase).  */
    short next_state;  /* The next state.                        */
};

typedef struct TrieState TrieState;
typedef struct TrieTrans TrieTrans;

extern TrieState MSNEAR TrieStateTbl[];
extern TrieTrans MSNEAR TrieTransTbl[];
