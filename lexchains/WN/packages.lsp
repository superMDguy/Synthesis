;;; -*- Mode: LISP; Syntax: Common-lisp; Package: COMMON-LISP-USER; Base: 10 -*-

;;; CommonLisp interface to WordNet
;;; 1995, Mark Nahabedian
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology

(in-package :COMMON-LISP-USER)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package definitions

(defpackage wordnet
  (:nicknames wn)
  (:use #+Genera future-common-lisp
        #-Genera common-lisp)
  (:export
    ;; file "parts-of-speech"
    "CANONICALIZE-PART-OF-SPEECH"
    "DO-PARTS-OF-SPEECH"
    "PARTS-OF-SPEECH"

    ;; file "wordnet-database-files":
    "INDEX-ENTRY-FOR-WORD"
    "READ-DATA-FILE-ENTRY"

    ;; file "parse-wordnet-data":
    "PARSE-DATA-FILE-ENTRY"
    "PARSE-INDEX-FILE-ENTRY"

    ;; file "representation":
    "CACHED-DATA-LOOKUP"
    "CACHED-INDEX-LOOKUP"
    "INDEX-ENTRY-SYNSETS"
    "INDEX-ENTRY-WORD"
    "WORDNET-INDEX-ENTRY"
    "WORDNET-SYNSET-ENTRY"
    "WORDNET-NOUN-ENTRY"
    "WORDNET-ADJECTIVE-ENTRY"
    "WORDNET-ADVERB-ENTRY"
    "WORDNET-VERB-ENTRY"
    "WORDNET-POINTER"
    "WORDNET-POINTERS"
    "WORDNET-POINTER-TYPE"
    "WORDNET-POINTER-FROM-SYNSET"
    "WORDNET-POINTER-FROM-SYNSET-INDEX"
    "WORDNET-POINTER-TO-SYNSET"
    "WORDNET-POINTER-TO-SYNSET-INDEX"
    #+CLIMxxx "PRETTY-PRINT-SYNSET"
    "PART-OF-SPEECH"
    "SYNSET-WORDS"
    "WORDNET-POINTER-FROM-WORD"
    "WORDNET-POINTER-TO-WORD"

    ;; file "relationship-algorithms":
    "RELATION-TRANSITIVE-CLOSURE"
    "COMMONALITY"
    ))

#+CLIMxxx
(defpackage wordnet-interface
  (:nicknames wni)
  (:use wordnet clim clim-lisp)
  (:export
    "WORDNET-BROWSER"
    ))

