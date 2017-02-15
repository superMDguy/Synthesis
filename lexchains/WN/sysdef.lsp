;;; -*- Mode: LISP; Syntax: Common-lisp; Package: FUTURE-COMMON-LISP-USER; Base: 10 -*-

;;; CommonLisp interface to WordNet
;;; 1995, Mark Nahabedian
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology


#+Genera
(scl:defsystem WordNet 
    (:default-pathname "w:>naha>wordnet>"
     :maintain-journals nil)
    (:serial
      "packages"
      "parts-of-speech"
      "wordnet-database-files"
      "parse-wordnet-data"
      "representation"
      "relationship-algorithms"
      ))
