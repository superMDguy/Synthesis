;;; -*- Mode: LISP; Syntax: Common-lisp; Package: WORDNET; Base: 10 -*-

;;; CommonLisp interface to WordNet
;;; 1995, Mark Nahabedian
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology

;;; The functions in this file take a string as read from a WordNet data or
;;; index file, and decode it.  Any structural representation of the result is
;;; done at a higher level.

(in-package :WORDNET)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pointer types

(defparameter +wordnet-pointer-types+ nil)
(defparameter +wordnet-pointer-symbols+ nil)

(defun define-wordnet-pointer-type (name transitive-p direction &optional reverse-type)
  (pushnew name +wordnet-pointer-types+)
  (setf (get name 'transitive-relation-p) transitive-p)
  (when direction
    (setf (get name 'pointer-direction)
	  (ecase direction (:up :up) (:down :down))))
  (when reverse-type
    (pushnew reverse-type +wordnet-pointer-types+)
    (setf (get name 'reverse-type) reverse-type)
    (setf (get reverse-type 'reverse-type) name)
    (setf (get reverse-type 'transitive-relation-p) transitive-p)
    (when direction
      (setf (get name 'upward-relation-p)
	    (ecase direction (:up :down) (:down :up)))))
  name)

(defun wordnet-relation-p (thing)
  (member thing +wordnet-pointer-types+))

(defun define-wordnet-pointer-symbol (symbol part-of-speech definition)
  (assert (wordnet-relation-p definition))
  (push (list part-of-speech symbol definition) +wordnet-pointer-symbols+))

(progn
  ;; It's debatable which of these should or shouldn't be considered transitive.
  ;; I made a quick guess and could easily be wrong about some of these.  Let me
  ;; know what you think.
  (define-wordnet-pointer-type :also-see nil nil)
  (define-wordnet-pointer-type :antonym nil nil)
  (define-wordnet-pointer-type :attribute nil nil)
  (define-wordnet-pointer-type :cause :up nil)
  (define-wordnet-pointer-type :derived-from nil nil)
  (define-wordnet-pointer-type :entailment t :up)
  (define-wordnet-pointer-type :hypernym t :up :hyponym)
  (define-wordnet-pointer-type :member-holonym nil :up :member-meronym)
  (define-wordnet-pointer-type :part-holonym t :up :part-meronym)
  (define-wordnet-pointer-type :participle-of-verb nil nil)
  (define-wordnet-pointer-type :pertainym nil nil)
  (define-wordnet-pointer-type :similar-to nil nil)
  (define-wordnet-pointer-type :substance-holonym t :up :substance-meronym)

  (define-wordnet-pointer-symbol "!" :noun :antonym)
  (define-wordnet-pointer-symbol "@" :noun :hypernym)
  (define-wordnet-pointer-symbol "~" :noun :hyponym)
  (define-wordnet-pointer-symbol "#m" :noun :member-meronym)
  (define-wordnet-pointer-symbol "#s" :noun :substance-meronym)
  (define-wordnet-pointer-symbol "#p" :noun :part-meronym)
  (define-wordnet-pointer-symbol "%m" :noun :member-holonym)
  (define-wordnet-pointer-symbol "%s" :noun :substance-holonym)
  (define-wordnet-pointer-symbol "%p" :noun :part-holonym)
  (define-wordnet-pointer-symbol "=" :noun :attribute)

  (define-wordnet-pointer-symbol "!" :verb :antonym)
  (define-wordnet-pointer-symbol "@" :verb :hypernym)
  (define-wordnet-pointer-symbol "~" :verb :hyponym)
  (define-wordnet-pointer-symbol "*" :verb :entailment)
  (define-wordnet-pointer-symbol ">" :verb :cause)
  (define-wordnet-pointer-symbol "^" :verb :also-see)

  (define-wordnet-pointer-symbol "!" :adjective :antonym)
  (define-wordnet-pointer-symbol "&" :adjective :similar-to)
  (define-wordnet-pointer-symbol "<" :adjective :participle-of-verb)
  (define-wordnet-pointer-symbol "\\" :adjective :pertainym)
  (define-wordnet-pointer-symbol "=" :adjective :attribute)
  (define-wordnet-pointer-symbol "^" :adjective :also-see)

  (define-wordnet-pointer-symbol "!" :adverb :antonym)
  (define-wordnet-pointer-symbol "\\" :adverb :derived-from))

(defun decode-pointer-symbol-type (pointer-symbol part-of-speech)
  (dolist (pointer-entry +wordnet-pointer-symbols+)
    (when (and (eq part-of-speech (first pointer-entry))
	       (string-equal pointer-symbol (second pointer-entry)))
      (return (third pointer-entry)))))

(defmethod transitive-relation-p ((pointer-type symbol))
  (get pointer-type 'transitive-relation-p))

(defmethod relation-direction ((pointer-type symbol))
  (get pointer-type 'pointer-direction))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tokenizer

(defmacro tokenizing-wordnet-entry ((entry-string &key start end) &body body)
  (let ((index-var '#:scan-index)
	(data-var '#:entry-string)
	(start-var '#:start)
	(end-var '#:end))
    `(let* ((,data-var ,entry-string)
	    (,start-var (or ,start 0))
	    (,end-var (or ,end (length ,data-var)))
	    (,index-var ,start-var))
       (flet ((next-token ()
		(loop
		  (when (>= ,index-var ,end-var)
		    (return-from next-token nil))
		  (unless (char-equal #\space (aref ,data-var ,index-var))
		    (return))
		  (incf ,index-var))
		(let ((space (or (position #\space ,data-var
					   :test #'char-equal
					   :start ,index-var)
				 ,end-var)))
		  (when (>= space ,end-var)
		    (setq space ,end-var))
		  (prog1 (subseq ,data-var ,index-var space)
			 (setq ,index-var space)))))
	 ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Index file entries

(defun parse-index-file-entry (entry)
  "Given a string as returned by INDEX-ENTRY-FOR-WORD, decode it and return the elements of the index entry."
  (declare (values word part-of-speech poly_cnt pointer-types synset-offsets))
  (when entry
    (let (word part-of-speech poly_cnt pointer-types synset-offsets)
      (tokenizing-wordnet-entry (entry)
	(setq word (next-token))
	(setq part-of-speech (part-of-speech-for-wordnet-db-token (next-token)))
	(setq poly_cnt (parse-integer (next-token) :junk-allowed t))
	(dotimes (i (parse-integer (next-token))
		    (setq pointer-types (nreverse pointer-types)))
	  (push (next-token) pointer-types))
	(dotimes (i (parse-integer (next-token))
		    (setq synset-offsets (nreverse synset-offsets)))
	  (push (parse-integer (next-token)) synset-offsets)))
      (values word part-of-speech poly_cnt pointer-types synset-offsets))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data file entries

(defparameter +wordnet-gloss-character+ #\|)

(defun parse-data-file-entry (entry)
  "Given a string as returned by READ-DATA-FILE-ENTRY, representing a symset, return the data."
  (declare (values part-of-speech words pointers gloss verb-frames))
  (let* ((gloss-index (position +wordnet-gloss-character+ entry :test #'char-equal))
	 lex_file_num
	 part-of-speech
	 (words nil)
	 (pointers nil)
	 (verb-frames nil)
	 (gloss (when gloss-index
		  (string-trim '(#\space) (subseq entry (1+ gloss-index))))))
    (tokenizing-wordnet-entry (entry :end gloss-index)
      (next-token)						;file offset check token
      (setq lex_file_num (next-token))				;decimal integer
      (setq part-of-speech (part-of-speech-for-wordnet-db-token (next-token)))
      (dotimes (i (parse-integer (next-token) :radix 16)
		  (setq words (nreverse words)))
	(let ((word (next-token))
	      (sense-number (parse-integer (next-token) :radix 16)))
	  (push (list word sense-number) words)))
      (dotimes (i (parse-integer (next-token))
		  (setq pointers (nreverse pointers)))
	(let* ((pointer (decode-pointer-symbol-type (next-token) part-of-speech))
	       (target (parse-integer (next-token)))
	       (part-of-speech (part-of-speech-for-wordnet-db-token (next-token)))
	       (source/target (parse-integer (next-token) :radix 16))
	       (source-index (ldb (byte 8 8) source/target))
	       (target-index (ldb (byte 8 0) source/target)))
	  (push (list pointer target part-of-speech source-index target-index) pointers)))
      (let ((frame-count (next-token)))
	(when frame-count
	  (dotimes (i (parse-integer frame-count)
		      (setq verb-frames (nreverse verb-frames)))
	    (push (list (next-token) (next-token) (next-token)) verb-frames))))
      (values part-of-speech words pointers gloss verb-frames lex_file_num))))

