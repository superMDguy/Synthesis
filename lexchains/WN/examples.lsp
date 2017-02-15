;;; -*- Mode: LISP; Syntax: Common-lisp; Package: COMMON-LISP-USER; Base: 10 -*-

(in-package :COMMON-LISP-USER)


(defun synsets-containing-words (part-of-speech words)
  (reduce #'intersection
	  (mapcar #'(lambda (word)
		      (wn:index-entry-synsets
			(wn:cached-index-lookup word part-of-speech)))
		  words)))

(defun get-synonyms (words part-of-speech)
  (reduce #'union
	  (mapcar #'wn:synset-words
		  (synsets-containing-words part-of-speech
					    (if (listp words) words (list words))))
	  :initial-value nil))

(defun get-antonyms (word part-of-speech)
  (let ((antonyms nil)
	(synsets (wn:index-entry-synsets
		   (wn:cached-index-lookup word part-of-speech))))
    (dolist (s synsets)
      (dolist (p (wn:wordnet-pointers s))
	(when (eq (wn:wordnet-pointer-type p) :antonym)
	  (let ((from (wn:wordnet-pointer-from-word p))
		(to (wn:wordnet-pointer-to-word p)))
	    (when (or (typep from 'wn:wordnet-synset-entry)
		      (and (stringp (car from))
			   (string-equal (car from) word)))
	      (if (typep to 'wn:wordnet-synset-entry)
		  (dolist (w (wn:synset-words to))
		    (pushnew (car w) antonyms :test #'string-equal))
		  (pushnew (car to) antonyms :test #'string-equal)))))))
    antonyms))

(defvar *my-commonality-table*)

(defun MY-COMMONALITY (synset1 synset2)
  (let* ((hash1 (gethash synset1 *my-commonality-table*))
         (hash2 (and hash1 (gethash synset2 hash1))))
    (or (and hash1 hash2)
        (let ((val (wn::commonality :hypernym synset1 synset2)))
          (unless hash1
            (setf hash1
                  (setf (gethash synset1 *my-commonality-table*)
                        (make-hash-table :test #'equal))))
          (setf (gethash synset2 hash1) val)
          val))))


(defun FIND-SYNSET-WITH-SENSE (word-synsets like-word-synsets)
  (let ((found nil))
    (dolist (like-synset like-word-synsets)
      (dolist (synset word-synsets)
	(let ((c (my-commonality synset like-synset))) 
	  (when c
	    (push c found)))))
    (sort (pairlis (mapcar #'(lambda (f)
			       (apply #'+ (mapcar #'cdr (cdr f))))
			   found)
		   found)
	  #'< :key #'car)))
