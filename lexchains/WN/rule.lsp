;;; -*- Mode: LISP; Syntax: Common-lisp; Package: COMMON-LISP-USER; Base: 10 -*-

(in-package :COMMON-LISP-USER)


(defun GET-MEANING (part-of-speech word)
  (let ((val (gethash word *my-meaning-table*)))
    (if val
      (return-from get-meaning val)
      (wn:index-entry-synsets
       (wn:cached-index-lookup word part-of-speech)))))



;;1)synonym;
(defun FIND-COMMON-SENSE (word1-sense word2-sense)
  (let ((com (intersection word1-sense word2-sense)))
    (if com
      (list :syn com com)
      nil)))


(defun P-SYNONYM (word1 word2 sense)
  (if (null sense)
      (setf sense (car (find-common-sense word1 word2)))
      (when (and (assoc word1 (wn:synset-words sense) :test #'equalp)
            (assoc word2 (wn:synset-words sense) :test #'equalp))
        sense)))


;;2)hypernym^1(x) = hypernym^2(y);
;;!! sense-word is not list;
(defun HEIGHT-HYPERNYM-TREE (word-sense)
  (second (first (wn:relation-transitive-closure word-sense :hypernym))))

(defun HYPONYM-NUMBER (word-sense)
  (let ((val (gethash (wn-to-html word-sense) *number-hyponym-table*)))
    (if val
      val
      (progn
        (format t "~a  ~%" (wn-to-html word-sense))
        (setf (gethash (wn-to-html word-sense) *number-hyponym-table*)
         (length (wn:relation-transitive-closure word-sense :hyponym)))))))

(defun HYPERNYM-RELATION (word1-sense word2-sense)
  (let ((com-anted
         (car (find-synset-with-sense word1-sense word2-sense))))
    (if (and com-anted (< (first com-anted) 4)
             (> (height-hypernym-tree (second com-anted)) 0))
      (list :hyp
            (list (car (fourth com-anted)))
            (list (car (third  com-anted)))
            ;(list (height-hypernym-tree (second com-anted))
            ;      (hyponym-number (second com-anted)))
            )
      nil)))

;;3)antonym;
(defun ANTONYM-RELATION (synsets1 synsets2)
  (dolist (s1 synsets1)
    (dolist (s2 synsets2)
      (dolist (p (wn:wordnet-pointers s2))
        (when (and (eq (wn:wordnet-pointer-type p) :antonym)
                   (equalp (first (first (wn:synset-words s1)))
                           (first (wn:wordnet-pointer-to-word p))))
         (return-from antonym-relation (list :ant (list s2) (list s1)))))))
  nil)

(defun MERONYM-RELATION1 (s1 s2)
  (when (and  s1 s2)
    (dolist (i2 s2)
      (dolist (p (wn:wordnet-pointers i2))
        (dolist (i1 s1)
          (when (or (and (eq (wn:wordnet-pointer-type p) :part-meronym)
                     (eq i1 (wn:wordnet-pointer-to-word p)))
                    (and (eq (wn:wordnet-pointer-type p) :member-meronym)
                     (eq i1 (wn:wordnet-pointer-to-word p)))
                    (and (eq (wn:wordnet-pointer-type p) :substance-meronym)
                     (eq i1 (wn:wordnet-pointer-to-word p))))
            (return-from meronym-relation1 (list :mer (list i2) (list i1)))))))))

(defun MERONYM-RELATION2 (s1 s2)
  (when (and  s1 s2)
    (dolist (i1 s1)
      (dolist (p (wn:wordnet-pointers i1))
        (dolist (i2 s2)
          (when (and (eq (wn:wordnet-pointer-type p) :part-meronym)
                     (eq i2 (wn:wordnet-pointer-to-word p)))
            (return-from meronym-relation2 (list :mer (list i2) (list i1)))))))))

(defun MERONYM-RELATION (s1 s2)
  (or (meronym-relation1 s1 s2)
      (meronym-relation2 s1 s2)))

(defun CONNECTED (word1 word2 word1-sense word2-sense)
    (let ((val (or (gethash (cons word1 word2) *my-connection-table*)
                 (gethash (cons word2 word1) *my-connection-table*))))
    (if val
      (return-from connected (list :man word2-sense word1-sense))
      (or (find-common-sense word1-sense word2-sense)
          (antonym-relation  word1-sense word2-sense)
          (hypernym-relation word1-sense word2-sense)
          (meronym-relation word1-sense word2-sense)))))
