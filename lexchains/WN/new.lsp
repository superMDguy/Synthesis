;;; -*- Mode:Lisp; Syntax:Common-Lisp; Package: USER -*-
;;; -----------------------------------------------------------------------
;;; File:         marti.l
;;; Description:  Implementation of Marti Hearst ACL94 Text Segmentation Algo.
;;; Author:       Regina Barzilay
;;; Created:      15 May 1996
;;; Modified:     22 May 1996
;;; Modified      5 October 1996 
;;; Package:      USER
;;; -----------------------------------------------------------------------

(in-package "USER")

(defconstant +WORDS-IN-SENTENCE+ 20)

(defconstant +PARAGRAPG-STR+ "&")

(defconstant +BLOCK-SIZE+ 4)

(defvar *SENTENCE-NUM*)

(defun WHITE-CHAR-P-S (char)
  (member char '(#\space #\newline #\Linefeed  #\tab) :test #'equalp))

(defun STR-TYPE-S (str)
  (let ((type-morph (subseq str (1+ (position #\/ str)))))
    (subseq type-morph 0 (position #\/ type-morph))))

(defun STR-MORPH-S (str)
  (let ((type-morph (subseq str (1+ (position #\/ str)))))
    (subseq type-morph (1+ (position #\/ type-morph)))))

(defun STR-LEX-S (str)
  (subseq str 0 (position #\/ str)))

(defun DIVIDED-CHAR-P-S (string)
  (or (equalp (char string 0)  #\space)
     ;(equalp (char string 0)  #\newline)
      (equalp (char string 0)  #\tab)))

(defun PUNCTUATION-P (STRING)
  (member (char (str-lex-s string) 0) '(#\* #\- #\. #\? #\!
                                       #\+ #\< #\> #\= #\:
                                       #\[ #\] #\{ #\} #\@
                                       #\( #\) #\" #\' #\, #\;) :test #'equalp))
(defun REAL-SENT-END-P (string)
  ;(member (str-lex-s string) '("." "?" "!") :test #'equalp))
   (equalp (char string 0)  #\newline))

(defun SPECIAL-WORDS-P (STRING)
  (member string '("the" "a" "as" "in" "it" "of" "to" "from"
                   "be" "is" "has" "now" "much" "or" "and" "was" "so")
          :test #'equalp))

(defun CONT-READ-S (s first-ch pred)
  (let ((c  first-ch)
         word)
    (setf word
      (with-output-to-string (word)
        (loop do (write-char c word)
               (setf c (read-char s nil nil))
              until (or (null c) (funcall pred c)))
        (when c
          (unread-char c s))))
    word))

(defun READ-WORD-S (s)
  (let (c word)
    (setf c (read-char s nil nil))
      (cond
        ((null c)(return-from read-word-s nil))
        ((white-char-p-s c)
           (setf word (cont-read-s s c #'(lambda(x) (not (white-char-p-s x))))))
        (t (setf word (cont-read-s s c #'white-char-p-s))))
      word))

;; Add token list l2 to token list l1, return the addition token list
;; Warning: l1 is modified in this process!
(defun TOKEN-ADD (l1 l2)
  (if (eq l2 '())
    l1
    (let ((elt1 (assoc (caar l2) l1 :test 'equal)))
      (if elt1
        (progn
          (setf (cdr elt1) (+ (cdr elt1) (cdar l2)))
          (token-add l1 (cdr l2)))
        (token-add (cons (copy-tree (car l2)) l1) (cdr l2))))))

;; Subtract token list l2 to token list l1, return the addition token list
;; Warning: l1 is modified in this process!
(defun TOKEN-SUBTRACT (l1 l2)
  (if (eq l2 '())
    l1
    (let ((elt1 (assoc (caar l2) l1 :test 'equal)))
      (if elt1
        (progn
          (setf (cdr elt1) (- (cdr elt1) (cdar l2)))
          (token-subtract l1 (cdr l2)))
        (error "Error: token-subtract - l2=~a~% have an item=~a not in l1=~a~%"
               l2 (caar l2) l1)))))

;; Generate output for pair-mult
(defun SIM (l1 l2)
  (mapcar #'(lambda (x)
              (list (car x)
                    (cdr (assoc (car x) l1 :test 'equal))
                    (cdr (assoc (car x) l2 :test 'equal))))
          (intersection l1 l2 :key #'car)))

;; Uses sim's output
(defun PAIR-MULT (l)
  (let ((sum 0))
    (dolist (x l)
      (incf sum (* (second x) (third x))))
    sum))

(defun SUM-SQR (l)
  (labels ((sqr (x) (* x x)))
    (let ((sum 0))
      (dolist (x l)
        (incf sum (sqr (cdr x))))
      sum)))

;;devivation = (sqrt((sum x^2)/(n-1) - aver^2));
;;criterion  = aver - dev/2

(defun CRITERION (l)
  (when (= (length l) 1)
    (return-from criterion (cdr (first l))))
  (let ((sum 0))
    (dolist (x l)
      (incf sum (cdr x)))
    (let ((temp (sum-sqr l))
          (aver (/ sum (length l))))
      (- aver (/ (sqrt (- (/ temp (1- (length l))) (* aver aver))) 2)))))

(defun SIM-COEF (l1 l2)
  (let* ((t1 (pair-mult (sim l1 l2)))
         (t2 (sum-sqr l1))
         (t3 (sum-sqr l2)))
    (/ t1 (sqrt (* t2 t3)))))

(defun ADJ-FILT (l prev ans)
  (cond
    ((and (eq (second l) nil) (> (cdr (first l)) prev))
     (cons (first l) ans))
    ((eq (second l) nil)
     ans)
    ((and (> (cdr (first l)) prev) (> (cdr (first l)) (cdr (second l))))
     (adj-filt (cdr l) (cdr (first l)) (cons (first l) ans)))
    (t
     (adj-filt (cdr l) (cdr (first l)) ans))))


(defun TOKENIZER (fname table)
  (let ((sentence -1)
        (word-counter 0)
        word
        (newline-list '()))
    (with-open-file
        (inp fname :direction :input :if-does-not-exist :error)
      ;; (format t "Counting sentence words...~%")
      (loop
       do (when (zerop word-counter)
            (incf sentence)
            (setf word-counter +words-in-sentence+))
       (setf word (read-word-s inp))
       until (null word)
       do (unless
           (or (divided-char-p-s word)
               (real-sent-end-p word)
               (special-words-p (str-morph-s word))
               (punctuation-p (str-morph-s word)))
            (if (equal (char word 0) #\&)
              (progn
                (read-word-s inp)
                (pushnew sentence newline-list))
              (progn
                (decf word-counter)
                (if (gethash (cons (str-morph-s word) sentence) table)
                  (incf (gethash (cons (str-morph-s word) sentence) table))
                  (setf (gethash (cons (str-morph-s word) sentence) table) 1)))))
    (setf *sentence-num* (1+ sentence))))
    (nreverse newline-list)))


(defun BUILD-GAP-ARR (table)
  (let ((arr (make-array (list *sentence-num*) :initial-element nil))
        (gap-arr (make-array (list (1+ (- *sentence-num* +block-size+)))
                              :initial-element nil)))
    
    (maphash #'(lambda (key val)
                 (push (cons (car key) val) (aref arr (cdr key))))
             table)
    (loop as i from 0 to (1- +block-size+)
          do (setf (aref gap-arr 0)
                   (token-add (aref gap-arr 0) (aref arr i))))
    (loop as i from 1 to (- *sentence-num* +block-size+)
          do (setf (aref gap-arr i)
                   (copy-tree (aref gap-arr (1- i))))
             (setf (aref gap-arr i)
                   (token-subtract (aref gap-arr i) (aref arr (1- i))))
             (setf (aref gap-arr i)
                   (delete-if #'(lambda(x) (zerop (cdr x)))
                           (aref gap-arr i)))
             (setf (aref gap-arr i)
                   (token-add (aref gap-arr i)
                           (aref arr (+ i (1- +block-size+))))))
  gap-arr))


(defun BUILD-SIM-ARR (gap-arr)
  (let ((sim-arr (make-array (list (1+ (- *sentence-num* +block-size+)))
                              :initial-element nil)))
    (with-open-file
     (out "SimArray.dat" :direction :output
          :if-exists :supersede :if-does-not-exist :create)
     (format out "# Sim-Array =~%")
     (loop as i from +block-size+ to (- *sentence-num* +block-size+)
           do (setf (aref sim-arr i)
                    (sim-coef (aref gap-arr (- i +block-size+))
                              (aref gap-arr i)))
           (format out "~a ~a ~%" i (aref sim-arr i))))
    sim-arr))

(defun BUILD-DEPTH-SCORE-LIST (depth-list sim-arr)
  (loop as i from +block-size+ to (- *sentence-num* +block-size+)
        do 
        (push (cons i (depth-sc sim-arr i)) depth-list))
  depth-list)


(defun BOUNDARY-IDENTIFICATION (depth-list)
  (let* ((filt-depth-list (adj-filt depth-list -1 nil))
         (bound (criterion filt-depth-list)))
    (setf depth-list (delete-if #'(lambda(x) (> bound  (cdr x)))
                                filt-depth-list)))
;  (setf depth-list (sort depth-list #'< :key #'car))
   (setf depth-list (sort depth-list #'> :key #'cdr))
  (with-open-file
   (out "DepthList.dat" :direction :output
        :if-exists :supersede :if-does-not-exist :create)
   (format out "# Depth-List =~%")
   (dolist (x depth-list)
           (format out "~a ~a~%" (car x) (cdr x))))
; (format t "~a~%" depth-list)
  depth-list)


(defun ADJ-BOUND (parag-list key)
  (unless (eq nil key)
    (cond
      ((member key parag-list)
       key)
      ((member (1- key) parag-list)
       (1- key))
      ((member (1+ key) parag-list)
       (1+ key))
      ((member (- key 2) parag-list)
       (- key 2))
      ((member (+ 2 key) parag-list)
       (+ 2 key))
      (t
       nil))))



(defun UPDATED-BOUND (parag-list depth-list res)
   (let ((val (adj-bound parag-list (caar depth-list))))
    (cond
      ((eq depth-list nil)
       (sort res #'< :key #'car))
      ((eq parag-list nil)
       (sort res #'< :key #'car))
      (val
       (updated-bound (delete val parag-list)
                      (cdr depth-list) (cons (cons val (cdr (car depth-list)))
                                              res)))
      (t
       (updated-bound parag-list
                      (cdr depth-list) res)))))

(defun SEGMENTS-IN-REAL-SENT (depth-list fname)
  (let ((sentence -1)
        (word-counter 0)
        (real-sent 0)
        (result nil)
        word)
   ; (format t ">>> RES=~a~%" depth-list)
    (with-open-file
        (inp fname :direction :input :if-does-not-exist :error)
      ;; (format t "Counting sentence words...~%")
         (loop
           do (when (zerop word-counter)
            (incf sentence)
            (setf word-counter +words-in-sentence+))
         (setf word (read-word-s inp))
         until (null word)
         do 
            (if (real-sent-end-p word)
              (incf real-sent)
            (unless
              (or (divided-char-p-s word)
                (special-words-p (str-morph-s word))
                (punctuation-p   (str-morph-s word)))
             (if (equal (char word 0) #\&)
              (progn
                (read-word-s inp)
                (when (and depth-list (= sentence (caar depth-list)))
                  (push real-sent result)
                  (pop depth-list)))
              (decf word-counter))))))
  (push real-sent result)
  (reverse result)))


(defun DEPTH-SC (sim-arr i)
  (let ((center i)
        (flag t)
        (peak i)
        (res 0))
    ;; Go left as long as the value of the gap is increasing
    (loop
     do
     (cond
       ((> i +block-size+)
        (setf flag (> (aref sim-arr (1- i)) (aref sim-arr i)))
        (unless flag (setf peak i))
        (decf i))
       (t
        (setf flag nil)
        (setf peak i)
        ))
     (setf res (- (aref sim-arr peak) (aref sim-arr center)))
     until (eq flag nil))
    ;; Go right as long as the value of the gap is increasing
    (setf flag t)
    (loop
     do
     (cond
       ((< i (- *sentence-num* +block-size+))
        (setf flag (> (aref sim-arr (1+ i)) (aref sim-arr i)))
        (unless flag (setf peak i))
        (incf i))
       (t
        (setf flag nil)
        (setf peak i)))
     (incf res (- (aref sim-arr peak) (aref sim-arr center)))
     until (eq flag nil))
    res))



(defun PRINT-WORDS-SUM (arr)
  (map 'list
       #'(lambda (l)
           (let ((sum 0))
             (dolist (x l)
               (incf sum (cdr x)))
             sum))
       arr))


(defun SEGMENT (fname)
  (let ((table (make-hash-table :test #'equal))
        (depth-list '())
        gap-arr sim-arr
        newline-list)
    (setf newline-list (tokenizer fname  table))
      (if (> (length newline-list) 1)
        (progn  
          (setf gap-arr (build-gap-arr table))
          (setf sim-arr (build-sim-arr gap-arr))
          (setf depth-list (build-depth-score-list depth-list sim-arr))
          (when depth-list
            (setf depth-list (boundary-identification depth-list)))
          (segments-in-real-sent (updated-bound newline-list depth-list '())
                                 fname))
        newline-list)))
