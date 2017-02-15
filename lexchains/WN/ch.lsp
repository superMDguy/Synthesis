;;; -*- Mode: LISP; Syntax: Common-lisp; Package: COMMON-LISP-USER; Base: 10 -*-

(in-package :COMMON-LISP-USER)

(defconstant +END-SENTENCE-IN-PARS+ "!")

;; The maximal number of sentences from first word of the chain to the next one.
(defconstant +WAIT-SIZE+   5)

(defconstant +ACTIVE-INTERPRETATION+ 10)

(defconstant +INFINITY+ 55)

(defun INTRESTING-WORD (word)
  (not (member word '("time" "way" "case" "type" "kind" "object" "idea"
                      "piece" "part" "point" "example" "thing" "use"
                      "number" "month" "year" "day" "work" "post")
                    :test #'equalp)))

(defun MORPH-NOUN-COMPOUND (noun-compound)
  "Extract normal morphological form from parser output.
   For example,quantum(1)-computer(2)/@n_seq/quantum-computer."
 (let ((type-morph (subseq noun-compound (1+ (position #\/ noun-compound)))))
   (subseq type-morph (1+ (position #\/ type-morph)))))

(defun HEAD (m-noun-compound)
  (let (n-c)
  "Extract head from noun-compound."
    (if (consp m-noun-compound)
      (setf n-c (caar m-noun-compound))
      (setf n-c m-noun-compound))
  (let* ((str (reverse n-c))
         (pos (position #\- str)))
    (if pos
      (reverse (subseq str 0  pos))
      n-c))))
  

;;; ---------------------------------------------------------------------------
;;; ---------------------------------------------------------------------------

(defstruct (CONNECTION (:conc-name con-))
  mem ; first element
  type ; connection type
 ; abstract-level ;level of common antedecent and words in hypernym tree
  )

;;; ---------------------------------------------------------------------------

(defstruct (ENTRY-IN-CHAIN (:conc-name entry-))
  num       ; serial number in chain
  sent      ; sentence number for this word
  head-lex  ; head string
  noun-compound ; noun compound string
  sense     ; list of senses for this word entry, usually from all senses to one
  (rel nil) ; entries in this chain that this entry is linked to
  )

(defun NEW-ENTRY (word sentence number given-sense)
  (make-entry-in-chain :num   number
                       :sent  (if (consp sentence) sentence (list sentence))
                       :head-lex  (head word)
                       :noun-compound
                         (if (consp word) word (list (cons word 1)))
                       :sense given-sense))

(defun COPY-ENTRY (entry)
  (make-entry-in-chain :num   (entry-num   entry)
                       :sent  (copy-list (entry-sent  entry))
                       :head-lex   (entry-head-lex   entry)
                       :noun-compound (copy-list (entry-noun-compound entry))
                       :sense (entry-sense entry)
                       :rel   (entry-rel entry)))

(defun REPETITION-ENTRY (entry)
  (length (entry-sent entry)))

(defun PRINT-ENTRY (entry)
  (mapcar #'(lambda(x) (format t "~a ~a " (car x) (cdr x)))
           (entry-noun-compound entry)))

(defun PRINT-HEAD-ENTRY (entry)
 (format t "~a " (entry-head-lex entry)))
                    
(defun INSERT-IN-CHAIN (word word-sense sentence chain)
  (let* ((new-mem (new-entry word sentence (chain-length chain) word-sense)))
    (dolist (entry (chain-entry-list chain))
      (let ((common-sense
             (connected (entry-head-lex entry) word
                        (entry-sense entry) (entry-sense new-mem))))
        (when common-sense
          (setf (entry-sense entry) (third common-sense))
          (update-chain-connectness chain (first common-sense) (repetition-entry entry))
          (let ((new-connection
                 (make-connection :mem (entry-head-lex entry)
                                  :type (first common-sense)
                                  ;:abstract-level (fourth common-sense)
                                  )))
            (push new-connection (entry-rel new-mem))))))
    (push new-mem (chain-entry-list chain)))
  chain)

;;; ---------------------------------------------------------------------------

(defstruct (LEXICAL-CHAIN (:conc-name chain-))
  ;;dist-sum   ; normalized distance between entries on this chain
  ;;first-sent ; first sentence of this chain
  (connectness 0) ;measure of connectness between entries
  entry-list ; list of entries, in reverse order
  )

(defun CHAIN-LENGTH (chain)
  (length (chain-entry-list chain)))

(defun NEW-CHAIN (entry)
  (make-lexical-chain ;;:dist-sum   0
                       ;;:first-sent 0
                       :connectness 0
                       :entry-list (list entry)))

(defun COPY-CHAIN (old)
  (make-lexical-chain ;;:dist-sum   (chain-dist-sum old)
                      ;;:first-sent (chain-first-sent old)
                      :connectness (chain-connectness old)
                      :entry-list (mapcar #'copy-entry
                                          (chain-entry-list old))))
(defun UPDATE-CHAIN-CONNECTNESS (chain type rep)
  (cond
    ((equal type ':man) (incf (chain-connectness chain) ( * 10 rep)))
    ((equal type ':syn) (incf (chain-connectness chain) ( * 6 rep)))
    ((equal type ':ant) (incf (chain-connectness chain) ( * 6 rep)))
    ((equal type ':mer) (incf (chain-connectness chain) ( * 4 rep)))
    ((equal type ':hyp) (incf (chain-connectness chain) ( * 1 rep)))))

(defun CHAIN-WORDS-NUMBER (chain)
  (let ((sum 0))
    (dolist (entry (chain-entry-list chain))
      (incf sum (repetition-entry entry)))
    sum))


(defun IF-WORD-RELATED-TO-CHAIN (chain noun-compound word-sense)
  (dolist (entry (chain-entry-list chain))
    (let ((common-sense (connected (entry-head-lex entry) (head noun-compound)
                                   (entry-sense entry) word-sense)))
      (when common-sense
       (return-from if-word-related-to-chain t))))
  nil)

(defun ADD-IF-REPEATED-IN-CHAIN (noun-compound sentence chain)
  "When a word is found in one chain, can stop immediately."
  (let ((rep nil)
        (new-mem nil)
        (word (head noun-compound)))
    (dolist (entry (chain-entry-list chain))
      (when (equalp word (entry-head-lex entry))
        (push sentence (entry-sent entry))
        (setf (entry-noun-compound entry)
              (assoc-lists-merge (entry-noun-compound entry)
                                 (list (cons noun-compound 1))))
        (setf rep t))
        (setf new-mem entry))
    (when rep
     (dolist (entry (chain-entry-list chain))
        (let ((common-sense
             (connected (entry-head-lex entry) word
                        (entry-sense entry) (entry-sense new-mem))))
          (when common-sense
            (setf (entry-sense entry) (third common-sense))
            (if (equalp word (entry-head-lex entry))
              (update-chain-connectness chain (first common-sense) (1- (repetition-entry entry)))
             (update-chain-connectness chain (first common-sense) (repetition-entry entry)) )))))
  ;; Not found in any chain
  rep))

(defun PRINT-CHAIN (chain)
    (format t "    ~{~a~^, ~}~%"
               (mapc #'print-entry (chain-entry-list chain));)
    (format t "~%")))

(defun PRINT-HEAD-CHAIN (chain)
  (mapc #'print-head-entry (chain-entry-list chain));
  (format t "~%"))


;;; ---------------------------------------------------------------------------

(defstruct (INTERPRETATION (:conc-name interpret-))
  "An interpretation is a list of chains that can all co-occur together
   without contradictions in the selected senses of each member of each chain."
  chain-list)

(defun NEW-INTERPRETATION (chain-list)
  (make-interpretation :chain-list chain-list))


(defun MY-COPY-INTERPRETATION (old)
  ;(format t "COPY-INTERPRETATION~%")
  (make-interpretation :chain-list
                       (mapcar #'copy-chain  (interpret-chain-list old))))

(defun UPDATE-INTERPRETATION (interpret chain word word-sense sentence)
  ;(format t "UPDATE-INTERPRETATION~%")
  (setf (interpret-chain-list interpret)
         (delete chain (interpret-chain-list interpret) :test #'equalp))
  (new-interpretation
        (cons (insert-in-chain word word-sense sentence chain)
              (interpret-chain-list interpret))))


(defun ADD-IF-REPEATED-IN-INTERPRETATION (word sentence interpret)
  "When a word is found in one chain, can stop immediately."
  (dolist (chain (interpret-chain-list interpret))
    (when (add-if-repeated-in-chain word sentence chain)
      (return-from add-if-repeated-in-interpretation t)))
  ;; Not found in any chain
  nil)

(defun IF-WORD-RELATED-TO-INTERPRETATION (interpret word word-sense)
  ;(format t "IF-WORD-RELATED-TO-INTERPRETATION~%")
  (dolist (chain (interpret-chain-list interpret))
    (when (if-word-related-to-chain chain word word-sense)       
      (return-from if-word-related-to-interpretation t)))
  nil)


(defun INTERPRET-LENGTH (interpret)
  (length (interpret-chain-list interpret)))

(defun INTERPRETATION-WORDS-NUMBER (interpret)
  (let ((sum 0))
    (dolist (chain (interpret-chain-list interpret))
      (incf sum (chain-words-number chain)))
    sum))

(defun INTERPRETATION-CONNECTNESS (interpret)
  (let ((sum 0))
    (dolist (chain (interpret-chain-list interpret))
      (incf sum (chain-connectness chain)))
    sum))
  
(defun PRINT-INTERPRETATION (interpret)
 ;; (format t "Connectness per chain in interpretation ~a~%"
 ;;         (/(interpretation-connectness interpret)
 ;;                          (interpretation-words-number interpret)))
  (dolist (chain (interpret-chain-list interpret))
    (print-chain chain)))
  
;;; ---------------------------------------------------------------------------
(defstruct COMPONENT
  "A component encodes list of interpretations.  
Within a component, interpretations are exclusive of each other.
There is a separate component for each semantically related set of words.
At the end of the processing, one must select one representative from each
component.
When adding a new noun with many senses, new interpretations can be created
in existing components; components can be merged as a result; or a new
component can be created for the new noun."
  (interprets nil)) ;; interprets is a list of interpretilities, each a list of chains

(defmacro NEW-COMPONENT (interprets)
  `(make-component :interprets ,interprets))


(defvar *component-list* nil
  "The list of components currently active.")

(defun COMPONENT-LENGTH (component)
  (length (component-interprets component)))

(defun PRINT-COMPONENT-LIST (component-list)
  (format t "Component list printout~%=========================~%")
  (dolist (component component-list)
    (dolist (interpret (component-interprets component))
      (print-interpretation interpret)
      (format t "int-con ~a~%" (interpretation-connectness interpret))
      (format t "  -----------------------~%"))
    (format t "************************~%"))
  (format t "=========================~%"))


;; ======================================================================
;; Check if can find a semantic relation between a new word and a word
;; appearing in one of the chains of one of the interpretations of the
;; component. 
;; ======================================================================

(defun IF-WORD-RELATED-TO-COMPONENT (comp word word-sense)
  (dolist (interpret (component-interprets comp))
    (when (if-word-related-to-interpretation interpret word word-sense)
      (setf *component-list*
            (delete comp *component-list* :test #'eq))
      (return-from if-word-related-to-component t)))
  nil)

;; ======================================================================
;; Insert a new related word to a component: this results in splitting
;; interpretilities: on each interpretility does the following:
;; ((w11 w12) (w21 w22 w23)) + w -->
;; ((w11 w12 w) (w21 w22 w23)) +         [if exists link w1-w]
;; ((w11 w12) (w21 w22 w23 w)) +         [if exists link w2-w]
;; ((w11 w12) (w21 w22 w23) (w))
;; ======================================================================

(defun METRICA-OF-CONNECTNESS (x y)
  (< (interpretation-connectness x)
    (interpretation-connectness y)))


(defun FILTERING-COMPONENT (found)
  (when (> (length found) +active-interpretation+)
   ;; (let ((words-num (interpretation-words-number (first found))))   
      (setf found (nthcdr (- (length found) +active-interpretation+)  
                          (sort found #'metrica-of-connectness))))
  found)


(defun SPLIT-INTERPRET-LIST (interpret-list noun-compound word-sense sentence)
  (let ((found nil)
        (word (head noun-compound)))
    (dolist (interpret interpret-list)
      (dolist (chain (interpret-chain-list interpret))
        (dolist (entry (chain-entry-list chain))
          (let ((common-sense (connected (entry-head-lex entry) word
                               (entry-sense entry) word-sense)))
            (when common-sense
              (pushnew (update-interpretation
                          (my-copy-interpretation interpret)
                          (copy-chain chain)
                          noun-compound (second common-sense) sentence)
                    found :test #'equalp)))))
       (when found    
             (pushnew (new-interpretation
                       (cons (new-chain (new-entry noun-compound
                                                   sentence 0 word-sense))
                             (interpret-chain-list
                              (my-copy-interpretation interpret))))
               found :test #'equalp)))
    (setf found (filtering-component found))
    (new-component found)))




;; ======================================================================
;; Deal with repetitions of words
;; ======================================================================

(defun ADD-IF-REPEATED (word sentence component)
  "Check if same word already appeared previously - in this case assume the
word reappears with the same sense. Destructively update the chains."
  (let ((repeated nil))
    (dolist (interpret (component-interprets component))
      (when (add-if-repeated-in-interpretation word sentence interpret)
        (setf repeated t)))
    repeated))



;; ======================================================================
;; Decay to avoid influence of words from far away
;; ======================================================================

(defun BUILD-RESULT-CHAINS (component-list)
  (let ((result-chains nil))
    (dolist (component component-list)
      (let ((x (first (sort (component-interprets component)
                    #'(lambda (x y)
                              (> (interpretation-connectness x)
                                 (interpretation-connectness y)))))))
        (when (> (interpretation-words-number x) 1)
          (setf result-chains
                (append (interpret-chain-list x) result-chains)))))
    (setf result-chains
        (delete-if #'(lambda (x) (eq (chain-words-number x) 1)) result-chains))
  result-chains))

;;; ---------------------------------------------------------------------------

(defun NEW-WORD (word sentence)
  "Insert a new noun read from sentence number sentence in the text into
the components."
  ;(format t ">>> New-Word: ~a   ~%"
  ;        word )
  (let ((word-senses (get-meaning :noun (head word))))
    (let ((united nil))
      ;; Check if new noun can be inserted in an existing component.
      ;; If the word is a repetition of a previous occurrence, do not split
      ;; the interpretations - just add the occurrence with the same senses.
      (dolist (comp *component-list*)
        (let ((repeated (add-if-repeated word sentence comp)))
          (if repeated
            (return-from new-word t)
            (when (if-word-related-to-component comp word word-senses)
                             (push (component-interprets comp) united)))))
      ;; Must merge all components that are now related through one of the
      ;; senses of the new noun - else create a new component just for the
      ;; new noun.
      (if united
        (progn
          (setf united (cartesian-mult united))
          (let ((tu (split-interpret-list united word word-senses sentence)))
          (push tu
                *component-list*)))
        (new-component-from-one-word word word-senses sentence)))))
  ;(format t "MU")
  ;(print-component-list *component-list*))

(defun new-component-from-one-word (word word-senses sentence)
  (push 
   (new-component
    (list (new-interpretation
           (list (new-chain (new-entry word sentence 0 word-senses))))))
   *component-list*))



(defvar *sentence* 0)


(defun BUILD-CHAIN-PER-SEGMENT (inp seg)
  "Main function: scan a segment file word by word.  For each noun in the
stream, insert the word in all chains were it can be connected through a
semantic relation.  This can lead to splitting of possibilities within
components to accomodate for the new word."
  (setf *component-list* nil)
  (let ((seg-bound (seg-right-bound seg))
        (word-num 0)
        word)
    (loop 
      do (setf word (read-word inp))
          (unless word
            (return-from build-chain-per-segment
                         (build-result-chains *component-list*)))
         (cond
          ((equal word +END-SENTENCE-IN-PARS+)
           (incf *sentence*))
          ((intresting-word (head (morph-noun-compound word)))
           (incf word-num)
           (incf *count*)
           (new-word (morph-noun-compound word) *sentence*)))
         until (eq *sentence* seg-bound))
  (setf (seg-noun-num seg) word-num))
  (build-result-chains *component-list*))



(defun BUILD-SEGMENT-L (segment-div-l)
  "Build list of segment structures from the list of sentences number that
   relate to Hearst segmentation."
  (let ((seg-num (length segment-div-l))
        (res-l '()))
    (loop as i from 0 to (1- seg-num)
      do  (push (make-segment-entry :left-bound
                                      (if (eq i 0)
                                           0
                                           (nth (1- i) segment-div-l))
                                    :right-bound
                                      (1- (nth i segment-div-l)))
                                    res-l))
    (reverse res-l)))


(defvar *count* 0)

(defun BUILD-CHAIN(fname segment-div-l)
  (let ((global-chain-list nil))
    (setf *sentence* 0)
    (setf *count* 0)
    (setf *segment-l* (build-segment-l segment-div-l))
    (with-open-file 
          (inp fname :direction :input :if-does-not-exist :error)
      (dolist (seg *segment-l*)
        (setf global-chain-list
           (append (build-chain-per-segment inp seg) global-chain-list)))
    (setf *final-chains*
          (merge-chains (merge-chains global-chain-list '()) '() ))
    (setf *final-chains*
          (sort *final-chains*
                #'(lambda (x y)
                  (> (* (chain-words-number x) (- 1.0 (/ (chain-length x)
                                                  (chain-words-number x))))
                     (* (chain-words-number y) (- 1.0 (/ (chain-length y)
                                                 (chain-words-number y)))))))))
  *final-chains*))


(defun RELATED-CHAINS-P (ch1 ch2)
  (dolist (entry1 (chain-entry-list ch1))
    (dolist (entry2 (chain-entry-list ch2))
      (when (find-common-sense (entry-sense entry1) (entry-sense entry2))
        (return-from related-chains-p t))))
  nil)

(defun UNITE-CHAINS (ch1 ch2)
  (dolist (entry (chain-entry-list ch2))
    (ins-in-chain (entry-noun-compound entry) (entry-sense entry)
                     (entry-sent entry) ch1))
  ch1)
  



(defun MERGE-CHAINS (global-l res)
  (let ((united-l nil))
    (if (null global-l)
      res
      (let ((f (car global-l))
            (new-l nil))
        (push f united-l)
        (dolist (ch2 (cdr global-l))
          (if (related-chains-p f ch2)
            (push ch2 united-l)  
            (push ch2 new-l)))
        (merge-chains new-l (cons (reduce #'unite-chains united-l) res))))))
  
;; ======================================================================
;; General utilities
;; ======================================================================

(defun CROSS-PROD (interpret1-l interpret2-l)
  "non-destructive - cross product of two lists"
  (let ((result nil))
    (dolist (interpret1 interpret1-l)
        (dolist (interpret2 interpret2-l)      
          (push (new-interpretation (append (interpret-chain-list interpret1)
                                            (interpret-chain-list interpret2)))
                result)))
    result))

(defun CARTESIAN-MULT (l)
  (cond ((null l) nil)
        ((null (rest l)) (first l))
        (t (reduce #'cross-prod l))))


(defun ASSOC-LISTS-MERGE (l1 l2)
  (dolist (i l1)
    (let ((x (assoc (car i) l2 :test 'equalp)))
      (if x
        (rplacd x (+ (cdr x) (cdr i)))
        (push i l2))))
  l2)

(defun INS-IN-CHAIN (noun-compound word-sense sentence chain)
  "When a word is found in one chain, can stop immediately."
  (let ((rep nil)
        (new-mem nil)
        (word (head noun-compound)))
    (dolist (entry (chain-entry-list chain))
      (when (equalp word (entry-head-lex entry))
        (setf (entry-sent entry) (append sentence (entry-sent entry)))
        (setf (entry-noun-compound entry)
              (assoc-lists-merge (entry-noun-compound entry) noun-compound))
        (setf rep t))
        (setf new-mem entry))
    (unless rep
      (setf new-mem (new-entry noun-compound sentence
                               (chain-length chain) word-sense))) 
    (dolist (entry (chain-entry-list chain))
      (let ((common-sense
           (connected (entry-head-lex entry) word
                      (entry-sense entry) (entry-sense new-mem))))
        (when common-sense
          (setf (entry-sense entry) (third common-sense))
          (if (equalp word (entry-head-lex entry))
            (update-chain-connectness chain (first common-sense)
                                       (1- (repetition-entry entry)))
            (progn
              (update-chain-connectness chain (first common-sense)
                                       (repetition-entry entry))
              (let ((new-connection
                (make-connection :mem (entry-head-lex new-mem)
                                 :type (first common-sense))))
                (pushnew new-connection (entry-rel entry)
                                                :test #'equalp)))))))
    (unless rep
      (push new-mem (chain-entry-list chain)))
  chain))
