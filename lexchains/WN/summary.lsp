

(in-package :COMMON-LISP-USER)

(defstruct (CHAIN-IN-SEG)
  chain-num; number of chain in result list;
  sent-list; list of sentences in chain;
  )
            

(defstruct (SEGMENT-ENTRY (:conc-name seg-))
  chains-in-seg ; list of chains which is in segment
  right-bound   ; segment right boundary
  left-bound    ; segment left boundary
  extract-l     ;list of sentences extracted for summary
  noun-num      ;number of nouns in segment
  )

(defstruct (CLUSTERED-CHAIN-SEGMENT (:conc-name clust-))
  begin        ; start segment
  end          ; end segment
  ch-word-num  ; number og chain words in clustered segment
)  
;_______________________________________________________________________________
;UTILITIES FOR INPUT/OUTPUT OPERATIONS FOR SUMMARY PRINTING.
;_______________________________________________________________________________


(defun CONT-READ (s first-ch pred)
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

(defun READ1-WORD (s)
  (let (c word)
    (setf c (read-char s nil nil))
      (cond
        ((null c)(return-from read1-word nil))
        ((white-char-p-s c)
           (setf word (cont-read s c #'(lambda(x) (not (white-char-p-s x))))))
        (t (setf word (cont-read s c #'white-char-p-s))))
      word))

(defun COMPRESS-STAT (out extract-l seg-bound-l)
  (format out "<BR> <BR> <BR>")
  (format out "<CENTER> <H1><FONT SIZE=5 COLOR=PURPLE>
                            STATISTICS
              </FONT></H1></CENTER>")
  (format out "<CENTER> <TABLE BORDER>~%")
  (format out "<tr><th>Number of  </th>
               <th>In text</th>
               <th>In summary</th>
               <th>Ratio</th></tr>~%")
  (format out "<tr><th>Sentences  </th>
              <th>~a</th>
              <th>~a</th>
              <th>~a</th>
              </tr>~%"
              (car (last seg-bound-l)) (length extract-l)
              (* 100.0 (/ (length extract-l) (car (last seg-bound-l)))))
  (let ((seg-in-sum (length (remove-duplicates
              (mapcar #'(lambda(x) (find-segment x seg-bound-l)) extract-l)))))
  (format out "<tr><th>Paragraph  </th>
                   <th>~a</th>
                   <th>~a</th>
                   <th>~a</th>
                   </tr>~%"
              (length seg-bound-l) seg-in-sum
                  (* 100.0 (/ seg-in-sum (length seg-bound-l)))))
  (format out "</TABLE BORDER></CENTER>~%")
  (format out "<BR><BR><BR><BR><BR>"))
  
  

(defun READ-WRITE-SENT (write-p inp out)
  "Read and write sentence from morph output.
  Return NIL after each sentence -- T when eof reached."
 (let ((word nil))
   (loop 
    do (setf word (read1-word inp))
    until (null word)
    do
    (cond
     ((real-sent-end-p word)
       (incf *sentence*)
       (when write-p (format out "~a" word))
       (return-from read-write-sent nil))
      ((white-char-p-s (char word 0))
       (when write-p (format out "~a" word)))              
      ((char= (char word 0) #\&)
       (read1-word inp)
       (when write-p (format out "<p>")))
      (t             
       (when write-p (format out "~a" (str-lex word)))))))
 ;; Reached eof -- return T
 T)


(defun MAKE-TITLE (fname method)
  (let ((title (format nil "~a ~a" fname method)))
    title))

(defun PRINT-SUMMARY (extract-l inpname out seg-bound-l
                                &optional (print-all t))
  (let ((print-green nil)
        (eof nil)
        (*sentence* 0))
    (declare (special *sentence*))
    (with-open-file
        (inp inpname :direction :input :if-does-not-exist :error)
        (loop
         do
         (setf print-green (and (not (null extract-l))
                                (eq *sentence* (car extract-l))))
         (when (and print-green)
           (when print-all
             (format out "<FONT SIZE=2 COLOR= GREEN >"))
           (pop extract-l))
         (if (or print-all print-green)
           (progn
             (setf eof (read-write-sent t inp out))
             (when (eq *sentence* (car seg-bound-l))
               (format out "<HR>")
               (pop seg-bound-l)))
           (setf eof (read-write-sent nil inp out)))
         (when (and print-green print-all)
           (format out "</FONT>"))
         until eof))))


(defun PRINT-SUMMARY-PAGE (extract-l inpname outname seg-bound-l title)
  (with-open-file
    (out outname :direction :output
                 :if-exists :supersede :if-does-not-exist :create)
    (format out "<HTML>~%")
    (format out "<BODY>~%")
    (compress-stat out extract-l seg-bound-l)
    (format out "<TITLE>~a</TITLE>" title)
    (format out "<CENTER> <H1><FONT SIZE=5 COLOR=PURPLE>SUMMARY</CENTER></FONT></H1>")
    (format out "<BR>")
    (format out "<FONT SIZE =4>~%")
    (print-summary extract-l inpname out seg-bound-l nil)
    (format out "</FONT>")
    (format out "<BR><BR><BR><BR><BR><BR>")
    (format out "<CENTER> <H1><FONT SIZE=5 COLOR=PURPLE>SUMMARY DISTRIBUTION IN THE TEXT</CENTER></FONT></H1>")
    (format out "<BR>")
    (format out "<BLOCKQUOTE>")
    (format out "<FONT SIZE =2>~%")
    (print-summary extract-l inpname out seg-bound-l t)
    (format out "</BLOCKQUOTE>")
    (format out "</FONT>")
    (format out "</BODY>")))
    

;_______________________________________________________________________________


(defun FIRST-CHAIN-SENTENCE (chain)
  "Find the first appearence of chain in text."
  (let ((first-sent 1000))
    (dolist (entry (chain-entry-list chain))
      (let ((min (minimal (entry-sent entry))))
        (when (> first-sent min)
          (setf first-sent min))))
    first-sent))

(defun BUILD-FIRST-APPEARENCE-CHAIN-SUMMARY (fin-ch seg-bound-l
                                             num-result-chains
                                             fname outdir
                                             &optional (outfname "Summary1"))
  "Extract first sentence for each important chain."
  (let ((extract-l nil)
        (inpfname (format nil "~a~a.tag" outdir fname))
        (outfname (format nil "~a/~a" outdir outfname)))
    (dolist (ch-n num-result-chains)
      (pushnew (first-chain-sentence (nth (1- ch-n) fin-ch))
            extract-l))
   (setf extract-l (sort extract-l #'<))
   (print-summary-page extract-l inpfname outfname seg-bound-l
                  (make-title fname "First Appearance Summary"))))
;_______________________________________________________________________________


(defun FIRST-IMPORTANT-CHAIN-SENTENCE (chain)
  "Find the first appearence of important chain word in text.
   Important word is word with frequency larger than average frequency
   in chain."
  (let ((first-sent 1000)
        (aver-freq (/ (chain-words-number chain) (chain-length chain))))
    (dolist (entry (chain-entry-list chain))
      (when (>= (repetition-entry entry) aver-freq)
        (let ((min (minimal (entry-sent entry))))
          (when (> first-sent min)
            (setf first-sent min)))))
    first-sent))


(defun BUILD-FIRST-IMPORTANT-APPEARENCE-CHAIN-SUMMARY
                             (fin-ch seg-bound-l num-result-chains
                              fname outdir &optional (outfname "Summary2"))
  "Extract first sentence for each important chain."
  (let ((extract-l nil)
        (inpfname (format nil "~a~a.tag" outdir fname))
        (outfname (format nil "~a/~a" outdir outfname)))
    (dolist (ch-n num-result-chains)
      (pushnew (first-important-chain-sentence (nth (1- ch-n) fin-ch))
            extract-l))
   (setf extract-l (sort extract-l #'<))
   (print-summary-page extract-l inpfname outfname seg-bound-l (make-title fname "First Important Appearance Summary"))))

;_______________________________________________________________________________

(defun FIND-SEGMENT (sent seg-bound-l)
  "Find the segment  that contains the given sentence."
  (let ((i 0))
    (dolist (bound seg-bound-l) ;bound means right bound.
      (cond
        ((eq sent bound) (return-from find-segment (1+ i)))
        ((< sent bound) (return-from find-segment i))
        (t (incf i))))
    nil))

(defun RELATE-CHAIN-TO-SEGMENT (fin-ch ch-num seg-bound-l)
 "Relate words in chain to their segments - fill chain-in-seg field in
  segment structure.In addition,it build clustered segment array for
  the chain.Update *segment-l*."
  (let ((ch-seg-arr (make-array (length seg-bound-l)
                                        :initial-element nil))
        (clust-l nil)
        (in-clust nil))
    (dolist (entry (chain-entry-list (nth (1- ch-num) fin-ch)))
      (dolist (sent (entry-sent entry))
        (let ((seg-num (find-segment sent seg-bound-l)))
          (when seg-num
            (push sent (aref ch-seg-arr seg-num))))))
    (loop as i from 0 to (1- (length seg-bound-l))
      do 
      (if (aref ch-seg-arr i)
           (progn
             (if in-clust
               (progn
                 (setf (clust-end (car clust-l)) i)
                 (incf (clust-ch-word-num (car clust-l))
                       (length (aref ch-seg-arr i))))
               (progn
                 (push (make-clustered-chain-segment :begin i :end i
                                   :ch-word-num (length (aref ch-seg-arr i)))
                       clust-l)
                 (setf in-clust 1)))
              (push (make-chain-in-seg :chain-num ch-num
                                       :sent-list (aref ch-seg-arr i))
                    (seg-chains-in-seg (nth i *segment-l*))))
           (setf in-clust nil)))
    clust-l))

(defun CHOOSE-CLUSTERS-PICK (clust-l)
  "Find the picks of clusters according to the formula :
                  number of words in pick segment >= (average + 2*deviation)"
  (let ((crit (clust-stat-criterion clust-l)))
    (setf clust-l (delete-if #'(lambda(x) (< (clust-ch-word-num x) crit))
                             clust-l)))
  clust-l)


(defun FIRST-CHAIN-SENT-IN-SEG (seg-num ch-num)
   (minimal (chain-in-seg-sent-list
            (find-if #'(lambda(x) (eq (chain-in-seg-chain-num x) ch-num))
                      (seg-chains-in-seg (nth seg-num *segment-l*))))))
  

(defun BUILD-PEAK-SUMMARY (fin-ch num-result-chains seg-bound-l
                                  fname outdir &optional (outfname "Summary3"))
  (let ((extract-l nil)
        (inpfname (format nil "~a~a.tag" outdir fname))
        (outfname (format nil "~a/~a" outdir outfname)))
    (dolist (ch-n num-result-chains)
      (setf extract-l (append (mapcar #'(lambda(x)
                           (first-chain-sent-in-seg (clust-begin x) ch-n))
              (choose-clusters-pick
               (relate-chain-to-segment fin-ch ch-n seg-bound-l)))
            extract-l)))
   (setf extract-l (sort (delete-duplicates extract-l) #'<))
   (print-summary-page extract-l inpfname outfname seg-bound-l (make-title fname "Concentrated appearance Summary"))))

;_______________________________________________________________________________
"For every segment find the strongest chain(s)" 


(defun STRONG-CHAIN-IN-SEGMENT (segment)
  (let ((x (sort (seg-chains-in-seg segment) #'> :key #'(lambda (x)
                                               (length
                                                (chain-in-seg-sent-list x))))))
    (cons (chain-in-seg-chain-num (first x))
          (* 1.00 (/ (length (chain-in-seg-sent-list (first x)))
                  (seg-noun-num segment))))))

(defun CHOOSE-CHAIN-PER-SEGMENT (seg-l)
  (mapcar #'(lambda(x) (format t "~a~%" (strong-chain-in-segment x))) seg-l)) 

(defun NUM-CHAIN-MEM-IN-SEG (chain-num seg)
  (let ((chain-node (find chain-num
                  (seg-chains-in-seg seg)
                  :key  #'(lambda(x) (chain-in-seg-chain-num x)))))
    (if chain-node
      (length (chain-in-seg-sent-list chain-node))
      0)))

(defun CHAIN-DISTRIBUTION-DATA (chain-num seg-l)
  (let ((distr-l nil)
        (sum-ch 0)
        (sum-tot 0)
        res)
    (dotimes (i (length seg-l))
      (setf res (num-chain-mem-in-seg chain-num (nth i seg-l)))
      (if (> res 0)
        (progn
          (incf sum-ch res)
          (incf sum-tot (seg-noun-num (nth i seg-l)))
          (push (* 100.0 (/ sum-ch sum-tot)) distr-l))
        (progn
          (setf sum-ch 0)
          (setf sum-tot 0)
          (push 0 distr-l)))
      )
    (nreverse distr-l)))

(defun PRINT-CHAIN-DISTR-DATA (chains-num seg-l)
  (dotimes (i (1- chains-num))
    (format t "Chain num :~a ~a~%"(1+ i)
            (chain-distribution-data (1+ i) seg-l)))) 
;_______________________________________________________________________________
;UTILITIES
;_______________________________________________________________________________

(defun MINIMAL (l)
  (let ((min 1000))
    (dolist (x l)
      (when (> min x)
        (setf min x)))
  min))

;;========================================================================
;;devivation = (sqrt((sum x^2)/(n-1) - aver^2))

(defun SUM-OF-SQUARES (clust-l)
  (let ((sum 0))
    (dolist (clust clust-l)
      (incf sum (* (clust-ch-word-num clust) (clust-ch-word-num clust))))
    sum))

(defun CLUST-AVERAGE (clust-l)
  (let ((sum 0))
    (dolist (clust clust-l)
      (incf sum (clust-ch-word-num clust)))
    (/ sum (length clust-l))))

(defun CLUST-DEVIATION (clust-l)
  (when (eq (length clust-l) 1)
    (return-from CLUST-DEVIATION 0))
  (let ((aver (clust-average clust-l)))
   (sqrt
        (- (/ (sum-of-squares clust-l)
              (length clust-l))
           (* aver aver)))))

(defun CLUST-STAT-CRITERION (clust-l)
  (let ((average (clust-average clust-l)) 
        (deviation (clust-deviation clust-l)))
    (+ average  deviation)))
