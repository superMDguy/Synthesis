 
(in-package :COMMON-LISP-USER)

;;========================================================================
;; Building html-representation
;;========================================================================

(defun STR-TYPE (str)
  (let ((type-morph (subseq str (1+ (position #\/ str)))))
    (subseq type-morph 0 (position #\/ type-morph))))

(defun STR-MORPH (str)
  (let ((type-morph (subseq str (1+ (position #\/ str)))))
    (subseq type-morph (1+ (position #\/ type-morph)))))

(defun STR-LEX (str)
  (subseq str 0 (position #\/ str)))

;; Special words that are ignored.
(defun NOUN-P (str)
  (let ((part-of-speech (str-type str)))
    (or (equalp "NN" part-of-speech)
        (equalp "NNS"  part-of-speech)
        (equalp "NNP"  part-of-speech))))


(defun AUTHOR-SEGMENT-P (word)
  (equalp (str-morph word) "&"))



;;--------------------------------------------------------------------------
(defun WRITE-CHAIN-INIT-ENTRY (outfile chain num)
  (format outfile
          "<A HREF=\"textdata~a~:*.html\"> CHAIN ~a  </A>~% " num)
  (format outfile
          "&#28 &#28 &#28 <FONT SIZE=3 COLOR=BORDO> LENGTH ~a REPETITION ~a SCORE ~a </FONT> <BR> "
           (chain-words-number chain)
           (- 1.0 (/ (chain-length chain) (chain-words-number chain)))
           (chain-score chain))
  (dolist (entry (chain-entry-list chain))
    (mapcar #'(lambda(x) (format outfile "~a ~a " (car x) (cdr x)))
           (entry-noun-compound entry)))
  ;;(color-chain chain)
  (format outfile "<BR> <HR>~%"))

(defun WRITE-CHAIN-PAGE (chain seg-bound-l num inp outdir)
  (with-open-file
      (out (format nil "~atextdata~a.html" outdir
                   num)
           :direction :output
           :if-exists :supersede :if-does-not-exist :create)
      (let ((gifname (format nil "chaingraph~a.gif" num)))
        (format out "<HTML>~%<BODY>~%<FONT SIZE=2>~%")
        (write-chain-init-entry out chain num)
        (format out "<HR size=5> ~
                     <IMG SRC=~a>~%" gifname)
        (draw-chain-graph chain (format nil "~a~a" outdir gifname))
        (close out)
        (bold-chains (list num) seg-bound-l inp out outdir))))

(defun WRITE-CRITERIA (crit out)
  (format out "<CENTER><FONT SIZE=5 COLOR=BORDO>CRITERION ~a</FONT></CENTER><HR>"
                       crit))
   

(defun INITIAL-PAGE (result-chains seg-bound-l crit inp outdir)
  (with-open-file
      (out (format nil "~aText_psegm.html" outdir)
           :direction :output
           :if-exists :supersede :if-does-not-exist :create)
    (format out "<HTML>~%<BODY>~%<FONT SIZE=4>~%")
    (format out "~:(~r~) interesting chains found.<BR><HR size=5>~%"
            (length result-chains))
    (let ((i 0)
          (crit_thresh t))
      (dolist (ch result-chains)
        (when (and crit_thresh (< (chain-score ch) crit))
          (write-criteria crit out)
          (setf crit_thresh nil))
        (write-chain-init-entry out ch (incf i))
        (write-chain-page ch seg-bound-l i inp outdir)))
    (format out "</BODY>~%</HTML>~%")))

(defun COLOR-TYPE (type)
  (cond
    ((equal type :man) 'green)
    ((equal type :syn) 'red)
    ((equal type :mer) 'orange)
    ((equal type :ant) 'gray)
    ((equal type :hyp) 'blue)))


(defun DRAW-CHAIN-GRAPH (chain outfile)
  (let ((infile (format nil "~apu" outfile)))
    (with-open-file
         (out infile :direction :output
              :if-exists :supersede :if-does-not-exist :create)
     (format out "graph G {~%")
     (format out "node [style=filled color=MistyRose]~%")
     (dolist (entry (chain-entry-list chain))
       (when (gethash (entry-head-lex entry) *my-meaning-table*)
         (format out "~a [style=filled color=RosyBrown]~%"
                      (entry-head-lex entry))))
     (dolist (entry (chain-entry-list chain))
       (dolist (connect (entry-rel entry))
         (format out "~a -- ~a [color=\"~a\" ] ;~%"
                 (entry-head-lex entry)
                 (con-mem connect)
                 (color-type (con-type connect))
                 (con-type connect))))
     (format out "}~%"))
    (run-shell-command (format nil "./drawgraph ~a ~a" infile outfile))
    (run-shell-command (format nil "rm ~a" infile))))


;;========================================================================
;; Coloring choosen chains on the text.
;;========================================================================


(defun BUILD-COLOR-TABLE (color-table ch color)
;  (format t ">> BUILD-COLOR-TABLE~%")
  (dolist (entry (chain-entry-list ch))
    (unless (gethash (entry-head-lex entry) color-table)
      (setf (gethash (entry-head-lex entry) color-table) color)))
  color-table)

(defun COLORED-TEXT (color-table seg-bound-l inpfname outpfname)
 ; (format t  ">> COLORED-TEXT0 ~%") 
  (let ((sentence 0)
        word)
    (with-open-file
        (inp inpfname :direction :input :if-does-not-exist :error)
      (with-open-file
          (out outpfname :direction :output
            :if-exists :supersede :if-does-not-exist :create)
        (format out "<HTML> ~% <BODY> ~% <FONT SIZE=2>~%")
        (format out "<BR> <BR> <BR> <BR>")
        (loop
         do (setf word (read1-word inp))
        ; (format t ">> COLORED-TEXT1 ~a~%" word)
         until (null word)
         do (if (not (white-char-p-s (char word 0)))
              (let ((color (gethash (str-morph word) color-table)))
               ; (format t  ">> COLORED-TEXT2 ~%")
                (if color
                  (format out "<FONT SIZE=4 COLOR= ~a > ~a </FONT>"
                        color (str-lex word))
                  (cond
                   ((author-segment-p word)
                    (read1-word inp)
                    (format out "<BR>    "))
                   (t
                    (format out "~a" (str-lex word))))))
              (if (real-sent-end-p word)
                (progn
                  (incf sentence)
                  (format out "~a" (str-lex word))
                  (when (eq sentence (car seg-bound-l))
                    (format out "<BR> <HR> <BR> <HR>")
                    (pop seg-bound-l)))
                (format out "~a" word))))
         (format out "~a~%~a~%" "</FONT>" "</BODY>" "</HTML>")))))


(defun COLOR-CHAINS (chains-l seg-bound-l inpfname outpfname)
  (let ((color-table (make-hash-table :test #'equalp))
        (colors (list 'red 'blue 'cyan 'violet 'yellow 'gray 'green 'grey
                      'orange 'pink 'skyblue 'purple 'bordo 'navy 'steel 'gold
                      'MistyRose 'RosyBrown 'brown 'goldenrod 'OliveDrab
                      'DarkKhaki 'ForestGreen 'GreenYellow 'LightSeaGreen)))
    ;(format t ">>> COLOR-CHAINS0 ~%") 
    (dolist (i chains-l)
      (setf color-table
            (build-color-table color-table (nth (1- i) *result-chains*)
                               (car colors)))
      (pop colors))
   ; (format t ">>> COLOR-CHAINS1 ~%")
    (colored-text color-table seg-bound-l  inpfname outpfname)))


(defun COLORED-LINK-TEXT (bold-table seg-bound-l inpfname outpfname outdir)
  (let ((sentence 0)
        word)
    (with-open-file
        (inp inpfname :direction :input :if-does-not-exist :error)
      (with-open-file
          (out outpfname :direction :output
            :if-exists :append :if-does-not-exist :create)
       (with-open-file
           (out1 (format nil "~aMeanings.html" outdir)
             :direction :output
             :if-exists :append :if-does-not-exist :create)
        (format out "<HTML> ~% <BODY> ~% <FONT SIZE=2>~%")
        (format out "<BR> <BR> <BR> <BR>")
        (loop
         do (setf word (read1-word inp))
         until (null word)
         do 
         (if (not (white-char-p-s (char word 0)))
              (let ((mean (gethash (str-morph word) bold-table)))
                  (if mean
                    (progn 
                      (format out "<a href = \" ~a\#~a\"> ~a </a>"
                        (format nil "Meanings.html"
                        (subseq inpfname 0 (position #\. inpfname)))
                         (str-lex word) (str-lex word))
                      (format out1 "<a name = \"~a\"> ~a </a> <BR>"
                            (str-lex word) mean))
                   (cond 
                   ((author-segment-p word)
                    (read1-word inp)
                    (format out "<BR>    "))
                   ((real-sent-end-p word)
                    (incf sentence)
                    (when (eq sentence (car seg-bound-l))
                      (format out "<BR> <HR> <BR> <HR>")
                      (pop seg-bound-l)))
                   (t
                    (format out "~a" (str-lex word))))))
              (if (real-sent-end-p word)
                (progn
                  (incf sentence)
                  (format out "~a" (str-lex word))
                  (when (eq sentence (car seg-bound-l))
                    (format out "<BR> <HR> <BR> <HR>")
                    (pop seg-bound-l)))
                (format out "~a" word))))
         (format out "~a~%~a~%" "</FONT>" "</BODY>" "</HTML>")
       (close out1))))))

(defun WN-TO-HTML (sense)
  (let ((str-wn (format nil "~a" sense)))
    (setf str-wn
          (subseq str-wn (position #\space str-wn) (position #\@ str-wn)))
    str-wn))
  
(defun BUILD-LINK-TABLE (link-table ch)
;  (format t ">> BUILD-COLOR-TABLE~%")
  (dolist (entry (chain-entry-list ch))
    (unless (gethash (entry-head-lex entry) link-table)
      (setf (gethash (entry-head-lex entry) link-table)
            (mapcar #'wn-to-html (entry-sense entry)))))
  link-table)


(defun BOLD-CHAINS (chains-l seg-bound-l inpfname outpfname outdir)
  (let ((link-table (make-hash-table :test #'equalp)))
    (dolist (i chains-l)
      (setf link-table
            (build-link-table link-table (nth (1- i) *result-chains*))))
  (colored-link-text link-table seg-bound-l inpfname outpfname outdir)))

;;========================================================================
;;devivation = (sqrt((sum x^2)/(n-1) - aver^2))

(defun CHAIN-SCORE (chain)
  (* (chain-words-number chain)
     (- 1.0 (/ (chain-length chain) (chain-words-number chain)))))

(defun SUM-OF-CHAIN-SQUARES (result-chains)
  (let ((sum 0))
    (dolist (ch result-chains)
      (incf sum (* (chain-score ch) (chain-score ch))))
    sum))

(defun AVERAGE (result-chains)
  (let ((sum 0))
    (dolist (ch result-chains)
      (incf sum (chain-score ch)))
    (/ sum (length result-chains))))

(defun deviv (result-chains)
  (let ((aver (average result-chains)))
   (sqrt
        (- (/ (sum-of-chain-squares result-chains)
           (1- (length result-chains)))
           (* aver aver)))))

(defun STAT-CRITERION (result-chains)
  (let ((aver (average result-chains))) 
    (+ aver
     (/ (deviv result-chains)  2))))

;============================================================================

(defvar *result-chains* nil)

(defun UP-CRITER (crit chain-l)
  (let ((i 0)
        score)
    (loop
      do (setf score (chain-score (nth (incf i) chain-l))) 
    until (< score crit))
    i))
    

(defun GRAPHICAL-REPRESENTATION (chain-l seg-bound-l inpfile outdir)
    (setf *result-chains* chain-l)
    (let ((crit (stat-criterion chain-l)))
      (with-open-file
        (out (concatenate 'string outdir +crit-file+) :direction :output
                           :if-exists :supersede :if-does-not-exist :create)
        (format out "~a" (up-criter crit chain-l)))  
     (initial-page chain-l seg-bound-l crit inpfile outdir)))

(defun number-list (num)
  (let ((res nil)
        (i 1))
    (loop
      do (push i res)
      (setf i (1+ i))
    until (> i num))
    (nreverse res)
   ))
