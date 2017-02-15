(in-package :COMMON-LISP-USER)

(defun WRITE-ENTRY (entry out)
  (format out "\"~a\" ~a" (entry-head-lex entry) (entry-sent entry)))

(defun WRITE-CHAIN (chain out)
  (dolist (entry (chain-entry-list chain))
    (write-entry entry out))
  (format out "\"&\""))
   
(defun KEEP-CHAINS (outfname ch-l)
  (with-open-file
        (out outfname :direction :output
                      :if-exists :supersede :if-does-not-exist :create)
    (mapcar #'(lambda(x) (write-chain x out)) ch-l)))

(defun KEEP-BOUND-L (outfname bound-l)
  (with-open-file
      (out outfname :direction :output
                    :if-exists :supersede :if-does-not-exist :create)
    (format out "~a" bound-l)))


(defun WRITE-SEGMENT (out segment)
  (format out "~a ~a ~a " (seg-right-bound segment)
                          (seg-left-bound segment)
                          (seg-noun-num segment)))


(defun KEEP-SEGMENT-L (outfname segment-l)
  (with-open-file
        (out outfname :direction :output
                      :if-exists :supersede :if-does-not-exist :create)
    (mapcar #'(lambda(x) (write-segment out x)) segment-l)))
