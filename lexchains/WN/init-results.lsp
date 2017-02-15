(in-package :COMMON-LISP-USER)

(defun INIT-BOUND-L (fname)
  (with-open-file 
    (inp fname :direction :input :if-does-not-exist :error)
      (read inp nil)))

(defun INIT-ENTRY (inp)
  (let ((lex (read inp nil)))
    (if (and lex (not (char= (char lex 0) #\&)))
       (make-entry-in-chain :sent (read inp nil)
                            :head-lex lex)
        nil)))
  
(defun INIT-CHAIN (inp)
  (let (entry-l entry)
    (loop
       do (setf entry (init-entry inp))
       until (null entry)
       do (push entry entry-l))
    (if entry-l
      (make-lexical-chain :entry-list entry-l)
      nil)))

(defun INIT-CHAINS (fname)
  (let (chain-l ch)
    (with-open-file 
            (inp fname :direction :input :if-does-not-exist :error)
      (loop
        do (setf ch (init-chain inp))
        until (null ch)
        do (push ch chain-l)))
    (nreverse chain-l)))


(defun INIT-SEGMENT (inp)
  (let ((r-bound (read inp nil)))
    (if r-bound
      (make-segment-entry :right-bound r-bound
                          :left-bound (read inp nil)
                          :noun-num (read inp nil))
      nil)))

(defun INIT-SEGMENT-L (fname)
  (let (segment-l seg)
    (with-open-file 
            (inp fname :direction :input :if-does-not-exist :error)
      (loop
        do (setf seg (init-segment inp))
        until (null seg)
        do (push seg segment-l)))
    (nreverse segment-l)))

(defun INIT-CRIT (fname)
  (with-open-file 
    (inp fname :direction :input :if-does-not-exist :error)
      (read inp nil)))

