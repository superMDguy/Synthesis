(in-package :COMMON-LISP-USER)


(defun build-connection-table (fname)
  (when (probe-file (format nil "~a_connect" fname))
    (let (word1 word2 connect)
      (with-open-file
           (inp (format nil "~a_connect" fname) :direction
                :input :if-does-not-exist :error)
        (loop
         do (setf word1   (read-word inp))
            (setf word2   (read-word inp))
            (setf connect (read-word inp))
         until (null word1)
         do (setf (gethash (cons word1 word2) *my-connection-table*)
             connect))))
    (format t "!! Table connect~a~%" *my-connection-table*))
    *my-connection-table*)

(defun build-meaning-table (fname)
  (when (probe-file (format nil "~a_means" fname))
    (let (word1 word2  meaning-num)
      (with-open-file
           (inp (format nil "~a_means" fname) :direction
                :input :if-does-not-exist :error)
        (loop
         do (setf word1   (read-word inp))
            (setf word2   (read-word inp))
            (setf meaning-num (read-word inp))
         until (null word1)
         do (format t ">>build-meaning-table ~a ~a ~a~%" word1 word2 meaning-num)
           (setf (gethash word1 *my-meaning-table*)
                  (list (nth (digit-char-p (char meaning-num 0))
                             (wn:index-entry-synsets
			             (wn:cached-index-lookup word2 :noun))))))))
    (format t "!! Table meaning~a~%" *my-meaning-table*))
    *my-meaning-table*)
