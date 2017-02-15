(in-package :COMMON-LISP-USER)

(defun BUILD-STAT (fname)
  (let ((noun-table (make-hash-table :test #'equalp))
        (verb-table (make-hash-table :test #'equalp))
        word noun-l verb-l)
  (with-open-file
        (inp fname :direction :input :if-does-not-exist :error)
    (loop
       do (setf word (read-word inp))
       until (null word)
       do 
         (cond
           ((and (noun-p word) (gethash (str-morph word) noun-table))
            (incf (gethash (str-morph word) noun-table)))
           ((noun-p word)
            (setf (gethash (str-morph word) noun-table) 1))
           ((and (verb-p word) (gethash (str-morph word) verb-table))
            (incf (gethash (str-morph word) verb-table)))
           ((verb-p word)
            (setf (gethash (str-morph word) verb-table) 1)))))
    (with-open-file
      (out (format nil "ch_~a/noun-verb-statis"
                  (subseq fname 0 (position #\. fname)))
            :direction :output
           :if-exists :supersede :if-does-not-exist :create)
    (maphash #'(lambda (key val)
                   (push (list key val) noun-l))
               noun-table)
      (setf noun-l (sort noun-l #'> :key #'second))
      (format out "~%NOUN TABLE~%")
      (dolist (i noun-l)
        (when (> (second i) 1)
          (format out "~a      ~a~%" (first i) (second i))))
    (maphash #'(lambda (key val)
                   (push (list key val) verb-l))
               verb-table)
      (format out "~%VERB TABLE~%")
      (setf verb-l (sort verb-l #'> :key #'second))
      (dolist (i verb-l)
        (when (> (second i) 0)
          (format out "~a      ~a~%" (first i) (second i)))))))