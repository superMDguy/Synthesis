(in-package :COMMON-LISP-USER)


(defun DIVIDED-CHAR-P (char)
  (member char '(#\space #\newline #\Linefeed #\tab)))

;; This will read and retun a words is the input stream, and if print? is
;; true then the input file will be echoed to the screen .
(defun READ-WORD (s &optional print?)
  (let (c word)
    (setf word
          (with-output-to-string (word)
            (loop do (setf c (read-char s nil nil))
                     (when (and print? c) (write-char c t))
                  while (divided-char-p c))
            (when (null c)
              (return-from read-word nil))
            (loop do (write-char c word)
                     (setf c (read-char s nil nil))
                     (when (and print? c) (write-char c t))
                  until (or (null c) (divided-char-p c)))))
    ;(format t "~a~%" word)
    (nstring-downcase word)))
