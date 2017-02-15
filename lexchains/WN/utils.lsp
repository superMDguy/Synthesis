(defun GET-X-DISPLAY ()
  (with-open-file (x "~/.X11" :direction :input)
    (read-line x)))

(defun RUN-X-COMMAND (command)
  (let* ((spc (position #\@ command))
         (str (if spc
                (concatenate 'string
                             (subseq command 0 spc)
                             " -display "
                             (get-x-display)
                             " "
                             (subseq command (1+ spc)))
                command)))
    (run-shell-command str)))
