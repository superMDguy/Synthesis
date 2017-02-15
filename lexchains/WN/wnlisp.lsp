
(in-package :cl-user)


(defconstant +X-FILES+
  '(
    ("packages"                . :common-lisp-user)
    ("parts-of-speech"         . :wordnet)
    ("wordnet-database-files"  . :wordnet)
    ("parse-wordnet-data"      . :wordnet)
    ("representation"          . :wordnet)
    ("relationship-algorithms" . :wordnet)
    ("examples"                . :common-lisp-user)
    ("init"                    . :common-lisp-user)
    ("init-meaning-table"      . :common-lisp-user)
    ("rule"                    . :common-lisp-user)
    ("read-input"              . :common-lisp-user)
    ("new"                     . :common-lisp-user)
    ("stat-table"              . :common-lisp-user)
    ("ch"                      . :common-lisp-user)
    ("keep-results"            . :common-lisp-user)
    ("init-results"            . :common-lisp-user)
    ("run-summary"             . :common-lisp-user)
    ("summary"                 . :common-lisp-user)
    ("visualization"           . :common-lisp-user)
    ("run"                     . :common-lisp-user)
    ))

(defconstant +LISP-EXTENSION+ "lsp")
(defconstant +FASL-EXTENSION+ "fasl")

(defun LISP-FILENAME (fname)
  (concatenate 'string fname "." +lisp-extension+))
(defun FASL-FILENAME (fname)
  (concatenate 'string fname "." +fasl-extension+))

(defvar *LOADED-X-FILES* nil)

(defun LOAD-X-FILE (filename &optional (verbose nil))
  (let* ((fname (pathname-name filename))
         (lisp-filename (lisp-filename fname))
         (fasl-filename (fasl-filename fname))
         (lisp-file-t   (and (probe-file lisp-filename)
                             (file-write-date lisp-filename)))
         (fasl-file-t   (and (probe-file fasl-filename)
                             (file-write-date fasl-filename)))
         (best (cond ((and (not lisp-file-t) (not fasl-file-t))
                      nil)
                     ((not lisp-file-t)
                      (format t "~&Warning: No source for '~a'~%"
                              fasl-filename)
                      (cons fasl-filename fasl-file-t))
                     ((not fasl-file-t)
                      (cons lisp-filename lisp-file-t))
                     ((< lisp-file-t fasl-file-t)
                      (cons fasl-filename fasl-file-t))
                     (t
                      (cons lisp-filename lisp-file-t))))
         (loaded (assoc fname *loaded-x-files* :test #'equalp))
         (package (string (rest (assoc fname +x-files+ :test #'equalp)))))
    (if best
      (if (or (not loaded) (not (= (rest loaded) (rest best))))
        (progn
          (format t "~&Loading '~a' (~a package)...~:[ ~;~%~]"
                  (first best)
                  (rest (assoc fname +x-files+ :test #'equalp))
                  verbose)
          (load (first best) :verbose verbose)
          (format t "~:[~;~%~]Done.~%" verbose)
          (if loaded
            (setf (rest loaded) (rest best))
            (progn
              (push (cons fname (rest best)) *loaded-x-files*)
              (unless (eq (find-package package) (find-package "CL-USER"))
                (use-package (find-package package)
                             (find-package "CL-USER"))))))
        (when verbose
          (format t "~&File '~a' already loaded.~%" (first best))))
      (error "Could not find file '~a' in any form." fname))))

(defun LOAD-X-FILES (&optional (verbose nil))
  (dolist (f (mapcar #'first +x-files+))
    (load-x-file f verbose))
  (format t "~&Done loading all files.~%"))

(defun COMPILE-X-FILES (&optional (verbose t))
  (load-x-files nil)
  (dolist (f (mapcar #'first +x-files+))
    (when (and (probe-file (lisp-filename f))
               (or (not (probe-file (fasl-filename f)))
                   (<= (file-write-date (fasl-filename f))
                       (file-write-date (lisp-filename f)))))
      (format t "~&Compiling ~:@(~a.~a~)...~%" f +lisp-extension+)
      (compile-file (lisp-filename f)
                    :output-file (fasl-filename f)
                    :verbose verbose :print nil)
      (load (fasl-filename f) :verbose nil)
      (setf (rest (assoc f *loaded-x-files* :test #'equalp))
            (file-write-date (fasl-filename f)))
      (format t "~&Done.~%")))
  (when verbose
    (format t "~&Done compiling all files.~%")))

(defun DOALL ()
  (compile-x-files))

