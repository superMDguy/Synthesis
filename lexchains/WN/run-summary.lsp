(in-package :COMMON-LISP-USER)

(defvar *segment-l* nil)

(defun CHOOSE-SUMMARY ()
  (let (sum-type)
    (format t "~@
               ~@
               Types of summaries~@
               1. FIRST-APPEARENCE-CHAIN-SUMMARY~@
               2. FIRST-IMPORTANT-APPEARENCE-CHAIN-SUMMARY~@
               3. BUILD-PEAK-SUMMARY~@
               ~@
               Choose number of desired type ")
    (setf sum-type (read))
  sum-type))

(defun CHOOSE-IMPORTANT-CHAINS (fin-ch)
  (format t "Chain list~%~%")
  (let ((i 0))
    (dolist (ch fin-ch)
      (when (> (chain-words-number ch) 3)
       (format t "~a " (incf i))
       (print-head-chain ch))))
  (format t "~%Choose the list of important chains,for example (1 2 3).~%")
  (read))

(defun AUTOM-CHOOSEN-IMPORTANT-CHAINS (crit)
  (let ((num-res-ch '())
        (i 1))
    (loop
      do (push i num-res-ch)
      until (>= i crit)
      do (incf i))
    (nreverse num-res-ch)))

(defun BUILD-AUT-SUMMARY (fname outdir fin-ch seg-bound-l crit)
  (build-first-important-appearence-chain-summary
                             fin-ch seg-bound-l
                             (autom-choosen-important-chains crit)
                             fname outdir "Summary.aut"))

(defun BUILD-SUMMARY (fname outdir fin-ch seg-bound-l)
  (let ((sum-type (choose-summary))
        (num-result-chains (choose-important-chains fin-ch)))
  (cond
    ((eq sum-type 1)
     (build-first-appearence-chain-summary
                             fin-ch seg-bound-l  num-result-chains
                             fname outdir))
    ((eq sum-type 2)
     (build-first-important-appearence-chain-summary
                             fin-ch seg-bound-l num-result-chains
                             fname outdir))
    ((eq sum-type 3)
     (build-peak-summary
            fin-ch num-result-chains seg-bound-l fname outdir)))))
      
(defun RUN-SUMMARY (fname &optional (aut '()))
  (setf *segment-l* nil)
  (let* ((outdir    (concatenate 'string +work-directory+ fname +dir-ext+))
        (chfname    (concatenate 'string outdir +chain-file+))
        (boundfname (concatenate 'string outdir +bound-file+))
        (segfname   (concatenate 'string outdir +segment-file+))
        (critfname  (concatenate 'string outdir +crit-file+))
        seg-bound-l fin-ch crit)
    (unless (probe-file chfname)
      (run fname))
    (setf fin-ch (init-chains chfname))
    (setf seg-bound-l (init-bound-l boundfname))
    (setf *segment-l* (init-segment-l segfname))
    (setf crit (init-crit critfname))
    (if aut
      (build-aut-summary fname outdir fin-ch seg-bound-l crit)
      (build-summary fname outdir fin-ch seg-bound-l))))
