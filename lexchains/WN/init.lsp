(defvar *SEGMENT-L* nil)
(defvar *MY-COMMONALITY-TABLE* nil)
(defvar *NUMBER-HYPONYM-TABLE* nil)
(defvar *MY-CONNECTION-TABLE* (make-hash-table :test #'equalp))
(defvar *MY-MEANING-TABLE* (make-hash-table :test #'equalp))
(defvar *final-chains* nil
  "The union of list of result-chains from different segments.")


(defun INIT (fname)
  (setf *segment-l* nil)
  (setf *final-chains* nil)
  (setf *my-commonality-table* (make-hash-table :test #'eq))
  (setf *my-meaning-table* (build-meaning-table fname))
  (setf *my-connection-table* (build-connection-table fname))
  (setf *number-hyponym-table* (make-hash-table :test #'equalp)))

