(defpackage succinct.rankselect
  (:use :common-lisp
	:succinct.rankselect.rank
	:succinct.rankselect.select))

(in-package succinct.rankselect)

;;; main definitions
(defstruct rankselect
  bits
  rank-index
  select-index)

(defun create-rankselect (bits)
  (make-rankselect :bits bits
		   :rank-index (create-rank-index bits)
		   :select-index (create-select-index bits)))

(defun rank (rankselect n)
  (query-rank-index (rankselect-bits rankselect)
		    (rankselect-rank-index rankselect)
		    n))

(defun select (rankselect n)
  (query-select-index (rankselect-bits rankselect)
		      (rankselect-select-index rankselect)
		      n))
