(defpackage succinct.rankselect
  (:use :succinct.rankselect.rank
	:succinct.rankselect.select
	:common-lisp))

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

(defun rank (rankselect)
  (rank-with-index (rankselect-bits rankselect)
		   (rankselect-rank-index rankselect)))

(defun select (rankselect)
  (select-with-index (rankselect-bits rankselect)
		     (rankkselect-select-index rankselect)))
