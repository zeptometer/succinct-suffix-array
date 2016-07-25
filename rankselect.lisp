(defpackage succinct.rankselect
  (:use :succinct.rankselect.rank
	:succinct.rankselect.select
	:common-lisp))

(in-package succinct.rankselect)


;;; auxilary functions for bits and integers
(defun lognth (n idx)
  (logand 1 (ash n (- idx))))

(defun log-length (n)
  "force log(n) to be even number: helper function for make-r*-table"
  (* 2 (ceiling (/ (log n 2) 2))))

;;;; Rank Index
(defstruct rank-index
  r1-size r1
  r2-size r2
  r3)


;;; Rank Index Constructor
(defun create-rank-index (bits)
  (let* ((len (length bits))
	 (loglen (log-length len)))
    (make-rank-index :r1-size (* loglen loglen)
		     :r2-size (/ loglen 2)
		     :r1 (make-r1-table bits)
		     :r2 (make-r2-table bits)
		     :r3 (make-r3-table bits))))

;; R1 table
(defun make-r1-table (bits)
  (let* ((len (length bits))
	 (loglen (log-length len))
	 (block-size (* loglen loglen))
	 (table-size (ceiling (/ len block-size)))
	 (table (make-array table-size)))
    (loop
       for i from 0 below table-size
       for n-ones = (loop
		       for j from 0 below block-size
		       for idx = (+ (* i block-size) j)
		       while (< idx len)
		       sum (aref bits (+ (* i block-size) j)))
       for rank = n-ones then (+ rank n-ones)
       do (setf (aref table i) rank))
    table))

;; R2 table
(defun make-r2-table (bits)
  (let* ((len (length bits))
	 (loglen (log-length len))
	 (block-size (/ loglen 2))
	 (blocks-per-r1-block (* 2 loglen))
	 (table-size (ceiling (/ len block-size)))
	 (table (make-array table-size)))
    (loop
       for i from 0 below table-size
       for n-ones = (loop
		       for j from 0 below block-size
		       for idx = (+ (* i block-size) j)
		       while (< idx len)
		       sum (aref bits (+ (* i block-size) j)))
       for rank = (if (zerop (mod i blocks-per-r1-block))
		      n-ones
		      (+ rank n-ones))
       do (setf (aref table i) rank))
    table))

;; r3 table
(defun popcount (int)
  (loop
     for i = int then (ash i -1)
     while (> i 0)
     sum (logand i 1)))

(defun make-r3-table (bits)
  (let* ((len (length bits))
	 (loglen (log-length len))
	 (pattern-size (/ loglen 2))
	 (table-size (expt 2 pattern-size))
	 (table (make-array table-size)))
    (loop
       for i from 0 below table-size
       do (setf (aref table i) (popcount i)))
    table))

;;; Rank Index Accessor

;; I want this be constant-time function, but that is impossible
;; because Common Lisp does not support it.
(defun extract (bits n m)
  (loop
     for i from n below m
     for base = 1 then (* 2 base)
     sum (* base (bit bits i))))

(defun rank-with-index (bits index n)
  (let* ((r1idx (floor (/ n (rank-index-r1-size index))))
	 (r2idx (floor (/ n (rank-index-r2-size index))))
	 (rem   (mod n (rank-index-r2-size index)))
	 (r1-rank (if (zerop r1idx)
		      0
		      (aref (rank-index-r1 index) (1- r1idx))))
	 (r2-rank (if (zerop r2idx)
		      0
		      (aref (rank-index-r2 index) (1- r2idx))))
	 (r3-rank (aref (rank-index-r3 index) (extract bits (- n rem) n))))
    (+ r1-rank r2-rank r3-rank)))


;;; selsect index
(defstruct select-index)

(defun create-select-index (bits)
  (make-select-index))


;;; main definitions
(defstruct rankselect
  bits
  rank-index
  select-index)

(defun create-rankselect (bits)
  (make-rankselect :bits bits
		   :rank-index (create-rank-index bits)
		   :select-index (create-select-index bits)))
