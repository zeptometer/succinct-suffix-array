(defpackage succinct.rankselect.rank
  (:use :common-lisp
	:succinct.common
	:succinct.bits
	:succinct.bitwise-vector)
  (:export :create-rank-index
	   :query-rank-index
	   :rank-index-size))

(in-package :succinct.rankselect.rank)

;;; auxilary functions for bits and integers
(defun lognth (n idx)
  (logand 1 (ash n (- idx))))

(labels ((log-length (n) (* 2 (ceiling (/ (log n 2) 2)))))

  (defun largeblock-size (n)
    (expt (log-length n) 2))

  (defun smallblock-size (n)
    (/ (log-length n) 2)))


;;;; Rank Index
(defstruct rank-index
  r1-size r2-size
  r1 r2 r3)

(defun rank-index-size (index)
  (+ (length (rank-index-r1 index))
     (length (rank-index-r2 index))
     (length (rank-index-r3 index))))

;;; Rank Index Constructor
;; R1 table
(defun count-ones-in-block (bits block-idx block-size)
  (loop
     for j from 0 below block-size
     for idx = (+ (* block-idx block-size) j)
     while (< idx (bits-length bits))
     sum (bref bits (+ (* block-idx block-size) j))))

(defun make-r1-table (bits)
  (let* ((len        (bits-length bits))
	 (block-size (largeblock-size len))
	 (table-size (ceiling len block-size))
	 (elm-size   (lb len))
	 (table      (create-bitwise-vector elm-size table-size)))
    (loop
       for i from 0 below table-size
       for n-ones = (count-ones-in-block bits i block-size)
       sum n-ones into rank
       do (setf (bwvref table i) (- rank n-ones)))
    table))

;; R2 table
(defun make-r2-table (bits)
  (let* ((len                 (bits-length bits))
	 (block-size          (smallblock-size len))
	 (blocks-per-r1-block (/ (largeblock-size len)
				 (smallblock-size len)))
	 (table-size          (ceiling len block-size))
	 (elm-size            (lb (largeblock-size len)))
	 (table               (create-bitwise-vector elm-size table-size)))
    (loop
       for i from 0 below table-size
       for n-ones = (count-ones-in-block bits i block-size)
       for rank = (if (zerop (mod i blocks-per-r1-block))
		      n-ones
		      (+ rank n-ones))
       do (setf (bwvref table i) rank))
    table))

;; r3 table
(defun popcount (int)
  (loop
     for i = int then (ash i -1)
     while (> i 0)
     sum (logand i 1)))

(defun make-r3-table (bits)
  (let* ((len          (bits-length bits))
	 (pattern-size (smallblock-size len))
	 (table-size   (expt 2 pattern-size))
	 (elm-size     (lb pattern-size))
	 (table        (create-bitwise-vector elm-size table-size)))
    (loop
       for i from 0 below table-size
       do (setf (bwvref table i) (popcount i)))
    table))

;; toplevel constructor
(defun create-rank-index (bits)
  (make-rank-index :r1-size (largeblock-size (bits-length bits))
		   :r2-size (smallblock-size (bits-length bits))
		   :r1 (make-r1-table bits)
		   :r2 (make-r2-table bits)
		   :r3 (make-r3-table bits)))

;;; Rank Index Accessor
(defun query-rank-index (bits index n)
  (let* ((r1idx   (floor n (rank-index-r1-size index)))
	 (r2idx   (floor n (rank-index-r2-size index)))
	 (rem     (mod n (rank-index-r2-size index)))
	 (r1-rank (bwvref (rank-index-r1 index) r1idx))
	 (r2-rank (bwvref (rank-index-r2 index) r2idx))
	 (r3-rank (bwvref (rank-index-r3 index) (extract-bits bits rem (- n rem)))))
    (+ r1-rank r2-rank r3-rank)))
