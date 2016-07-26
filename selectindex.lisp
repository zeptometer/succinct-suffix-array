(defpackage succinct.rankselect.select
  (:use :common-lisp
	:succinct.common
	:succinct.bits
	:succinct.bitwise-vector)
  (:export :create-select-index
	   :query-select-index))

(in-package succinct.rankselect.select)

;;; parameter
(defparameter border-const 3)

;;; data structure
(defstruct large-block1
  tree
  small-blocks)

(defstruct large-block
  type
  block)

(defstruct select-index
  blocks)


;;; construct select index

;; large block with many zeros
(defun create-large-block0 (bits from to)
  (declare (ignore bits from to))
  "FIXME")

;; large block with less zeros
(defun create-large-block1 (bits from to)
  (declare (ignore bits from to))
  "FIXME")

;; construct large block
(defun create-large-block (bits from to)
  (let ((border (expt border-const (lb (bits-length bits)))))
    (if (> (- to from -1) border)
	(create-large-block0 bits from to)
	(create-large-block1 bits from to))))

;; whole vector
(defun count-ones (bits)
  (loop
     for i from 0 below (bits-length bits)
     sum (bref bits i)))

(defun ones-per-large-block (len)
  (let ((a (ceiling (log len)))) (* a a)))

(defun create-select-index (bits)
  (let* ((len            (bits-length bits))
	 (ones-per-block (ones-per-large-block len))
	 (n-blocks       (ceiling (count-ones bits) ones-per-block))
	 (blocks         (make-array n-blocks)))
    (loop
       for block_idx from 0 below n-blocks
       with bits_idx = 0
       do (loop
	     for j = (1+ bits_idx)
	     sum (bref bits j) into n-ones
	     while (and (< n-ones ones-per-block) (< j len))
	     finally (setf (aref blocks block_idx)
			   (create-large-block bits (1+ bits_idx) j)
			   bits_idx
			   j)))
    (make-select-index :blocks blocks)))
