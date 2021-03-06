(defpackage succinct.rankselect.select
  (:use :common-lisp
	:succinct.common
	:succinct.bits
	:succinct.bitwise-vector)
  (:export :create-select-index
	   :query-select-index
	   :select-index-size))

(in-package succinct.rankselect.select)

;;; parameter
(defparameter border-const 3)

;;; data structure
(defstruct large-block0
  table)

(defun large-block0-size (block)
  (word-length (large-block0-table block)))

(defstruct large-block1
  ;;; point where this block starts
  offset
  ;;; small blocks
  small-block-size
  small-blocks
  small-offsets
  ;;; tree
  n-branch
  tree
  tree-depth)

(defun large-block1-size (block)
  (+ (word-length (large-block1-small-blocks block))
     (word-length (large-block1-small-offsets block))
     (word-length (large-block1-tree block))))

(defstruct select-index
  ones-per-block
  blocks)

(defun select-index-size (index)
  (loop
     for block across (select-index-blocks index)
     sum (etypecase block
	   (large-block0 (large-block0-size block))
	   (large-block1 (large-block1-size block)))))

;;; construct select index
(defun ones-per-large-block (len)
  (let ((a (ceiling (lb len)))) (* a a)))

(defun small-block-size (len)
  (ceiling (lb len) 2))

;; large block with many zeros
(defun create-large-block0 (bits from to)
  (let* ((len            (bits-length bits))
	 (ones-per-block (ones-per-large-block len))
	 (elm-size       (lb len))
	 (table          (create-bitwise-vector elm-size ones-per-block)))
    (loop
       with n_ones = 0
       for i from from to to
       when (= 1 (bref bits i))
       do (setf (bref table n_ones) i
		n_ones (1+ n_ones)))
    (make-large-block0 :table table)))

(defun select-large-block0 (block n)
  (bwvref (large-block0-table block) n))

;; large block with less zeros
(defun get-n-branch (len)
  (ceiling (sqrt (lb len))))

(defun popcount-range (bits from to)
  (loop
     for i from from to to
     sum (bref bits i)))

(defun create-tree (bits from to smallblock-size n-branch)
  (let* ((largeblock-size (- to from -1))
	 (depth    (1+ (ceiling (log (ceiling largeblock-size smallblock-size) n-branch))))
	 (n-nodes  (floor (1- (pow n-branch depth)) (1- n-branch)))
	 (elm-size (lb largeblock-size))
	 (tree     (create-bitwise-vector elm-size n-nodes)))
    (loop
       for level from 0 below depth
       do (loop
	     with offset = (floor (1- (pow n-branch level)) (1- n-branch))
	     with width = (* smallblock-size (pow n-branch (- depth level 1)))
	     for i from 0 below (pow n-branch level)
	     do (setf (bwvref tree (+ i offset))
		      (popcount-range bits
				      (min to (+ from (* width i)))
				      (min to (+ from (* width (1+ i)) -1))))))
    (values depth tree)))

(defun create-small-blocks (bits from to size)
  (let* ((blocks-size      (popcount-range bits from to))
	 (blocks-elm-size  (lb size))
	 (blocks           (create-bitwise-vector blocks-elm-size blocks-size))
	 (offsets-size     (ceiling (- to from -1) size))
	 (offsets-elm-size (lb blocks-size))
	 (offsets          (create-bitwise-vector offsets-elm-size offsets-size)))
    (loop
       with offset = 0
       for block-idx from 0 below offsets-size
       do (setf (bwvref offsets block-idx) offset)
       do (loop
	     for i from 0 below size
	     for bit-idx = (+ from (* size block-idx) i)
	     while (<= bit-idx to)
	     for bit = (bref bits bit-idx)
	     sum bit into n-ones
	     when (= 1 bit)
	     do (setf (bwvref blocks (+ offset n-ones -1)) i)
	     finally (incf offset n-ones)))
    (values offsets blocks)))

(defun create-large-block1 (bits from to)
  (let* ((len  (bits-length bits))
	 (size (small-block-size len))
	 (n-branch (get-n-branch len)))
    (multiple-value-bind (depth tree) (create-tree bits from to size n-branch)
      (multiple-value-bind (small-offsets small-blocks) (create-small-blocks bits from to size)
	(make-large-block1 :offset from
			   :small-block-size size
			   :small-offsets small-offsets
			   :small-blocks small-blocks
			   :n-branch n-branch
			   :tree-depth depth
			   :tree tree)))))

(defun traverse-tree (tree depth n-branch n)
  (labels ((traverse-tree% (tree level parent offset depth n-branch n)
	     (if (= level depth)
		 (values parent n)
		 (loop
		    for i from offset
		    for r = (bwvref tree i)
		    for rs = 0 then (+ r rs)
		    when (< rs n (+ r rs 1))
		    return (traverse-tree% tree (1+ level)
					   (+ offset i)
					   (1+ (* (+ offset i) n-branch))
					   depth n-branch n)))))
    (traverse-tree% tree 0 0 1 depth n-branch n)))

(defun select-large-block1 (block n)
  (multiple-value-bind (idx rem) (traverse-tree (large-block1-tree block)
						(large-block1-tree-depth block)
						(large-block1-n-branch block)
						n)
    (+ (large-block1-offset block)
       (* idx (large-block1-small-block-size block))
       (bwvref (large-block1-small-blocks block)
	       (+ (bwvref (large-block1-small-offsets block) idx) rem)))))

;;; large block
(defun create-large-block (bits from to)
  (let ((border (pow border-const (lb (bits-length bits)))))
    (if (> (- to from -1) border)
	(create-large-block0 bits from to) 
	(create-large-block1 bits from to))))

(defun select-large-block (block n)
  (etypecase block
    (large-block0 (select-large-block0 block n))
    (large-block1 (select-large-block1 block n))))

;;; whole index
(defun count-ones (bits)
  (loop
     for i from 0 below (bits-length bits)
     sum (bref bits i)))

(defun create-select-index (bits)
  (let* ((len            (bits-length bits))
	 (ones-per-block (ones-per-large-block len))
	 (n-blocks       (ceiling (count-ones bits) ones-per-block))
	 (blocks         (make-array n-blocks)))
    (loop
       for block_idx from 0 below n-blocks
       with bits_idx = 0
       do (loop
	     for j from bits_idx below len
	     sum (bref bits j) into n-ones
	     while (< n-ones ones-per-block)
	     finally (setf (aref blocks block_idx)
			   (create-large-block bits bits_idx (min j (1- len)))
			   bits_idx
			   (1+ j))))
    (make-select-index :ones-per-block ones-per-block
		       :blocks blocks)))

(defun query-select-index (bits index n)
  (declare (ignorable bits))
  (let* ((ones-per-block (select-index-ones-per-block index))
	 (div (floor n ones-per-block))
	 (rem (mod n ones-per-block))
	 (block (aref (select-index-blocks index) div)))
    (select-large-block block rem)))
