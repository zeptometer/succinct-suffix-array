(defpackage succinct.bitwise-vector
  (:use :common-lisp
	:succinct.types)
  (:export create-bitwise-vector
	   bwvref))

(in-package succinct.bitwise-vector)

(defstruct bitwise-vector
  element-size
  elements-per-word
  length
  array)

(defun create-bitwise-vector (element-size length)
  (assert (< element-size 64))
  (let* ((elements-per-word (floor 64 element-size))
	 (array-size (floor length elements-per-word)))
      (make-bitwise-vector :element-size element-size
			   :elements-per-word elements-per-word
			   :length length
			   :array (make-array array-size
					      :element-type 'word
					      :initial-element 0))))

(defun bwvref (bv n)
  (assert (< -1 n (bitwise-vector-length bv)))
  (multiple-value-bind (div rem) (floor n (bitwise-vector-elements-per-word bv))
    (ldb (byte (bitwise-vector-element-size bv)
	       (* (bitwise-vector-element-size bv) rem))
	 (aref (bitwise-vector-array bv) div))))

(defsetf bwvref (bv n) (bits)
  `(multiple-value-bind (div rem) (floor ,n (bitwise-vector-elements-per-word ,bv))
     (setf (ldb (byte (bitwise-vector-element-size ,bv)
		      (* (bitwise-vector-element-size ,bv) rem))
		(aref (bitwise-vector-array ,bv) div))
	   ,bits)))
