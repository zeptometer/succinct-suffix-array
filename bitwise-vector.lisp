(defpackage tk.zptm.succinct.bitwise-vector
  (:use :common-lisp))

(in-package tk.zptm.succinct.bitwise-vector)

;;; Word size is 64bit
(deftype word () '(unsigned-byte 64))

(defstruct bitwise-vector
  element-size
  elements-per-word
  array)

(defun create-bitwise-vector (element-size length)
  (assert (< element-size 64))
  (let* ((elements-per-word (floor 64 element-size))
	 (array-size (floor length elements-per-word)))
      (make-bitwise-vector :element-size element-size
			   :elements-per-word elements-per-word
			   :array (make-array array-size
					      :element-type 'word
					      :initial-element 0))))

(defun bvref (bv n)
  (multiple-value-bind (div rem) (floor n (bitwise-vector-elements-per-word bv))
    (ldb (byte (bitwise-vector-element-size bv)
	       (* (bitwise-vector-element-size bv) rem))
	 (aref (bitwise-vector-array bv) div))))

(defsetf bvref (bv n) (bits)
  (let ((m (gensym))
	(div (gensym))
	(rem (gensym)))
    `(let ((,m ,n))
       (multiple-value-bind (,div ,rem) (floor ,m (bitwise-vector-elements-per-word ,bv))
	 (setf (ldb (byte (bitwise-vector-element-size ,bv)
			  (* (bitwise-vector-element-size bv) rem))
		    (aref (bitwise-vector-array ,bv) ,div))
	       ,bits)))))
