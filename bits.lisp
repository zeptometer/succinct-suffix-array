(defpackage succinct.bits
  (:use :common-lisp
	:succinct.common)
  (:export create-bits
	   bref
	   bits-length
	   extract-bits))

(in-package succinct.bits)

(defstruct bits
  length
  array)

(defun create-bits (length)
  (make-bits :length length
	     :array (make-array (/ length 64)
				:element-type 'word)))

(defun bref (bits idx)
  (multiple-value-bind (div rem) (floor idx 64)
    (ldb (byte 1 rem) (aref (bits-array bits) div))))

(defsetf bref (bits idx) (new)
  `(multiple-value-bind (div rem) (floor ,idx 64)
     (setf (ldb (byte 1 rem) (aref (bits-array ,bits) div)) ,new)))

(defun extract-bits (bits size position)
  (assert (<= 1 size 64))
  (assert (< -1 position (+ size position) (bits-length bits)))
  (if (= (floor position 64)
	 (floor (+ size position -1) 64))
      (multiple-value-bind (div rem) (floor position 64)
	(ldb (byte size rem) (aref (bits-array bits) div)))
      (multiple-value-bind (div rem) (floor position 64)
	(+ (ldb (byte (- 64 rem) rem) (aref (bits-array bits) div))
	   (ash (ldb (byte (+ rem size -64) 0) (aref (bits-array bits) (1+ div)))
		(- 64 rem))))))
