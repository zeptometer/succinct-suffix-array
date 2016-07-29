(defpackage succinct.common
  (:use :common-lisp)
  (:export word
           lb
	   pow))

(in-package succinct.common)

;;; Word size is 64bit
(deftype word () '(unsigned-byte 64))

(defun lb (n)
  (max 1 (ceiling (log n 2))))

(defun pow (base power)
  (cond ((zerop power) 1)
	((evenp power) (let ((a (pow base (floor power 2)))) (* a a)))
	((oddp  power) (let ((a (pow base (floor power 2)))) (* base a a)))))
