(defpackage succinct.common
  (:use :common-lisp)
  (:export word
           lb))

(in-package succinct.common)

;;; Word size is 64bit
(deftype word () '(unsigned-byte 64))

(defun lb (n)
  (ceiling (log n 2)))
