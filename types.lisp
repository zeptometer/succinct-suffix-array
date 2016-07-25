(defpackage succinct.types
  (:use :common-lisp)
  (:export word))

(in-package succinct.types)

;;; Word size is 64bit
(deftype word () '(unsigned-byte 64))

