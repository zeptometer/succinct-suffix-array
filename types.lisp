(defpackage tk.zptm.succinct.types
  (:use :common-lisp)
  (:export word))

(in-package tk.zptm.succinct.types)

;;; Word size is 64bit
(deftype word () '(unsigned-byte 64))

