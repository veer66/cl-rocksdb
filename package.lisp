;;;; package.lisp

(defpackage #:cl-rocksdb
  (:use #:cl #:cffi)
  (:export #:create-options
	   #:destroy-options
	   #:increase-parallelism))
