;;;; package.lisp

(defpackage #:cl-rocksdb
  (:use #:cl #:cffi)
  (:export #:create-options
	   #:destroy-options
	   #:increase-parallelism
	   #:optimize-level-style-compaction
	   #:set-create-if-missing
	   #:create-writeoptions
	   #:destroy-writeoptions
	   #:create-readoptions
	   #:destroy-readoptions
	   #:open-db
	   #:close-db
	   #:put-kv
	   #:get-kv))
