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
	   #:cancel-all-background-work
	   #:put-kv
	   #:get-kv
	   #:create-iter
	   #:destroy-iter
	   #:move-iter-to-first
	   #:move-iter-forward
	   #:move-iter-backword
	   #:valid-iter-p
	   #:iter-key
	   #:iter-value
	   #:with-open-db
	   #:with-iter))
