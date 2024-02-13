;;;; package.lisp

(defpackage #:cl-rocksdb
  (:use #:cl #:cffi)
  (:export
   ;; LRU CACHE
   #:create-lru-cache


   ;; BLOCK-BASED OPTIONS
   #:create-block-based-options
   #:destroy-block-based-options
   #:set-block-based-options-block-cache
   #:set-block-based-options-cache-index-and-filter-blocks


   ;; OPTIONS
   #:create-options
   #:destroy-options
   #:increase-parallelism
   #:optimize-level-style-compaction
   #:set-create-if-missing
   #:set-block-based-table-factory-options
   #:create-writeoptions
   #:destroy-writeoptions
   #:create-readoptions
   #:destroy-readoptions


   ;; BASIC
   #:open-db
   #:close-db
   #:cancel-all-background-work
   #:put-kv
   #:put-kv-str
   #:get-kv
   #:get-kv-str


   ;; ITERATOR
   #:create-iter
   #:destroy-iter
   #:move-iter-to-first
   #:move-iter-forward
   #:move-iter-backword
   #:valid-iter-p
   #:iter-key
   #:iter-key-str
   #:iter-value
   #:iter-value-str
   #:with-open-db
   #:with-iter


   ;; PROPERTY
   #:property-value))
