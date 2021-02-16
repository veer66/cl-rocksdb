;;;; cl-rocksdb.lisp

(in-package #:cl-rocksdb)

(define-foreign-library rocksdb
  (:win32 "rocksdb")
  (t (:default "librocksdb")))

(use-foreign-library rocksdb)

(defcfun ("rocksdb_options_create" create-options) :pointer)
(defcfun ("rocksdb_options_destroy" destroy-options) :void (options :poi nter))
(defcfun ("rocksdb_options_increase_parallelism" increase-parallelism) :void (opt :pointer) (total-threads :int))
(defcfun ("rocksdb_options_optimize_level_style_compaction" optimize-level-style-compaction) :void (opt :pointer) (memtable_memory_budget :uint64))
(defcfun ("rocksdb_options_set_create_if_missing" set-create-if-missing) :void (opt :pointer) (val :unsigned-char))

(defcfun ("rocksdb_writeoptions_create" create-writeoptions) :pointer)
(defcfun ("rocksdb_writeoptions_destroy" destroy-writeoptions) :void (opt :pointer))
(defcfun ("rocksdb_readoptions_create" create-readoptions) :pointer)
(defcfun ("rocksdb_readoptions_destroy" destroy-readoptions) :void (opt :pointer))

(defcfun ("rocksdb_open" open-db*) :pointer (opt :pointer) (name :string) (errptr :pointer))
(defcfun ("rocksdb_close" close-db) :void (opt :pointer))

(defcfun ("rocksdb_put" put*) :void (db :pointer) (options :pointer) (key :pointer) (keylen :unsigned-int) (val :pointer) (vallen :unsigned-int) (errptr :pointer))
(defcfun ("rocksdb_get" get*) :pointer (db :pointer) (options :pointer) (key :pointer) (keylen :unsigned-int) (vallen :pointer) (errptr :pointer))

(defcfun ("rocksdb_create_iterator" create-iterator) :pointer (db :pointer) (opt :pointer))
(defcfun ("rocksdb_iter_destroy" destroy-iterator) :void (iter :pointer))
(defcfun ("rocksdb_iter_valid" valid?) :boolean (iter :pointer))
(defcfun ("rocksdb_iter_next" move-next) :void (iter :pointer))
(defcfun ("rocksdb_iter_prev" move-prev) :void (iter :pointer))
