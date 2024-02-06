;;;; cl-rocksdb.lisp

(in-package #:cl-rocksdb)

(define-foreign-library rocksdb
  (:win32 "rocksdb")
  (t (:default "librocksdb")))

(use-foreign-library rocksdb)


;; LRU
(defcfun ("rocksdb_cache_create_lru" create-lru-cache) :pointer (capacity :unsigned-int))


;; Block based options
(defcfun ("rocksdb_block_based_options_create" create-block-based-options) :pointer)
(defcfun ("rocksdb_block_based_options_destroy" destroy-block-based-options) :void (options :pointer))
(defcfun ("rocksdb_block_based_options_set_block_cache" set-block-based-options-block-cache) :void (options :pointer) (block-cache :pointer))
(defcfun ("rocksdb_block_based_options_set_cache_index_and_filter_blocks" set-block-based-options-cache-index-and-filter-blocks) :void (options :pointer) (val :string))


;; Options
(defcfun ("rocksdb_options_create" create-options) :pointer)
(defcfun ("rocksdb_options_destroy" destroy-options) :void (options :pointer))
(defcfun ("rocksdb_options_increase_parallelism" increase-parallelism) :void (opt :pointer) (total-threads :int))
(defcfun ("rocksdb_options_optimize_level_style_compaction" optimize-level-style-compaction) :void (opt :pointer) (memtable_memory_budget :uint64))
(defcfun ("rocksdb_options_set_create_if_missing" set-create-if-missing) :void (opt :pointer) (val :boolean))
(defcfun ("rocksdb_options_set_block_based_table_factory" set-block-based-table-factory-options) :void (opt :pointer) (table-options :pointer))
(defcfun ("rocksdb_writeoptions_create" create-writeoptions) :pointer)
(defcfun ("rocksdb_writeoptions_destroy" destroy-writeoptions) :void (opt :pointer))
(defcfun ("rocksdb_readoptions_create" create-readoptions) :pointer)
(defcfun ("rocksdb_readoptions_destroy" destroy-readoptions) :void (opt :pointer))


;; Basic functions
(defcfun ("rocksdb_open" open-db*) :pointer (opt :pointer) (name :string) (errptr :pointer))
(defcfun ("rocksdb_close" close-db) :void (opt :pointer))
(defcfun ("rocksdb_cancel_all_background_work" cancel-all-background-work) :void (db :pointer) (wait :boolean))

(defcfun ("rocksdb_put" put*) :void (db :pointer) (options :pointer) (key :pointer) (keylen :unsigned-int) (val :pointer) (vallen :unsigned-int) (errptr :pointer))
(defcfun ("rocksdb_get" get*) :pointer (db :pointer) (options :pointer) (key :pointer) (keylen :unsigned-int) (vallen :pointer) (errptr :pointer))


;; Iterator
(defcfun ("rocksdb_create_iterator" create-iter*) :pointer (db :pointer) (opt :pointer))
(defcfun ("rocksdb_iter_destroy" destroy-iter) :void (iter :pointer))
(defcfun ("rocksdb_iter_seek_to_first" move-iter-to-first) :void (iter :pointer))
(defcfun ("rocksdb_iter_valid" valid-iter-p) :boolean (iter :pointer))
(defcfun ("rocksdb_iter_next" move-iter-forward) :void (iter :pointer))
(defcfun ("rocksdb_iter_prev" move-iter-backward) :void (iter :pointer))
(defcfun ("rocksdb_iter_key" iter-key*) :pointer (iter :pointer) (klen-ptr :pointer))
(defcfun ("rocksdb_iter_value" iter-value*) :pointer (iter :pointer) (vlen-ptr :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition unable-to-open-db (error)
  ((db-path :initarg :db-path
            :reader db-path)
   (error-message :initarg :error-message
                  :reader error-message)))

(defmethod print-object ((obj unable-to-open-db) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "error-message=~A" (error-message obj))))

(define-condition unable-to-put-key-value-to-db (error)
  ((db :initarg :db
       :reader db)
   (key :initarg :key
        :reader key)
   (val :initarg :val
        :reader val)
   (error-message :initarg :error-message
                  :reader error-message)))

(define-condition unable-to-get-value-to-db (error)
  ((db :initarg :db
       :reader db)
   (key :initarg :key
        :reader key)
   (error-message :initarg :error-message
                  :reader error-message)))

(defun open-db (db-path &optional opt)
  (unless opt
    (setq opt (create-options)))
  (let ((errptr (foreign-alloc :pointer)))
    (setf (mem-ref errptr :pointer) (null-pointer))
    (let* ((db-path (if (pathnamep db-path)
                        (namestring db-path)
                        db-path))
           (db (open-db* opt db-path errptr))
           (err (mem-ref errptr :pointer)))
      (unless (null-pointer-p err)
        (error 'unable-to-open-db
               :db-path db-path
               :error-message (foreign-string-to-lisp err)))
      db)))

(defmacro clone-octets-to-foreign (lisp-array foreign-array)
  (let ((i (gensym)))
    `(loop for ,i from 0 below (length ,lisp-array)
           do (setf (mem-aref ,foreign-array :unsigned-char ,i)
                    (aref ,lisp-array ,i)))))

(defmacro clone-octets-from-foreign (foreign-array lisp-array len)
  (let ((i (gensym)))
    `(loop for ,i from 0 below ,len
           do (setf (aref ,lisp-array ,i)
                    (mem-aref ,foreign-array :unsigned-char ,i)))))

(defun put-kv (db key val &optional opt)
  (unless opt
    (setq opt (create-writeoptions)))
  (with-foreign-objects ((errptr :pointer)
                         (key* :unsigned-char (length key))
                         (val* :unsigned-char (length val)))
    (clone-octets-to-foreign key key*)
    (clone-octets-to-foreign val val*)
    (setf (mem-ref errptr :pointer) (null-pointer))
    (put* db
          opt
          key*
          (length key)
          val*
          (length val)
          errptr)
    (let ((err (mem-ref errptr :pointer)))
      (unless (null-pointer-p err)
        (error 'unable-to-put-key-value-to-db
               :db db
               :key key
               :val val
               :error-message (foreign-string-to-lisp err))))))

(defun put-kv-str (db key val &optional opt)
  (let ((key-octets (babel:string-to-octets key))
        (val-octets (babel:string-to-octets val)))
    (put-kv db key-octets val-octets opt)))

(defun get-kv (db key &optional opt)
  (unless opt
    (setq opt (create-readoptions)))

  (with-foreign-objects ((val-len-ptr :unsigned-int)
                         (errptr :pointer)
                         (key* :unsigned-char (length key)))
    (clone-octets-to-foreign key key*)
    (setf (mem-ref errptr :pointer) (null-pointer))
    (let ((val (get* db
                     opt
                     key*
                     (length key)
                     val-len-ptr
                     errptr)))
      (let ((err (mem-ref errptr :pointer)))
        (unless (null-pointer-p err)
          (error 'unable-to-get-value-to-db
                 :db db
                 :key key
                 :error-message (foreign-string-to-lisp err)))

        (unless (null-pointer-p val)
          (let* ((val-len (mem-ref val-len-ptr :unsigned-int))
                 (val* (make-array val-len
                                      :element-type '(unsigned-byte 8))))
            (clone-octets-from-foreign val val* val-len)
            val*))))))

(defun get-kv-str (db key &optional opt)
  (let ((key-octets (babel:string-to-octets key)))
    (let ((#1=val-octets (get-kv db key-octets opt)))
      (when #1#
        (babel:octets-to-string #1#)))))

(defun create-iter (db &optional opt)
  (unless opt
    (setq opt (create-readoptions)))
  (create-iter* db opt))

(defun iter-key (iter)
  (with-foreign-objects ((klen-ptr :unsigned-int))
    (setf (mem-ref klen-ptr :unsigned-int) 0)
    (let* ((key-ptr (iter-key* iter klen-ptr))
           (klen (mem-ref klen-ptr :unsigned-int))
           (key (make-array klen :element-type '(unsigned-byte 8))))
      (clone-octets-from-foreign key-ptr key klen)
      key)))

(defun iter-key-str (iter)
  (let ((#1=key-octets (iter-key iter)))
    (when #1#
      (babel:octets-to-string #1#))))

(defun iter-value (iter)
  (with-foreign-objects ((len-ptr :unsigned-int))
    (setf (mem-ref len-ptr :unsigned-int) 0)
    (let* ((value-ptr (iter-value* iter len-ptr))
           (vlen (mem-ref len-ptr :unsigned-int))
           (value* (make-array vlen :element-type '(unsigned-byte 8))))
      (clone-octets-from-foreign value-ptr value* vlen)
      value*)))

(defun iter-value-str (iter)
  (let ((#1=val-octets (iter-value iter)))
    (when #1#
      (babel:octets-to-string #1#))))

(defmacro with-open-db ((db-var db-path &optional opt) &body body)
  `(let ((,db-var (open-db ,db-path ,opt)))
     (unwind-protect (progn ,@body)
       (close-db ,db-var))))

(defmacro with-iter ((iter-var db &optional opt) &body body)
  `(let ((,iter-var (create-iter ,db ,opt)))
     (unwind-protect (progn ,@body)
       (destroy-iter ,iter-var))))
