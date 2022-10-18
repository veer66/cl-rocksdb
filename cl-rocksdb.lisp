;;;; cl-rocksdb.lisp

(in-package #:cl-rocksdb)

(define-foreign-library rocksdb
  (:win32 "rocksdb")
  (t (:default "librocksdb")))

(use-foreign-library rocksdb)

(defcfun ("rocksdb_options_create" create-options) :pointer)
(defcfun ("rocksdb_options_destroy" destroy-options) :void (options :pointer))
(defcfun ("rocksdb_options_increase_parallelism" increase-parallelism) :void (opt :pointer) (total-threads :int))
(defcfun ("rocksdb_options_optimize_level_style_compaction" optimize-level-style-compaction) :void (opt :pointer) (memtable_memory_budget :uint64))
(defcfun ("rocksdb_options_set_create_if_missing" set-create-if-missing) :void (opt :pointer) (val :boolean))

(defcfun ("rocksdb_writeoptions_create" create-writeoptions) :pointer)
(defcfun ("rocksdb_writeoptions_destroy" destroy-writeoptions) :void (opt :pointer))
(defcfun ("rocksdb_readoptions_create" create-readoptions) :pointer)
(defcfun ("rocksdb_readoptions_destroy" destroy-readoptions) :void (opt :pointer))

(defcfun ("rocksdb_open" open-db*) :pointer (opt :pointer) (name :string) (errptr :pointer))
(defcfun ("rocksdb_close" close-db) :void (opt :pointer))
(defcfun ("rocksdb_cancel_all_background_work" cancel-all-background-work) :void (db :pointer) (wait :boolean))

(defcfun ("rocksdb_put" put*) :void (db :pointer) (options :pointer) (key :pointer) (keylen :unsigned-int) (val :pointer) (vallen :unsigned-int) (errptr :pointer))
(defcfun ("rocksdb_get" get*) :pointer (db :pointer) (options :pointer) (key :pointer) (keylen :unsigned-int) (vallen :pointer) (errptr :pointer))

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

(defun put-kv (db key val &optional opt)
  (unless opt
    (setq opt (create-writeoptions)))
  (let ((errptr (foreign-alloc :pointer)))
    (setf (mem-ref errptr :pointer) (null-pointer))
    (put* db
	  opt
	  (static-vectors:static-vector-pointer key)
	  (length key)
	  (static-vectors:static-vector-pointer val)
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
    (static-vectors:with-static-vectors ((key-vec
					 (length key-octets)
					 :element-type '(unsigned-byte 8)
					 :initial-contents key-octets)
					(val-vec
					 (length val-octets)
					 :element-type '(unsigned-byte 8)
					 :initial-contents val-octets))
      (put-kv db key-vec val-vec opt))))

(defun get-kv (db key &optional opt)
  (unless opt
    (setq opt (create-readoptions)))
  (let ((errptr (foreign-alloc :pointer))
	(val-len-ptr (foreign-alloc :unsigned-int)))
    (setf (mem-ref errptr :pointer) (null-pointer))
    (let ((val (get* db
		     opt
		     (static-vectors:static-vector-pointer key)
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
		 (val-vec (static-vectors:make-static-vector val-len
							     :element-type '(unsigned-byte 8)))
		 (val-vec-ptr (static-vectors:static-vector-pointer val-vec)))
	    (static-vectors:replace-foreign-memory val-vec-ptr val val-len)
	    val-vec))))))

(defun get-kv-str (db key &optional opt)
  (let ((key-octets (babel:string-to-octets key)))
    (static-vectors:with-static-vectors ((key-vec
					 (length key-octets)
					 :element-type '(unsigned-byte 8)
					 :initial-contents key-octets))
      (let ((val-vec (get-kv db key-vec opt)))
	(when val-vec
	  (let ((val-str (babel:octets-to-string val-vec)))
	    (static-vectors:free-static-vector val-vec)
	    val-str))))))

(defun create-iter (db &optional opt)
  (unless opt
    (setq opt (create-readoptions)))
  (create-iter* db opt))

(defun pointer-to-static-octets (ptr len)
  (let* ((vec (static-vectors:make-static-vector len :element-type '(unsigned-byte 8)))
	 (vec-ptr (static-vectors:static-vector-pointer vec)))
    (static-vectors:replace-foreign-memory vec-ptr ptr len)
    vec))

(defun iter-key (iter)
  (let ((klen-ptr (foreign-alloc :unsigned-int)))
    (setf (mem-ref klen-ptr :unsigned-int) 0)
    (let ((key-ptr (iter-key* iter klen-ptr)))
      (pointer-to-static-octets key-ptr (mem-ref klen-ptr :unsigned-int)))))

(defun iter-key-str (iter)
  (let ((key-vec (iter-key iter)))
    (when key-vec
	(let ((key-str (babel:octets-to-string key-vec)))
	  (static-vectors:free-static-vector key-vec)
	  key-str))))

(defun iter-value (iter)
  (let ((len-ptr (foreign-alloc :unsigned-int)))
    (setf (mem-ref len-ptr :unsigned-int) 0)
    (let ((value-ptr (iter-value* iter len-ptr)))
      (pointer-to-static-octets value-ptr (mem-ref len-ptr :unsigned-int)))))

(defun iter-value-str (iter)
  (let ((val-vec (iter-value iter)))
    (when val-vec
	(let ((val-str (babel:octets-to-string val-vec)))
	  (static-vectors:free-static-vector val-vec)
	  val-str))))

(defmacro with-open-db ((db-var db-path &optional opt) &body body)
  `(let ((,db-var (open-db ,db-path ,opt)))
     (unwind-protect (progn ,@body)
       (close-db ,db-var))))

(defmacro with-iter ((iter-var db &optional opt) &body body)
  `(let ((,iter-var (create-iter ,db ,opt)))
     (unwind-protect (progn ,@body)
       (destroy-iter ,iter-var))))
