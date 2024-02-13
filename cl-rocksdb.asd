;;;; cl-rocksdb.asd

(asdf:defsystem #:cl-rocksdb
  :description "RocksDB binding for Common Lisp"
  :author "Vee Satayamas <vsatayamas@gmail.com>"
  :license  "APACHE-2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:babel)
  :components ((:file "package")
               (:file "cl-rocksdb")))

(asdf:defsystem #:cl-rocksdb/test
  :serial t
  :depends-on (#:cl-rocksdb #:fiveam #:uiop #:static-vectors)
  :components ((:module "t"
		:components ((:file "package")
			     (:file "test"))))
  :perform (test-op (o s)
		    (uiop:symbol-call :fiveam '#:run!
				      (uiop:find-symbol* 'low-level-suite
							 'cl-rocksdb/test))
		    (uiop:symbol-call :fiveam '#:run!
				      (uiop:find-symbol* 'lru-cache-option-suite
							 'cl-rocksdb/test))
		    (uiop:symbol-call :fiveam '#:run!
				      (uiop:find-symbol* 'property-suite
							 'cl-rocksdb/test))))
