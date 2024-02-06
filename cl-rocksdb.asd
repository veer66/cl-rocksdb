;;;; cl-rocksdb.asd

(asdf:defsystem #:cl-rocksdb
  :description "Describe cl-rocksdb here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
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
							 'cl-rocksdb/test))))
