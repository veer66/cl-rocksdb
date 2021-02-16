;;;; cl-rocksdb.asd

(asdf:defsystem #:cl-rocksdb
  :description "Describe cl-rocksdb here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "cl-rocksdb")))
