(in-package :cl-rocksdb/test)

(def-suite low-level-suite :description "test C API")
(in-suite low-level-suite)

(test basic-scenario
  "Test basic scenario using CFFI"
  (let ((options (create-options)))
    (is (= 1 1))
    (destroy-options options)))

(run! 'low-level-suite)

