(in-package :cl-rocksdb/test)

(def-suite low-level-suite :description "test C API")
(in-suite low-level-suite)

(test basic-scenario
  "Test basic scenario using CFFI"
  (uiop:delete-directory-tree  (make-pathname :directory (pathname-directory #p"/tmp/rock-basic/"))
			       :if-does-not-exist :ignore
			       :validate t)
  
  (static-vectors:with-static-vectors ((k 3 :element-type '(unsigned-byte 8)
					    :initial-contents '(1 2 3))
				       (v 3 :element-type '(unsigned-byte 8)
					    :initial-contents '(10 20 30)))
    (let ((opt (create-options)))
      (set-create-if-missing opt t)
      (let ((db (open-db "/tmp/rock-basic1" opt)))
	(destroy-options opt)
	(put-kv db k v)
	(let ((vv (get-kv db k)))
	  (is (equal (coerce vv 'list) '(10 20 30)))
	  (static-vectors:free-static-vector vv))	
	(close-db db)))))

(run! 'low-level-suite)
