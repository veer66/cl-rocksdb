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
      (let ((db (open-db "/tmp/rock-basic" opt)))
	(destroy-options opt)
	(put-kv db k v)
	(let ((vv (get-kv db k)))
	  (is (equal (coerce vv 'list) '(10 20 30)))
	  (static-vectors:free-static-vector vv))
	(cancel-all-background-work db t)
	(close-db db)))))

(test basic-iter
  "Test basic iter"
  (uiop:delete-directory-tree  (make-pathname :directory (pathname-directory #p"/tmp/rock-iter/"))
			       :if-does-not-exist :ignore
			       :validate t)
  
  (static-vectors:with-static-vectors ((k1 3 :element-type '(unsigned-byte 8)
					     :initial-contents '(1 2 3))
				       (v1 3 :element-type '(unsigned-byte 8)
					     :initial-contents '(10 20 30))
				       (k2 1 :element-type '(unsigned-byte 8)
					     :initial-contents '(100))
				       (v2 1 :element-type '(unsigned-byte 8)
					     :initial-contents '(200)))
    (let ((opt (create-options)))
      (set-create-if-missing opt t)
      (let ((db (open-db "/tmp/rock-iter" opt)))
	(destroy-options opt)
	(put-kv db k1 v1)
	(put-kv db k2 v2)
	(cancel-all-background-work db t)
	(close-db db))
      (let ((db (open-db "/tmp/rock-iter")))
	(let ((iter (create-iter db)))
	  (move-iter-to-first iter)
	  (is (equal '(1 2 3) (coerce (iter-key iter) 'list)))
	  (is (equal '(10 20 30) (coerce (iter-value iter) 'list)))
	  (is (valid-iter-p iter))
	  (move-iter-forward iter)
	  (is (equal '(100) (coerce (iter-key iter) 'list)))
	  (is (equal '(200) (coerce (iter-value iter) 'list)))
	  (is (valid-iter-p iter))
	  (move-iter-forward iter)
	  (is (not (valid-iter-p iter)))
	  (destroy-iter iter))
	(cancel-all-background-work db t)
	(close-db db)))))


(run! 'low-level-suite)
