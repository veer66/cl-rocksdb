(in-package :cl-rocksdb/test)

(def-suite low-level-suite :description "test C API")
(in-suite low-level-suite)

(test basic-scenario
      "Test basic scenario using CFFI"
  (uiop:delete-directory-tree  (make-pathname :directory (pathname-directory #p"/tmp/rock-basic/"))
                               :if-does-not-exist :ignore
                               :validate t)
  (let ((k (make-array 3 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3)))
        (v (make-array 3 :element-type '(unsigned-byte 8)
                         :initial-contents '(10 20 30)))
        (k2 (make-array 3 :element-type '(unsigned-byte 8)
                          :initial-contents '(10 20 9)))
        (opt (create-options)))
    (set-create-if-missing opt t)
    (let ((db (open-db "/tmp/rock-basic" opt)))
      (destroy-options opt)
      (put-kv db k v)
      (let ((vv (get-kv db k)))
        (is (equal (coerce vv 'list) '(10 20 30))))
      (is (null (get-kv db k2)))
      (cancel-all-background-work db t)
      (close-db db))))

(test open-with-pathname
      "Test basic scenario using CFFI"
  (uiop:delete-directory-tree  (make-pathname :directory (pathname-directory #p"/tmp/rock-basic-p/"))
                               :if-does-not-exist :ignore
                               :validate t)
  (let ((k (make-array 3 :element-type '(unsigned-byte 8)
                         :initial-contents '(1 2 3)))
        (v (make-array 3 :element-type '(unsigned-byte 8)
                         :initial-contents '(10 20 30)))
        (k2 (make-array 3 :element-type '(unsigned-byte 8)
                          :initial-contents '(10 20 9)))
        (opt (create-options)))
    (set-create-if-missing opt t)
    (let ((db (open-db #P"/tmp/rock-basic-p" opt)))
      (destroy-options opt)
      (put-kv db k v)
      (let ((vv (get-kv db k)))
        (is (equal (coerce vv 'list) '(10 20 30))))
      (is (null (get-kv db k2)))
      (cancel-all-background-work db t)
      (close-db db))))

(test basic-iter
  "Test basic iter"
  (uiop:delete-directory-tree  (make-pathname :directory (pathname-directory #p"/tmp/rock-iter/"))
                               :if-does-not-exist :ignore
                               :validate t)
  (let ((k1 (make-array 3 :element-type '(unsigned-byte 8)
                          :initial-contents '(1 2 3)) )
        (v1 (make-array 3 :element-type '(unsigned-byte 8)
                          :initial-contents '(10 20 30)))
        (k2 (make-array 1 :element-type '(unsigned-byte 8)
                          :initial-contents '(100)))
        (v2 (make-array 1 :element-type '(unsigned-byte 8)
                          :initial-contents '(200)))
        (opt (create-options)))
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
      (close-db db))))

(test basic-macro
  "basic macro with-*"
  (uiop:delete-directory-tree  (make-pathname :directory (pathname-directory #p"/tmp/rock-with/"))
                               :if-does-not-exist :ignore
                               :validate t)
  (let ((opt (create-options))
        (k1 (make-array 3  :element-type '(unsigned-byte 8)
              :initial-contents '(1 2 3)))
        (v1 (make-array 3 :element-type '(unsigned-byte 8)
              :initial-contents '(10 20 30)))
        (k2 (make-array 1 :element-type '(unsigned-byte 8)
              :initial-contents '(100)))
        (v2 (make-array 1 :element-type '(unsigned-byte 8)
              :initial-contents '(200))))
    (set-create-if-missing opt t)
    (with-open-db (db "/tmp/rock-with" opt)
      (put-kv db k1 v1)
      (put-kv db k2 v2)

      (with-iter (iter db)
        (move-iter-to-first iter)
        (is (equal '(1 2 3) (coerce (iter-key iter) 'list)))
        (is (equal '(10 20 30) (coerce (iter-value iter) 'list)))
        (is (valid-iter-p iter))
        (move-iter-forward iter)
        (is (equal '(100) (coerce (iter-key iter) 'list)))
        (is (equal '(200) (coerce (iter-value iter) 'list)))
        (is (valid-iter-p iter))
        (move-iter-forward iter)
        (is (not (valid-iter-p iter)))))))

(test basic-string-version
  "basic string verion"
  (uiop:delete-directory-tree  (make-pathname :directory (pathname-directory #p"/tmp/rock-string/"))
                               :if-does-not-exist :ignore
                               :validate t)
  (let ((opt (create-options)))
    (set-create-if-missing opt t)
    (with-open-db (db "/tmp/rock-string" opt)
      (put-kv-str db "A1" "B1")
      (put-kv-str db "C" "D")
      (is (equal "B1" (get-kv-str db "A1")))
      (is (null (get-kv-str db "V1"))))))

(test basic-iter-string
      "basic string verion"
      (uiop:delete-directory-tree  (make-pathname :directory (pathname-directory #p"/tmp/rock-string/"))
                                   :if-does-not-exist :ignore
                                   :validate t)
      (let ((opt (create-options)))
        (set-create-if-missing opt t)
        (with-open-db (db "/tmp/rock-string" opt)
          (put-kv-str db "A1" "B1")
          (put-kv-str db "C" "DX")
          (cancel-all-background-work db t)
          (with-iter (iter db)
            (move-iter-to-first iter)
            (is (valid-iter-p iter))
            (is (equal (iter-key-str iter) "A1"))
            (is (equal (iter-value-str iter) "B1"))
            (move-iter-forward iter)
            (is (valid-iter-p iter))
            (is (equal (iter-key-str iter) "C"))
            (is (equal (iter-value-str iter) "DX"))
            (move-iter-forward iter)
            (is (not (valid-iter-p iter)))))))

(test basic-loop-with-read-only-open
      "basic string verion with read-only open"
      (uiop:delete-directory-tree  (make-pathname :directory (pathname-directory #p"/tmp/rock-loop/"))
                                   :if-does-not-exist :ignore
                                   :validate t)
      (let ((opt (create-options)))
        (set-create-if-missing opt t)
        (with-open-db (db "/tmp/rock-loop" opt)
          (put-kv-str db "A1" "B1")
          (put-kv-str db "C" "D")
          (cancel-all-background-work db t))
	(with-open-db (db "/tmp/rock-loop" opt :read-only t)
	  (with-open-db (db "/tmp/rock-loop" opt :read-only t)
	    "DO-NOTHING")
	  (with-iter (iter db)
            (move-iter-to-first iter)
            (let ((lst nil))
              (loop while (valid-iter-p iter)
                    do
                       (setq lst (cons (iter-value-str iter)
                                       lst))
                       (move-iter-forward iter))
              (is (equal '("D" "B1") lst)))))))

(def-suite lru-cache-option-suite :description "Test adjusting block based options")
(in-suite lru-cache-option-suite)

(test resizing-lru-cache
  "Test resizing LRU cache"
  (uiop:delete-directory-tree  (make-pathname :directory (pathname-directory #p"/tmp/rock-lru/"))
			       :if-does-not-exist :ignore
			       :validate t)
  (let ((lru-cache (create-lru-cache (* 1024 1024 1024)))
	(opt (create-options))
	(table-options (create-block-based-options)))
    (set-create-if-missing opt t)
    (set-block-based-options-cache-index-and-filter-blocks table-options "true")
    (set-block-based-options-block-cache table-options lru-cache)
    (set-block-based-table-factory-options opt table-options)
    (with-open-db (db "/tmp/rock-lru" opt)
      (put-kv-str db "K1" "V1")
      (is (equal "V1" (get-kv-str db "K1"))))))


(def-suite property-suite :description "Test property-related functions")
(in-suite property-suite)

(test get-estimating-count-property
      "Test get estimating count property"
      (let ((db-pathname #p"/tmp/rock-property-estimate-num/")
	    (opt (create-options)))
	(set-create-if-missing opt t)
	(uiop:delete-directory-tree db-pathname
				    :if-does-not-exist :ignore
				    :validate t)
	(with-open-db (db db-pathname opt)
	  (put-kv-str db "K1" "V1")
	  (cancel-all-background-work db t))
	(with-open-db (db db-pathname nil :read-only t)
	  (is (equal (property-value db "rocksdb.estimate-num-keys")
		     "1")))))
