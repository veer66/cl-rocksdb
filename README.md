# cl-rocksdb

RocksDB binding for Common Lisp

## Example

```Lisp
(let ((opt (create-options)))
    (set-create-if-missing opt t)
    (with-open-db (db "/tmp/rock-loop" opt)
      (put-kv-str db "A1" "B1")
      (put-kv-str db "C" "D")
      (cancel-all-background-work db t)
      (with-iter (iter db)
	(move-iter-to-first iter)
	(let ((lst nil))
	  (loop while (valid-iter-p iter)
		do
		   (print (iter-value-str iter))
		   (move-iter-forward iter))))))
```

```Lisp
(with-open-db (db "existing-db.rocks")
    (with-iter (i db)
      (move-iter-to-first i)
      (loop while (valid-iter-p i)
	    for v = (let ((v (iter-value-str i)))
		      (move-iter-forward i)
		      v)
	    do
	       (princ v)
	       (terpri))))
```

## Open as read-only db example

```
CL-USER> (ql:quickload "cl-rocksdb" :silent t)
("cl-rocksdb")
CL-USER> (make-package :ex-read-only :use '(cl cl-rocksdb))
#<PACKAGE "EX-READ-ONLY">
CL-USER> (in-package :ex-read-only)
#<PACKAGE "EX-READ-ONLY">
EX-READ-ONLY> (defparameter *opt* (create-options))
*OPT*
EX-READ-ONLY> (set-create-if-missing *opt* t)
; No values
EX-READ-ONLY> (with-open-db (db #P"/tmp/rock-ex-read-only" *opt*)
		(put-kv-str db "KEY-1" "VAL-1"))
NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;; CANNOT PUT TO READ ONLY DB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EX-READ-ONLY> (with-open-db (db #P"/tmp/rock-ex-read-only" nil :read-only t)
		(put-kv-str db "KEY-2" "VAL-2"))

"Not implemented: Not supported operation in read only mode." 
; Debugger entered on #<CL-ROCKSDB::UNABLE-TO-PUT-KEY-VALUE-TO-DB Not implemented: Not supported operation in read only mode.
; KEY:#(75 69 89 45 50)
; VAL:#(86 65 76 45 50)
;  {105E731FE3}>
[1] EX-READ-ONLY> 
; Evaluation aborted on #<CL-ROCKSDB::UNABLE-TO-PUT-KEY-VALUE-TO-DB Not implemented: Not supported operation in read only mode.
; KEY:#(75 69 89 45 50)
; VAL:#(86 65 76 45 50)
;  {105E731FE3}>
EX-READ-ONLY> (with-open-db (db #P"/tmp/rock-ex-read-only" nil :read-only t)
		(get-kv-str db "KEY-1"))
"VAL-1"
```

## Example applications

* DuHin - yet another RocksDB browser [codeberg.org/veer66/duhin](https://codeberg.org/veer66/duhin)

## Status

Alpha

## License

Apache-2.0
