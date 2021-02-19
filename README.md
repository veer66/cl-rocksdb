# cl-rocksdb
### _Vee Satayamas <5ssgdxltv@relay.firefox.com>_

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

## Status

Experimental

## License

Apache-2.0
