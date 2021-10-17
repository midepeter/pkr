(defpackage #:postgresql-lisp-client
  (:use #:cl))

(in-package #:postgresql-lisp-client)

;;; Arguments to connect-toplevel are: database-name user-name password host
(postmodern:connect-toplevel "pkr" "pkr" "pkr" "localhost")

(defun qa (func &rest params)
  (destructuring-bind ((ok js))
      (postmodern:query (format nil "select ok, js from ~A(~{'~a'~^, ~})" func params))
    (values ok (cl-json:decode-json-from-string js))))

;;; Some helper functions for tests
(defun lookup (key alist)
  (cdr (assoc key alist)))

(defun keys (alist)
  (loop for (k . v) in alist
        collect k))

;;; Tests
(defun test-qa ()
  (multiple-value-bind (ok js) (qa "things")
    (assert (eql ok T))
    (assert (= (length js) 2))
    (assert (string= "one" (lookup :name (first js))))
    (assert (string= "2021-10-02" (lookup :created--at (second js)))))

  (multiple-value-bind (ok js) (qa "thing_get" 999)
    (assert (eql ok :null))
    (assert (string= "not found" (lookup :error js))))

  (multiple-value-bind (ok js) (qa "thing_get" 1)
    (assert (eql ok T))
    (assert (string= "init" (lookup :category js)))
    (assert (equal '(:active :category :created--at :id :name)
                   (sort (keys js) #'string<))))

  (multiple-value-bind (ok js) (qa "thing_add" "" "err")
    (assert (eql ok NIL))
    (assert (string= "new row for relation \"things\" violates check constraint \"no_name\""
                     (lookup :error js))))

  (postmodern:execute "begin")
  (multiple-value-bind (ok js) (qa "thing_add" "three" "test")
    (assert (eql ok T))
    (assert (string= "three" (lookup :name js)))
    (assert (string= "test" (lookup :category js)))
    (assert (eql T (lookup :active js))))
  (postmodern:execute "rollback")
  
  (multiple-value-bind (ok js) (qa "thing_rename" 2 "one")
    (assert (eql ok NIL))
    (assert (string= "duplicate key value violates unique constraint \"things_name_key\""
                     (lookup :error js))))

  (postmodern:execute "begin")
  (multiple-value-bind (ok js) (qa "thing_rename" 2 "deux")
    (assert (eql ok T))
    (assert (string= "deux" (lookup :name js)))
    (assert (eql NIL (lookup :active js)))
    (assert (eql NIL (lookup :category js))))
  (postmodern:execute "rollback"))

;; Comment out the line below, if you do not want any output
(trace qa)

(test-qa)
(postmodern:disconnect-toplevel)
