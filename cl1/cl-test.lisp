#!/usr/bin/env -S sbcl --script

;; "sbcl --script" implies "--no-userinit", so load it:
(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))

(ql:quickload '("postmodern" "cl-json"))
(use-package :postmodern)

(connect-toplevel "pkr" "pkr" "" "/run/postgresql") ; db user pwd host

(defun qa (fun &rest args)
  (let* ((q (format nil "select ok, js from ~a(~{~a~^,~})"
                    fun args))
         (res (query q :row))
         (ok (first res))
         (js (second res)))
    (let ((json:*json-identifier-name-to-lisp* #'identity)
          (json:*identifier-name-to-key*       #'identity))
      (values ok (json:decode-json-from-string js)))))

(defun assoc-value (item alist)
  (cdr (assoc item alist :test #'equal)))

(multiple-value-bind (ok js)
    (qa "things")
  (assert (eq t ok))
  (assert (= 2 (length js)))
  (assert (string= "one" (assoc-value "name" (first js))))
  (assert (string= "2021-10-02" (assoc-value "created_at" (second js)))))

(disconnect-toplevel)

(write-line "tests have passed successfully")
