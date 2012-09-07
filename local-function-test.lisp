(cl:in-package #:local-function-asd)

(defpackage :local-function-test
  (:use :cl :local-function :rt)
  (:import-from :rt #:*expected-failures*) )

(in-package :local-function-test)

(deftest recursion.1
  (local-function fun ((x 10) (sum 0))
    (if (plusp x) (fun (1- x) (+ sum x)) sum) )
  55 )

(deftest recursion.2
  (local-function fun ((x 10) (sum 0))
    (when (plusp x) (fun (1- x) (+ sum x)))
    sum )
  0 )

(deftest recursion-with-no-return.1
  (local-function fun ((x 10) (sum 0))
    (when (plusp x) (no-return (fun (1- x) (+ sum x))))
    sum )
  55 )

(deftest capture.1
  (mapcar
    #'funcall
    (local-function fun ((x 3) acc)
      (if (plusp x) (fun (1- x) (cons (lambda () x) acc)) acc) ))
  (1 2 3) )

(deftest capture.2
  (mapcar
    #'funcall
    (local-function fun ((x 3) acc)
      (when (plusp x) (no-return (fun (1- x) (cons (lambda () x) acc))))
      acc ))
  (1 2 3) )


