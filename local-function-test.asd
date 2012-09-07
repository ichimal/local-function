(cl:in-package :cl-user)

(defpackage #:local-function-asd
  (:use :cl :asdf) )

(in-package #:local-function-asd)

(defsystem local-function-test
  :depends-on (:local-function :rt)
  :components ((:file "local-function-test")) )

(defmethod perform ((op test-op)
                    (compo (eql (find-system :local-function-test))) )
  (declare (ignore op comp))
  (funcall (intern "DO-TESTS" :rt)) )

