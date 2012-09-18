(in-package :cl-user)

(defpackage #:local-function-asd
  (:use :cl :asdf) )

(in-package #:local-function-asd)

(defsystem local-function
  :name "local-function"
  :version "0.2.0"
  :maintainer "SUZUKI Shingo"
  :author "SUZUKI Shingo"
  :licence "MIT"
  :description "local-function: a variant of named-let with no-return feature"
  :serial nil
  :components
    ((:file "local-function")) )

(defmethod perform ((op test-op)
                    (component (eql (find-system :local-function))) )
  (declare (ignore op component))
  (operate 'load-op :local-function-test)
  (operate 'test-op :local-function-test :force t) )

