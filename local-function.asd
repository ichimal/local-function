(in-package :cl-user)

(defpackage #:local-function-asd
  (:use :cl :asdf) )

(in-package #:local-function-asd)

(defsystem local-function
  :name "local-function"
  :version "0.1.0"
  :maintainer "SUZUKI Shingo"
  :author "SUZUKI Shingo"
  :licence "MIT"
  :description "local-function: a variant of named-let with no-return feature"
  :serial nil
  :components
    ((:file "local-function")) )

#| ;; for future work
(defmethod perform ((op test-op)
                    (compo (eql (find-system :local-function))) )
  (declare (ignore op comp))
  (operate 'load-op :local-function-test)
  (operate 'test-op :local-function-test :force t) )
|#

