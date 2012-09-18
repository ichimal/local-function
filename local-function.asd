(in-package :cl-user)

(defpackage #:local-function-asd
  (:use :cl :asdf) )

(in-package #:local-function-asd)

(defsystem local-function
  :name "local-function"
  :version "0.2.0"
  :maintainer "SUZUKI Shingo"
  :author "SUZUKI Shingo"
  :license "MIT"
  :description "local-function: a variant of named-let with no-return feature"
  :serial nil
  :defsystem-depends-on (:asdf-project-helper)
  :components
    ((:file "local-function")) )

(defmethod perform :after ((op load-op)
                           (component (eql (find-system :local-function))) )
  (declare (ignore op component))
  (setf (system-long-description (find-system :local-function))
        (aph:convert-from-document-file "README.txt" :local-function) ))

(defmethod perform ((op test-op)
                    (component (eql (find-system :local-function))) )
  (declare (ignore op component))
  (operate 'load-op :local-function-test)
  (operate 'test-op :local-function-test :force t) )

