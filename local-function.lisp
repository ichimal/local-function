(cl:in-package :local-function-asd)

(defpackage :local-function
  (:nicknames :lf)
  (:shadow #:*local-function-verbose-debug*
           #:*local-function-force-eliminate-tail-call* )
  (:use :cl)
  (:export #:local-function #:no-return #:global-exit-from-local-function) )

(in-package :local-function)

(defvar *local-function-verbose-debug* nil
  "To enable dissassemble each internal function definition in a LOCAL-FUNCTION macro" )

(defvar *local-function-force-eliminate-tail-call* nil
  "To enable force to eliminate tail call" )

(defmacro local-function (name (&rest bindings) &body body &environment env)
  "LOCAL-FUNCTION enables to describe and execute recuresive local function
  which is similar to the named-let in Scheme language.
  
  LOCAL-FUNCTION supports NO-RETURN function call feature, which will be
  translated into a GO expression.
  NO-RETUREN can be located anywhere upon local-function body.
  
  NOTE: just on this implementation, NO-RETURN not supported to call other
  functions.

  NOTE: TCO for non NO-RETURN style recursive call is not guaranteed.

  LOCAL-FUNCTION also supports GLOBAL-EXIT-FROM-LOCAL-FUNCTION, similar to
  RETURN, for global exit from a local function.

  e.g.
  (defun fx (x)
    (declare (integer x))
    (local-function f ((i x)) (sum 0)
      (declare (integer i sum))
      (when (> i 0)
        (no-return (f (the integer (1- i)) (the integer (+ sum i)))) )
      (format t \"~&SUM is ~a~%\" sum)
      (global-exit-from-local-function sum) ))  ; or, just sum

  will be expanded to

  (defun fx (x)
    (declare (integer x))
    (block #1=#:G1
      (labels
        ((f (i sum) (declare (integer i sum))
           (tagbody #2=#:G2
             (return-from f
               (progn
                 (if (> i 0)
                   (progn
                     (psetq i (the integer (1- i))
                            sum (the integer (+ sum i)))
                     (go #2#) ))
                 (format t \"~&SUM is ~a~%\" sum)
                 (return-from #1# sum) )))))    ; or, just sum
        (f x 0) )))

  and enable to execute pseudo recursive call deeper than deeper.

  > (fx 100000000)
  SUM is 5000000050000000
  5000000050000000
  > "
  ;; helper functions
  (labels ((valid-binding-p (binding)
             "check valid LET binding or not"
             (or (and (symbolp binding) binding)
                 (and (consp binding)
                      (symbolp (car binding))
                      (null (cddr binding)) )))
           ;;
           (separate-pairs (lst)
             "separate LET style variable bindings"
             (loop for elm in lst
                   for (var val) = (if (valid-binding-p elm)
                                     (if (symbolp elm) (list elm) elm)
                                     (error "invalid binding form: ~a" elm) )
                   collect var into var-acc
                   collect val into val-acc
                   finally (return (values var-acc val-acc)) ))
           ;;
           (declare-p (form)
             (and (consp form)
                  (symbolp (car form))
                  (eq (car form) 'cl:declare) ))
           ;;
           (separate-declarations (form)
             "separate declarations and a documentation from entire body.
              this helper function returns 3 lists with VALUES;
              list of documentations, list of declarations and list of forms."
             (if (atom form)
               (values nil nil form)
               (loop for elm in form with body-p = nil
                     ;; check documentations
                     if (and (not body-p) (stringp elm))
                       collect elm into acc-doc
                     ;; check declarations
                     else if (declare-p elm)
                       if body-p
                         do (error "invalid location for declarations: ~a" elm)
                       else
                         collect (cdr elm) into acc-decl
                       end
                     ;; collect body
                     else
                       collect elm into acc-form and do (setq body-p 1)
                     end
                     finally (return (values acc-doc acc-decl acc-form)) )))
           ;;
           (replace-all (src from-elms to-elms)
             ;; assume (= (length from-elms) (length to-elms))
             (loop for from-elm in from-elms
                   for to-elm in to-elms
                   for list = (subst to-elm from-elm src)
                       then   (subst to-elm from-elm list)
                   finally (return list) ))
           ;;
           (decl-filter (decl lambda-list gs-lambda-list)
             (case (car decl)
               (type ; (type typespec var*)
                 (list* (first decl) (second decl)
                        (replace-all (cddr decl)
                                     lambda-list gs-lambda-list )))
               ((inline notinline ftype optimize)
                ;; none of arguments of local-function affected
                (copy-list decl) )
               ((ignore ignorable)
                ;; ({ignore|ignorable} {var|(function fn)}*)
                 (warn "LOCAL-FUNCTION implement limitation: IGNORE and IGNORABLE are not supported correctly on this version: ~s~%" decl)
                (cons (car decl)
                      (replace-all (cdr decl)
                                   lambda-list gs-lambda-list )))
               (otherwise
                ;; (dynamic-extent {var|(function fn)}*)
                ;; or (special var*)
                ;; or (typespec var*)
                (cons (car decl)
                      (replace-all (cdr decl)
                                   lambda-list gs-lambda-list )))))
           ;;
           (replace-decls (decls-list lambda-list gs-lambda-list)
               ;; decls-list is a list of lists
               ;; s.t. (((type fixnum i) (ignore x)) ((optimize ...)))
               (loop for decls in decls-list
                     append (loop for decl in decls
                                  collect (decl-filter
                                            decl lambda-list gs-lambda-list) )))
           ;;
           (eliminate-tail-recursion (start-tag lambda-list tail-form)
             ;; leave form as it is when force-elimination not required
             (unless *local-function-force-eliminate-tail-call*
               (return-from eliminate-tail-recursion tail-form) )
             ;; check and eliminate tail recursions
             (let ((form (macroexpand tail-form env)))
               (when form
                 (if (listp form)
                   (if (eq (car form) name)
                     ;; eliminate a tail recursive call
                     `(progn
                        (psetq ,@(mapcan (lambda (var val) `(,var ,val))
                                         lambda-list (cdr form) ))
                        (go ,start-tag) )
                     ;; check some sort of forms
                     (case (car form)
                       ((if)
                        ;; process conditional expr
                        `(if ,(cadr form)
                           ,@(mapcar (lambda (elm)
                                       (eliminate-tail-recursion
                                         start-tag lambda-list elm ))
                                     (cddr form) )))
                       ((let let* block tagbody progn catch the)
                        ;; process block code
                        `(,@(butlast form)
                           ,(eliminate-tail-recursion
                              start-tag lambda-list
                              (car (last form)) )))
                       ((flet labels) form) ; reserved
                       ((progn) form)       ; reserved
                       (otherwise form) ))  ; not tail recursive, maybe
                   ;; not a list
                   form )) )))
    ;; do parse local-function
    (multiple-value-bind (lambda-list init-params) (separate-pairs bindings)
      (let ((gs-lambda-list
              (mapcar (lambda (x) (gensym (symbol-name x))) lambda-list))
            (gs (mapcar (lambda (x) (gensym (symbol-name x))) lambda-list))
            (exit-val  (gensym "EXIT-VALUE-"))
            (block-tag (gensym "BLOCK-TAG-"))
            (start-tag (gensym "START-TAG-"))
            (nr-fn     (gensym "NO-RETURN-FN-")) )
        (multiple-value-bind (doc decls body) (separate-declarations body)
          ;; make customized macrolets
          `(macrolet ((no-return ((,nr-fn ,@gs))
                        "to be translated into GO expr with update"
                        (unless (eq ,nr-fn ',name)
                          (error
                            "not supported general TCO: jump from ~a into ~a"
                            ',name ,nr-fn ))
                        `(progn
                           (psetq ,@(mapcan ,(lambda (var val) `(,var ,val))
                                            ',gs-lambda-list (list ,@gs) ))
                           (go ,',start-tag) ))
                      ;;
                      (global-exit-from-local-function (,exit-val)
                        "for global exit from LOCAL-FUNCTION execution"
                        `(return-from ,',block-tag ,,exit-val) ))
             ;; expand local-function
             ;; global block for a LOCAL-FUNCTION expr
             (block ,block-tag  ; for global exit from a local function
               (labels ((,name ,gs-lambda-list
                          ;; allocate declarations and a documentation
                          (declare ,@(replace-decls
                                       decls lambda-list gs-lambda-list ))
                          ,@doc
                          ;; allocate function body
                          (tagbody
                            ,start-tag  ; for no-return and tail recursion
                            (let ,(mapcar (lambda (var val) `(,var ,val))
                                          lambda-list gs-lambda-list )
                              (declare ,@(mapcan #'identity decls))
                              (return-from ,name
                                (progn
                                  ,@(butlast body)
                                  ,(eliminate-tail-recursion
                                     start-tag lambda-list
                                     (car (last body)) )))) )))
                 ;; for debug
                 ,@(when *local-function-verbose-debug*
                     `((disassemble #',name) (terpri)) )
                 ;; execute local function
                 (,name ,@init-params) ))))))))

