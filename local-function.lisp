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
  #.(aph:convert-from-document-file "README.txt" :local-function)
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
                   collect (list var
                                 (gensym (symbol-name var))
                                 (gensym (concatenate 'string
                                           (symbol-name var) "-UPDATE") )
                                 val )))
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
           (replace-all (src lambda-list-info)
             ;; assume (= (length from-elms) (length to-elms))
             (loop for (from to) in lambda-list-info
                   for list = (subst to from src)
                       then   (subst to from list)
                   finally (return list) ))
           ;;
           (decl-filter (decl lambda-list-info)
             (case (car decl)
               (type ; (type typespec var*)
                 (list* (first decl) (second decl)
                        (replace-all (cddr decl) lambda-list-info) ))
               ((inline notinline ftype optimize)
                ;; none of arguments of local-function affected
                (copy-list decl) )
               ((ignore ignorable)
                ;; ({ignore|ignorable} {var|(function fn)}*)
                 (warn "LOCAL-FUNCTION implement limitation: IGNORE and IGNORABLE are not supported correctly on this version: ~s~%" decl)
                (cons (car decl)
                      (replace-all (cdr decl) lambda-list-info) ))
               (otherwise
                ;; (dynamic-extent {var|(function fn)}*)
                ;; or (special var*)
                ;; or (typespec var*)
                (cons (car decl)
                      (replace-all (cdr decl) lambda-list-info) ))))
           ;;
           (replace-decls (decls-list lambda-list-info)
               ;; decls-list is a list of lists
               ;; s.t. (((type fixnum i) (ignore x)) ((optimize ...)))
               (loop for decls in decls-list
                     append (loop for decl in decls
                                  collect (decl-filter
                                            decl lambda-list-info ))))
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
    (let ((lambda-list-info (separate-pairs bindings))
          (exit-val  (gensym "EXIT-VALUE-"))
          (block-tag (gensym "BLOCK-TAG-"))
          (start-tag (gensym "START-TAG-"))
          (nr-fn     (gensym "NO-RETURN-FN-")) )
      (multiple-value-bind (doc decls body) (separate-declarations body)
        ;; make customized macrolets
        `(macrolet ((no-return ((,nr-fn ,@(mapcar #'third lambda-list-info)))
                      "to be translated into GO expr with update"
                      (unless (eq ,nr-fn ',name)
                        (error
                          "not supported general TCO: jump from ~a into ~a"
                          ',name ,nr-fn ))
                      `(progn
                         (psetq ,@(mapcan ,(lambda (var val) `(,var ,val))
                                          ',(mapcar #'second lambda-list-info)
                                          (list
                                            ;; Nerver remove this LIST here.
                                            ;; We need a list of update forms,
                                            ;; not a list of gensym symbols.
                                            ,@(mapcar #'third
                                                      lambda-list-info ))))
                         (go ,',start-tag) ))
                    ;;
                    (global-exit-from-local-function (,exit-val)
                      "for global exit from LOCAL-FUNCTION execution"
                      `(return-from ,',block-tag ,,exit-val) ))
           ;; expand local-function
           ;; global block for a LOCAL-FUNCTION expr
           (block ,block-tag  ; for global exit from a local function
             (labels ((,name ,(mapcar #'second lambda-list-info)
                        ;; allocate declarations and a documentation
                        (declare ,@(replace-decls decls lambda-list-info))
                        ,@doc
                        ;; allocate function body
                        (tagbody
                          ,start-tag  ; for no-return and tail recursion
                          (let ,(mapcar (lambda (lst) (subseq lst 0 2))
                                        lambda-list-info )
                            (declare ,@(mapcan #'identity decls))
                            (return-from ,name
                              (progn
                                ,@(butlast body)
                                ,(eliminate-tail-recursion
                                   start-tag (mapcar #'car lambda-list-info)
                                   (car (last body)) )))) )))
               ;; for debug
               ,@(when *local-function-verbose-debug*
                   `((disassemble #',name) (terpri)) )
               ;; execute local function
               (,name ,@(mapcar #'fourth lambda-list-info)) ))) ))))

