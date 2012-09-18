LOCAL-FUNCTION enables to describe and execute recuresive local function
which is similar to the named-let in Scheme language.

LOCAL-FUNCTION supports NO-RETURN function call feature, which will be
translated into a GO expression.
NO-RETUREN can be located anywhere upon local-function body.

NOTE: jus on this implementation, NO-RETURN not supported to call other
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
    ((f (#3=#:G3 #4=#:G4)
       (declare (integer #3# #4#))
       (tagbody #2=#:G2
         (let ((i #3#) (sum #4#))
           (declare (integer i sum))
           (return-from f
             (progn
               (if (> i 0)
                 (progn
                   (psetq i (the integer (1- i))
                          sum (the integer (+ sum i)))
                   (go #2#) ))
               (format t \"~&SUM is ~a~%\" sum)
               (return-from #1# sum) ))))))    ; or, just sum
    (f x 0) )))

and enable to execute pseudo recursive call deeper than deeper.

> (fx 100000000)
SUM is 5000000050000000
5000000050000000
>

