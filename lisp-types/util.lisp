;; Copyright (c) 2017 EPITA Research and Development Laboratory
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


(in-package :lisp-types)

(defun run-program (program args &rest options)
  #+sbcl (apply #'sb-ext:run-program program args :search t options)
  #+allegro (apply #'excl:run-shell-command
                   (apply #'vector (cons program args))
                   :wait t
                   options
                   )
  )


(defun gc ()
  #+sbcl (sb-ext::gc :full t)
  #+allegro (excl:gc t)
)

(defun caching-call (thunk key hash)
  "Helper function used by the expansion of DEF-CACHE-FUN.  This function
 manages the caching and cache search of the function defined by DEF-CACHE-FUN."
  (declare (type (function () t) thunk)
           (type list key)
           (type (or null hash-table) hash))
  (cond
    ((null hash)
     (funcall thunk))
    (t
     (apply #'values
            (multiple-value-bind (value foundp) (gethash key hash)
              (cond
                (foundp value)
                (t
                 (setf (gethash key hash) (multiple-value-list (funcall thunk))))))))))

(defmacro def-cache-fun (fun-name with-name lam-list doc-string &body body)
  "Define two functions named by FUN-NAME and WITH-NAME.  The lambda list of the 
 first function is given by LAM-LIST.  The semantics of the first function will be
 to normally simply return the value of BODY.  However, if the call site to the
 first function is within the dymamic extent of the second function, the
 the return value will be cached, and the arguments are found in the cache
 BODY is not evaluated but simply the cached value of the return value will be
 returned."
  (declare (type string doc-string))
  (let ((hash (gensym "hash")))
    `(progn
       (defvar ,hash nil)

       (defun ,with-name (thunk)
         (declare (type (function () t) thunk))
         (let ((,hash (make-hash-table :test #'equal)))
           (declare (ignorable ,hash))
           (prog1 (funcall thunk)
             (format t "finished with ~A=~A~%" ',fun-name ,hash))))
       
       (defun ,fun-name (&rest arg)
         ,doc-string
         (destructuring-bind ,lam-list arg
           (caching-call (lambda () ,@body) arg ,hash))))))
