;; Copyright (c) 2016 EPITA Research and Development Laboratory
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

(in-package :rte)



(defun foo (x)
  (destructuring-case x
    ((arg1)
     (declare (type float arg1))
     (list arg1))
    ((arg1)
     (declare (type number arg1))
     (list arg1))
    ((arg1)
     (declare (type string arg1))
     (list arg1)))

  (destructuring-case x
    ((arg1)
     (declare (type number arg1))
     (list arg1))
    ((arg1)
     (declare (type float arg1))
     (list arg1))
    ((arg1)
     (declare (type string arg1))
     (list arg1)))

  (destructuring-case x
    ((&optional (arg1 0.0))
     (declare (type float arg1))
     (list arg1 43))
    ((&optional (arg1 0))
     (declare (type number arg1))
     (list arg1 42)))
  
  
  (destructuring-case x
    ((&optional (arg1 0) &rest args)
     (declare (type number arg1))
     (list arg1 args 42))
    ((&optional (arg1 0.0) &rest args)
     (declare (type float arg1))
     (list arg1 args 43)))

  (destructuring-case x
    ((&optional (x 0) y &rest args)
     (list x y args))
    ((&key x)
     (list x))))


