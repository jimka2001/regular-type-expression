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

(asdf:defsystem :lisp-types
  :depends-on (:dispatch)
  :components
  ((:module "lisp-types"
    :components
    ((:file "macros")
     (:file "lisp-types" :depends-on ("macros"))
     (:file "reduce" :depends-on ("macros" "lisp-types"))
     (:file "decompose" :depends-on ("reduce"))
     (:file "sat" :depends-on ("macros" "lisp-types"))
     (:file "typecase" :depends-on ("lisp-types"))
     (:file "graph" :depends-on ("macros" "lisp-types"))
     (:file "bdd" :depends-on ("macros" "lisp-types"))
     (:file "bdd-reduce" :depends-on ("bdd"))
     (:file "bdd-graph" :depends-on ("macros" "bdd" "bdd-reduce"))
     (:file "bdd-dot" :depends-on ("bdd" "bdd-reduce"))
     (:file "bdd-worst-case" :depends-on ("bdd"))
     (:file "bdd-size-simulation" :depends-on ("bdd"))
     ))))
