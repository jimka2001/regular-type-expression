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


(in-package   :rte)

(defclass asdf-user::rte-cl-source-file (asdf:cl-source-file)
  ()
  (:documentation
   "This class adds some qualified methods to ASDF:PERFORM to manage a .rte
file in the fasl directory.  A file with a .rte extension is created during
ASDF:COMPILE-OP and the file is loaded with LOAD during ASDF:LOAD-OP.
The file defines functions which implement a pattern recognizer for
each rte pattern newly encountered while compiling the file.

To instruct ASDF to instantiate ASDF::RTE-CL-SOURCE-FILE rather than
the normal ASDF::CL-SOURCE-FILE, substitute the keyword
:RTE-CL-SOURCE-FILE in place of :LOAD in the :COMPONENTS section
of the ASDF:DEFSYSTEM."))

(defmethod asdf:perform :before ((operation asdf:load-op) (file asdf-user::rte-cl-source-file))
  "Load the .rte file in the fasl directory if it exists with the same base file name."
  (let ((fasl (first (asdf/action::input-files operation file))))
    (when fasl
      (let ((registry (merge-pathnames (make-pathname :type "rte") fasl)))
	(when (probe-file registry)
	  ;; (format t "reading patterns from file ~A" registry)
	  (load registry))))))

(defmethod asdf:perform :around ((operation asdf:compile-op) (file asdf-user::rte-cl-source-file))
  "Create a file in the fasl directory whose lisp text defines the matching functions for the
rte patterns which come into existing while compiling FILE."
  (let ((old-patterns (copy-list (get-patterns)))
	;; (lisp (first (asdf/action::input-files operation file)))
	(fasl (first (asdf/action::output-files operation file))))
    (prog1 (call-next-method)
      (when fasl
	(let ((patterns (set-difference (get-patterns) old-patterns :test #'equal))
	      (registry (merge-pathnames (make-pathname :type "rte") fasl)))
	  ;; (format t "creating ~d patterns in file ~A" (length patterns) registry)
	  (with-open-file (stream registry :direction :output :if-exists :supersede)
	    (serialize-functions stream patterns)))))))

