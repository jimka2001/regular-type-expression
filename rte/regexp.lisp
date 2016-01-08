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

(defpackage :rte-regexp
  (:use :cl :rte)
  (:export
   "REGEXP-TO-RTE"))

(in-package   :rte-regexp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun syntax-not-supported (&rest args)
    (error "regular expression syntax ~A is not supported" args)))

;; <RE>            ::= <union> | <simple-RE>
;; <union>         ::= <RE> "|" <simple-RE>
;; <simple-RE>     ::= <concatenation> | <basic-RE>
;; <concatenation> ::= <simple-RE> <basic-RE>
;; <basic-RE>      ::= <star> | <plus> | <elementary-RE>
;; <star>          ::= <elementary-RE> "*"
;; <plus>          ::= <elementary-RE> "+"
;; <elementary-RE> ::= <group> | <any> | <bos> | <eos> | <char> | <set> ; <bos> is missing from the published spec
;; <group>         ::= "(" <RE> ")"
;; <any>           ::= "."
;; <eos>           ::= "$"
;; <char>          ::= any non metacharacter | "\" metacharacter
;; <set>           ::= <positive-set> | <negative-set>
;; <positive-set>  ::= "[" <set-items> "]"
;; <negative-set>  ::= "[^" <set-items> "]"
;; <set-items>     ::= <set-item> | <set-item> <set-items>
;; <set-item>      ::= <range> | <char> ; on web page it is published as <set-items>     ::= <range> | <char> 
;; <range>         ::= <char> "-" <char>
(yacc:define-parser *regexp-parser*
  ;; RE grammar borrowed from
  ;; Perl Style Regular Expressions in Prolog
  ;; CMPT 384 Lecture Notes
  ;; Robert D. Cameron
  ;; November 29 - December 1, 1999
  ;;    http://www.cs.sfu.ca/~cameron/Teaching/384/99-3/regexp-plg.html
  (:start-symbol <RE>)
  (:print-lookaheads nil)		; set this to t for debugging
  (:print-goto-graph nil)		; set this to t for debugging
  (:print-states nil)			; set this to t for debugging
  (:terminals (<char> * - ? + <positive-set-prefix> <negative-set-prefix> <set-suffix> |.| <paren-open> <paren-close> <and> <or>))
  (:precedence ((:left ())
                (:left <or>)))
  
  ;; <RE>            ::= <union> | <simple-RE>
  (<RE>
   <union>
   <intersection>
   <simple-RE>)

  ;; <union>         ::= <RE> "|" <simple-RE>
  (<union>
   (<RE> <or> <simple-RE> (lambda (re1 or re2)
                            (declare (ignore or))
                            `(:or ,re1 ,re2))))

  ;; <intersection>         ::= <RE> "&" <simple-RE>
  (<intersection>
   (<RE> <and> <simple-RE> (lambda (re1 and re2)
                            (declare (ignore and))
			     `(:and ,re1 ,re2))))
  
  ;; <simple-RE>     ::= <concatenation> | <basic-RE>
  (<simple-RE>
   <concatenation>
   <basic-RE>)

  ;; <concatenation> ::= <simple-RE> <basic-RE>
  (<concatenation>
   (<simple-RE> <basic-RE> (lambda (s b)
			     `(:cat ,s ,b))))
  
  ;; <basic-RE>      ::= <star> | <plus> | <elementary-RE>
  (<basic-RE>
   <star>
   <plus>
   <elementary-RE>)
  
  ;; <star>          ::= <elementary-RE> "*"
  (<star>
   (<elementary-RE> * (lambda (re _)
                        (declare (ignore _))
                        `(:0-* ,re))))
  
  ;; <plus>          ::= <elementary-RE> "+"
  (<plus>
   (<elementary-RE> + (lambda (re _)
                        (declare (ignore _))
                        `(:1-* ,re))))
  
  ;; <elementary-RE> ::= <group> | <any> | <bos> | <eos> | <char> | <set>
  (<elementary-RE>
   <group>
   <any>
   <bos>
   <eos>
   (<char> (lambda (c)
	     `(eql ,c)))
   <set>)
  
  ;; <group>         ::= "(" <RE> ")"
  (<group>
   ( <paren-open> <RE> <paren-close>  (lambda (_ re __)
                                        (declare (ignore _ __))
                                        re)))
  
  ;; <any>           ::= "."
  (<any>
   (|.| (constantly t)))
  
  ;; <eos>           ::= "^"
  (<bos>
   (|^| #'syntax-not-supported))

  ;; <eos>           ::= "$"
  (<eos>
   (|$| #'syntax-not-supported))
  
  ;; <char>          ::= any non metacharacter | "\" metacharacter
   
  ;; terminal symbol
  
  ;; <set>           ::= <positive-set> | <negative-set>
  (<set>
   <positive-set>
   <negative-set>)
  
  ;; <positive-set>  ::= "[" <set-items> "]"
  (<positive-set>
   (<positive-set-prefix> <set-items> <set-suffix> (lambda ([ set-items ])
						     (declare (ignore [ ] ))
						     `(member ,@set-items))))
  
  ;; <negative-set>  ::= "[^" <set-items> "]"
  (<negative-set>
   (<negative-set-prefix> <set-items> <set-suffix> (lambda ([ set-items ])
						     (declare (ignore [ ] ))
						     `(not (member ,@set-items)))))

  ;; <set-items>     ::= <set-item> | <set-item> <set-items>
  (<set-items>
   (<set-item>)
   (<set-item> <set-items>  #'cons))
  
  ;; <set-item>     ::= <range> | <char> 
  (<set-item>
   <range>
   <char>)
  
  ;; <range>         ::= <char> "-" <char>
  (<range>
   (<char> - <char> #'syntax-not-supported))
  
  )
  

(defun regexp-to-rte (regexp &key (reduce t))
  (declare (type string regexp))
  (cond
    (reduce
     (canonicalize-pattern (regexp-to-rte regexp :reduce nil)))
    (t
     (with-input-from-string (stream regexp)
       (yacc:parse-with-lexer (lambda (&aux (EOF stream) it)
				(let ((char (read-char stream nil EOF))
				      (look-ahead (read-char stream nil EOF)))
				  (unless (eql EOF look-ahead)
				    (unread-char look-ahead stream))
				  (cond
				    ((eql EOF char)
				     (values nil nil))
				    ((eql #\\ char)
				     (let ((escaped (read-char stream nil EOF)))
				       (cond
					 ((eql EOF escaped)
					  (error "no character following \\"))
					 (()
					  ;; TODO extend to handle meta chars, \n for example
					  ())
					 (t
					  (values '<char> escaped)))))
				    ((char= '#\[ char)
				     (cond ((eql EOF look-ahead)
					    (error "no character following ["))
					   ((eql '#\^ look-ahead)
					    (read-char stream nil EOF) ; skip the ^ char
					    (values '<negative-set-prefix> char))
					   (t
					    (values '<positive-set-prefix> char))))
				    ((setf it (assoc char '((#\*  *)
							    (#\-  -)
							    (#\?  ?)
							    (#\+  +)
							    (#\]  <set-suffix>)
							    (#\.  |.|)
							    (#\|  <or>)
							    (#\&  <and>)
							    (#\(  <paren-open>)
							    (#\)  <paren-close>))))
				     (destructuring-bind (char symbol) it
				       (values symbol char)))
				    (t
				     (values '<char> char)))))
			      *regexp-parser*)))))


