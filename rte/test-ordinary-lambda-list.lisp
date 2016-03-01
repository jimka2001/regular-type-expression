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

(in-package :rte.test)



(define-test rte/oll

  (deftype var-designator ()
    '(and symbol
      (not (or keyword
	    (member t nil)
	    (member &optional &key &rest &whole &allow-other-keys &aux)))))

  (deftype dwim-ordinary-lambda-list ()
    (let* ((var 'var-designator)
	   (optional-var `(:or ,var (:and list (rte (:1 ,var
							(:0-1 t
							      (:0-1 ,var)))))))
	   ;; [&optional {var | (var [init-form [supplied-p-parameter]])}*]
	   (optional `(:cat (eql &optional) (:0-* ,optional-var)))
	 
	   ;; [&rest var]
	   (rest `(:cat (eql &rest) ,var))

	   ;; var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])
	   (key-var `(:or ,var
			  (:and list
				(rte (:1 (:or ,var (cons keyword (cons ,var null)))
					 (:0-1 t
					       (:0-1 ,var)))))))
	   ;; [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]]
	   (key `(:cat (eql &key) (:0-* ,key-var) (:0-1 (eql &allow-other-keys))))
	   ;; var | (var [init-form])
	   (aux-var `(:or ,var (:and list (rte (:1 ,var (:0-1 t))))))
	   ;; [&aux {var | (var [init-form])}*]
	   (aux `(:cat (eql &aux) (:0-* ,aux-var))))

      `(rte
	(:1
	 (:0-* ,var)
	 (:0-1 ,optional)
	 (:0-1 ,rest)
	 (:0-1 ,key)
	 (:0-1 ,aux)))))

  ;; required
  (assert-true (typep '() 'dwim-ordinary-lambda-list))
  (assert-true (typep '(a) 'dwim-ordinary-lambda-list))
  (assert-false (typep '(:a) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(a b c) 'dwim-ordinary-lambda-list))

  ;; &optional
  (assert-true  (typep '(&optional a) 'dwim-ordinary-lambda-list))
  (assert-false (typep '(&optional :a) 'dwim-ordinary-lambda-list))
  (assert-true  (typep '(&optional a b c) 'dwim-ordinary-lambda-list))
  (assert-true  (typep '(&optional a (b) (c)) 'dwim-ordinary-lambda-list))
  (assert-true  (typep '(&optional (a 12)) 'dwim-ordinary-lambda-list))
  (assert-false (typep '(&optional (:a 12)) 'dwim-ordinary-lambda-list))
  (assert-true  (typep '(&optional (a 12 a-p)) 'dwim-ordinary-lambda-list))
  (assert-true  (typep '(&optional (a 12 a-p)  (b 12 b-p)  (c 12 c-p)) 'dwim-ordinary-lambda-list))


  ;; required + &optional
  (assert-true  (typep '(x &optional a) 'dwim-ordinary-lambda-list))
  (assert-true  (typep '(x y &optional a b) 'dwim-ordinary-lambda-list))
  (assert-true  (typep '(x y &optional (a) b) 'dwim-ordinary-lambda-list))

  ;; &key
  (assert-true (typep '(&key &allow-other-keys) 'dwim-ordinary-lambda-list))
  (assert-false (typep '(&allow-other-keys &key) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(x &key &allow-other-keys) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(x y &key &allow-other-keys) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&key a b &allow-other-keys) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&key a (b) &allow-other-keys) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&key ((:x a)) ((:y b)) &allow-other-keys) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&key ((:x a) 42) ((:y b) 42 b-p) &allow-other-keys) 'dwim-ordinary-lambda-list))
  
  (assert-true (typep '(&key) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(x &key) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(x y &key) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&key a b) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&key ((:x a)) ((:y b))) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&key ((:x a) 42) ((:y b) 42 b-p)) 'dwim-ordinary-lambda-list))

  ;; &optional + &key
  (assert-true (typep '(&optional &key &allow-other-keys) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(x &optional u &key &allow-other-keys) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(x y &optional u &key &allow-other-keys) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&optional u &key a b &allow-other-keys) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&optional u &key ((:x a)) ((:y b)) &allow-other-keys) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&optional u &key ((:x a) 42) ((:y b) 42 b-p) &allow-other-keys) 'dwim-ordinary-lambda-list))
  
  (assert-false (typep '(&optional u &optional a &key) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(x &optional u &key) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(x y &optional u &key) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&optional u &key a b) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&optional u &key ((:x a)) ((:y b))) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&optional u &key ((:x a) 42) ((:y b) 42 b-p)) 'dwim-ordinary-lambda-list))

  (assert-false (typep '(&key &optional) 'dwim-ordinary-lambda-list))
  
  ;; &rest
  (assert-true (typep '(&rest args) 'dwim-ordinary-lambda-list))
  (assert-false (typep '(&rest) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&rest args &key) 'dwim-ordinary-lambda-list))
  (assert-false (typep '(&key &rest args) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&rest args &key x) 'dwim-ordinary-lambda-list))
  (assert-false (typep '(&rest &key x) 'dwim-ordinary-lambda-list))
  
  ;; &aux
  (assert-true (typep '(&aux) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&aux r) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&aux r s) 'dwim-ordinary-lambda-list))
  (assert-false (typep '(&aux r s &key) 'dwim-ordinary-lambda-list))
  (assert-true (typep '(&aux (r) (s 42)) 'dwim-ordinary-lambda-list))

  ;; &rest + &aux
  (assert-true (typep '(&rest args &aux (r) (s 42)) 'dwim-ordinary-lambda-list))

  )
