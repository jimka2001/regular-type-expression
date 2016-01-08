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

  (deftype conventional-ordinary-lambda-list ()
    (let* ((var 'var-designator)
	   (optional-var `(:or ,var (:and list (rte:rte ,var
							(:0-1 t
							      (:0-1 ,var))))))
	   ;; [&optional {var | (var [init-form [supplied-p-parameter]])}*]
	   (optional `(:cat (eql &optional) (:0-* ,optional-var)))
	 
	   ;; [&rest var]
	   (rest `(:cat (eql &rest) ,var))

	   ;; var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])
	   (key-var `(:or ,var
			  (:and list
				(rte:rte (:or ,var (cons keyword (cons ,var null)))
					 (:0-1 t
					       (:0-1 ,var))))))
	   ;; [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]]
	   (key `(:cat (eql &key) (:0-* ,key-var) (:0-1 (eql &allow-other-keys))))
	   ;; var | (var [init-form])
	   (aux-var `(:or ,var (:and list (rte:rte ,var (:0-1 t)))))
	   ;; [&aux {var | (var [init-form])}*]
	   (aux `(:cat (eql &aux) (:0-* ,aux-var))))

      `(rte:rte
	(:0-* ,var)
	(:0-1 ,optional)
	(:0-1 ,rest)
	(:0-1 ,key)
	(:0-1 ,aux))))

  ;; required
  (assert-true (typep '() 'conventional-ordinary-lambda-list))
  (assert-true (typep '(a) 'conventional-ordinary-lambda-list))
  (assert-false (typep '(:a) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(a b c) 'conventional-ordinary-lambda-list))

  ;; &optional
  (assert-true  (typep '(&optional a) 'conventional-ordinary-lambda-list))
  (assert-false (typep '(&optional :a) 'conventional-ordinary-lambda-list))
  (assert-true  (typep '(&optional a b c) 'conventional-ordinary-lambda-list))
  (assert-true  (typep '(&optional a (b) (c)) 'conventional-ordinary-lambda-list))
  (assert-true  (typep '(&optional (a 12)) 'conventional-ordinary-lambda-list))
  (assert-false (typep '(&optional (:a 12)) 'conventional-ordinary-lambda-list))
  (assert-true  (typep '(&optional (a 12 a-p)) 'conventional-ordinary-lambda-list))
  (assert-true  (typep '(&optional (a 12 a-p)  (b 12 b-p)  (c 12 c-p)) 'conventional-ordinary-lambda-list))


  ;; required + &optional
  (assert-true  (typep '(x &optional a) 'conventional-ordinary-lambda-list))
  (assert-true  (typep '(x y &optional a b) 'conventional-ordinary-lambda-list))
  (assert-true  (typep '(x y &optional (a) b) 'conventional-ordinary-lambda-list))

  ;; &key
  (assert-true (typep '(&key &allow-other-keys) 'conventional-ordinary-lambda-list))
  (assert-false (typep '(&allow-other-keys &key) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(x &key &allow-other-keys) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(x y &key &allow-other-keys) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&key a b &allow-other-keys) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&key a (b) &allow-other-keys) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&key ((:x a)) ((:y b)) &allow-other-keys) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&key ((:x a) 42) ((:y b) 42 b-p) &allow-other-keys) 'conventional-ordinary-lambda-list))
  
  (assert-true (typep '(&key) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(x &key) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(x y &key) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&key a b) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&key ((:x a)) ((:y b))) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&key ((:x a) 42) ((:y b) 42 b-p)) 'conventional-ordinary-lambda-list))

  ;; &optional + &key
  (assert-true (typep '(&optional &key &allow-other-keys) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(x &optional u &key &allow-other-keys) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(x y &optional u &key &allow-other-keys) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&optional u &key a b &allow-other-keys) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&optional u &key ((:x a)) ((:y b)) &allow-other-keys) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&optional u &key ((:x a) 42) ((:y b) 42 b-p) &allow-other-keys) 'conventional-ordinary-lambda-list))
  
  (assert-false (typep '(&optional u &optional a &key) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(x &optional u &key) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(x y &optional u &key) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&optional u &key a b) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&optional u &key ((:x a)) ((:y b))) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&optional u &key ((:x a) 42) ((:y b) 42 b-p)) 'conventional-ordinary-lambda-list))

  (assert-false (typep '(&key &optional) 'conventional-ordinary-lambda-list))
  
  ;; &rest
  (assert-true (typep '(&rest args) 'conventional-ordinary-lambda-list))
  (assert-false (typep '(&rest) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&rest args &key) 'conventional-ordinary-lambda-list))
  (assert-false (typep '(&key &rest args) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&rest args &key x) 'conventional-ordinary-lambda-list))
  (assert-false (typep '(&rest &key x) 'conventional-ordinary-lambda-list))
  
  ;; &aux
  (assert-true (typep '(&aux) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&aux r) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&aux r s) 'conventional-ordinary-lambda-list))
  (assert-false (typep '(&aux r s &key) 'conventional-ordinary-lambda-list))
  (assert-true (typep '(&aux (r) (s 42)) 'conventional-ordinary-lambda-list))

  ;; &rest + &aux
  (assert-true (typep '(&rest args &aux (r) (s 42)) 'conventional-ordinary-lambda-list))

  )
