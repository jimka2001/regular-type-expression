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


(defun bdd-nth-row (n)
  (let ((k 1))
    (while (> (expt 2 (- n k))
              (- (expt 2 (expt 2 k))
                 (expt 2 (expt 2 (1- k)))))
      (incf k))
    (decf k)
    (let ((rx (- (expt 2 (expt 2 k)) (expt 2 (expt 2 (1- k)))))
          (ry (- (expt 2 (expt 2 (1+ k))) (expt 2 (expt 2 k)))))
      (list k rx ry))))

(defun bdd-size (n)
  (let ((k 1))
    (while (> (expt 2 (- n k))
              (- (expt 2 (expt 2 k))
                 (expt 2 (expt 2 (1- k)))))
      (incf k))
    (decf k)
    (let ((t1 (1- (expt 2 (- n k))))
          (t2 (expt 2 (expt 2 k))))
      (list n k t1 t2 (+ t1 t2)))))

(defun map-pairs (f objs)
  (let* ((size (length objs))
         (size^2 (* size size))
         (vec (make-array size :initial-contents objs))
         (prim size))
    ;; first we make sure that all the objects get 'used if possible
    ;; by calling F with adjacent pairs first f(0 1), f(2 3), f(4 5)
    ;; ...
    (loop for i from 0 below (1- size) by 2
          do (funcall f (svref vec i) (svref vec (1+ i))))
    ;; then we continue by calling the missed adjacent pairs
    ;;   f(1 2), f(3 4), f(5 6) ...
    (loop for i from 1 below (1- size) by 2
          do (funcall f (svref vec i) (svref vec (1+ i))))
    ;; then we continue by calling the adjacent pairs in reverse order
    ;;   f(1 0), f(2 1), f(3 2), f(4 3) ...
    (loop for i from 0 below (1- size)
          do (funcall f (svref vec (1+ i)) (svref vec i)))
    (loop until (= 1 (gcd prim size))
          do (incf prim))
    (do ((n 0 (1+ n))
         (b prim (+ b prim)))
        ((= n size^2) nil)
      (multiple-value-bind (q r) (truncate b size)
        (let ((i1 (mod q size)))
          (cond
            ((= i1 r)) ;; don't call the function on the same index at
                       ;; the same time.  f(1 1)
            ((= (1+ i1) r)) ;; skip adjacent pairs because they've
                            ;; been handled already above
            ((= i1 (1+ r))) ;; skip adjacent pairs because they've
                            ;; been handled already above
            (t
             (funcall f (svref vec i1) (svref vec r)))))))))

(defun bdd-make-worst-case (vars &key (basename (format nil "/tmp/jnewton/graph/bdd-worst-~D" (length vars))))
  (let* ((leaves (list *bdd-true* *bdd-false*))
         (size 2) ;; length of leaves
         (row-num (1- (length vars)))
         (rows (list leaves)))

    ;; build up the bottom
    (while (< (* size (1- size)) (expt 2 row-num))
      (let (bdds)
        (map-pairs (lambda (o1 o2)
                     (push (bdd-node (car vars) o1 o2) bdds))
                   (reduce #'append rows :initial-value ()))
        (push bdds rows)
        (assert (= (length bdds) (* size (1- size))) (size bdds))
        (incf size (* size (1- size)))
        (pop vars)
        (decf row-num)))

    ;; build the belt with exactly (expt 2 row-num) elements,
    ;; and 2* (expt 2 row-num) arrows.
    ;; so two cases, does the previous row below have enough elements
    ;; to support the arrows?
    (let* ((n row-num)
           (m (expt 2 n))
           (p (length (car rows)))
           (needed m)
           (remaining (- m (* p (1- p))))
           bdds)

      (assert (<= p (* 2 m)) (row-num m p)
              "expecting to create BDD row of ~D elements with ~D connecting to less than ~D elements"
              m (* 2 m) (* 2 m))
      
      (block create-links-to-n+1
        ;; First construct as many as possible, but not too many nodes
        ;; pointing to row n+1.  Assuming that row n=2 contains p
        ;; number of nodes, this will create a maximum of p(p-1)
        ;; nodes.  If p*(p-1) >= 2^n then this is sufficient,
        ;; otherwise, remaining denotes how many additional need to be
        ;; created in BLOCK create-remaining.
        (map-pairs (lambda (left right)
                     (cond
                       ((plusp needed)
                        (push (bdd-node (car vars) left right) bdds)
                        (decf needed))
                       (t
                        (return-from create-links-to-n+1))))
                   (car rows)))

      (block create-remaining
        ;; Next we create any remaining nodes that are needed.  This
        ;; has some effect only in the case that p*(p-1) < 2^n, which
        ;; means that the previous block create-links-to-n+1 failed to
        ;; create 2^n nodes, because row n+1 doesn't have enough
        ;; elements.  So the strategy is to create links to as many of
        ;; the existing nodes row n+2, n+3 ... as necessary, skipping
        ;; any pair which has already been created in the previous
        ;; block.
        (map-pairs (lambda (right left &aux (bdd (bdd-node (car vars) left right)))
                     (cond
                       ;; if there's already a bdd in bdds pointing to
                       ;; these two nodes, this skip this pair.  we
                       ;; don't want duplicate nodes.
                       ((member bdd bdds :test #'eq)) 
                       ((plusp remaining)
                        (push bdd bdds)
                        (decf remaining))
                       (t
                        (return-from create-remaining))))
                   (reduce #'append rows :initial-value ())))
      
      (assert (= m (length bdds)) (m n p)
              "failed to create exactly ~D=2^~D nodes for row ~d, created ~D instead"
              m n n (length bdds))
              
                      
      (push bdds rows)
      (pop vars))
      
    ;; build the top
    (while vars
      (let (bdds
            (ptr (car rows)))
        (assert (or (= 1 (length ptr))
                    (evenp (length ptr))) (ptr)
                    "expecting either 1 or even number as length, not ~D" (length ptr))
        (while ptr
          (push (bdd-node (car vars) (pop ptr) (pop ptr)) bdds))
        (push bdds rows))
      (pop vars))

    ;; the top row has one item, that element is the worst case bdd for the given variables
    (bdd-view (car (car rows)) :basename basename)
    (car (car rows))
    (values 
     (bdd-to-expr (car (car rows)))
     (bdd-count-nodes (car (car rows))))))
