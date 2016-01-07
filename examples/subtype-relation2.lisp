
(in-package :rte)

(let ((sm (make-state-machine '(:0-* (:0-1 (satisfies oddp))
				 (satisfies evenp)))))
  (ndfa:ndfa-to-dot sm #p"/tmp/jnewton/graph21.png"))

(deftype odd ()
  '(and integer (satisfies oddp)))

(deftype even ()
  '(and integer (satisfies evenp)))


(let ((sm (make-state-machine '(:0-* (:0-1 odd)
				 even))))
  (ndfa:ndfa-to-dot sm #p"/tmp/jnewton/graph22.png"))


(deftype odd ()
    '(and integer (or (eql 1) (satisfies oddp))))

(deftype even ()
  '(and integer (or (eql 0) (satisfies evenp))))


(let ((sm (make-state-machine '(:0-* (:0-1 odd)
				 even))))
  (ndfa:ndfa-to-dot sm #p"/tmp/jnewton/graph23.png"))


(deftype odd ()
    '(and integer (not (satisfies evenp)) (or (eql 1) (satisfies oddp))))

(deftype even ()
    '(and integer (not (satisfies oddp)) (or (eql 0) (satisfies evenp))))
(let ((sm (make-state-machine '(:0-* (:0-1 odd)
				 even))))
  (ndfa:ndfa-to-dot sm #p"/tmp/jnewton/graph24.png"))

