(defclass A ()
  ())

(defclass B (A)
  ())

(defun F (x) 
       (declare (optimize (speed 3)))
       (optimized-typecase x
			   (fixnum  (dotimes (y 400) (princ y)) 200)
			   ((and A (not B)) 101)
			   (B (dotimes (y 400) (princ y)) 102)
			   (integer 100)
			   (float 300)
			   ((and number (not float)) 400)))



(optimized-typecase obj
		  ((or C (not D)) 300)
		  (C 400)
		  (D 500)
		  (E 600))
(REDUCED-TYPECASE OBJ ((AND (NOT C) D) 500)
		  ((OR C (NOT D)) 300)
		  (NIL 600)
                  (NIL 400))
