
(in-package   :rte)

(defun get-data ()
  (let ((plist '(a 100 b 200 c 300 d 400))) ;8
    (setf plist (append plist plist)) ; 16
    (setf plist (append plist plist)) ; 32
    (setf plist (append plist plist)) ; 64
    (setf plist (append plist plist)) ; 128
    (setf plist (append plist plist)) ; 256
    (setf plist (append plist plist)) ; 512
    (setf plist (append plist plist)) ; 1024
    (setf plist (append plist plist)) ; 2048
    (setf plist (append plist plist)) ; 4096
    (setf plist (append plist plist)) ; 8192
    plist))

(defun test-rte ()
  (let ((plist-yes (get-data))
	(plist-no (append (get-data) '(nil))))
    (time
     (dotimes (x 100)
       (progn (assert (typep plist-yes '(rte (:0-* symbol number))))
	      (assert (not (typep plist-no '(rte (:0-* symbol number))))))))))

(defun test-natural ()
  (labels ((check-list (obj)
	     (or (null obj)
		 (and (cdr obj)
		      (symbolp (car obj))
		      (numberp (cadr obj))
		      (check-list (cddr obj))))))
		 
    (let ((plist-yes (get-data))
	  (plist-no (append (get-data) '(nil))))
      (time
       (dotimes (x 100)
	 (progn (assert (check-list plist-yes))
		(assert (null (check-list plist-no)))))))))



(defun test-generated ()
  (labels ((check-list (obj)
	     (or (null obj)
		 (and (cdr obj)
		      (symbolp (car obj))
		      (numberp (cadr obj))
		      (check-list (cddr obj))))))
		 
    (let ((plist-yes (get-data))
	  (plist-no (append (get-data) '(nil))))
      (time
       (dotimes (x 100)
	 (progn (assert (check-generated plist-yes))
		(assert (null (check-generated plist-no)))))))))

(defun check-generated (SEQ)
  (DECLARE (TYPE SEQUENCE SEQ)
	   (OPTIMIZE (SPEED 3) (DEBUG 0)))
  (BLOCK CHECK
    (TYPECASE SEQ
      (LIST
       (TAGBODY
	  (GO 13)
	13
	  (WHEN (NULL SEQ) (RETURN-FROM CHECK T))
	  (TYPECASE
	      (IF (NULL SEQ)
		  (RETURN-FROM CHECK NIL)
		  (POP SEQ))
	    (SYMBOL (GO 14)))
	  (RETURN-FROM CHECK NIL)
	14
	  (TYPECASE
	      (IF (NULL SEQ)
		  (RETURN-FROM CHECK NIL)
		  (POP SEQ))
	    (NUMBER (GO 13)))
	  (RETURN-FROM CHECK NIL)))
      (T
       (LET ((I 0))
	 (TAGBODY
	    (GO 13)
	  13
	    (WHEN (>= I (LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (SVREF SEQ I) (INCF I)))
	      (SYMBOL (GO 14)))
	    (RETURN-FROM CHECK NIL)
	  14
	    (TYPECASE
		(IF (>= I (LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (SVREF SEQ I) (INCF I)))
	      (NUMBER (GO 13)))
	    (RETURN-FROM CHECK NIL)))))))

(defun check-generated-truncated (SEQ)
  (declare (type list seq)
	   (optimize (speed 3)))
  (BLOCK CHECK
    (LABELS ((END ()
                  (NULL SEQ))
                (NEXT ()
                  (IF (END)
                      (RETURN-FROM CHECK NIL)
                      (POP SEQ))))
         (TAGBODY
	    (GO 13)
          13
	    (WHEN (END) (RETURN-FROM CHECK T))
	    (TYPECASE (NEXT) (SYMBOL (GO 14)))
	    (RETURN-FROM CHECK NIL)
          14
	    (TYPECASE (NEXT) (NUMBER (GO 13)))
	    (RETURN-FROM CHECK NIL)))))

(LAMBDA (SEQ)
  (BLOCK CHECK
    (LABELS ((END ()
	       (NULL SEQ))
	     (NEXT ()
	       (IF (END)
		   (RETURN-FROM CHECK NIL)
		   (POP SEQ)))
	     (L13 ()
	       (WHEN (END)
		 (RETURN-FROM CHECK T))
	       (TYPECASE (NEXT)
		 (SYMBOL (L14))
		 (NUMBER (L13))
		 (t (RETURN-FROM CHECK NIL))))
	     (L14 ()
	       (TYPECASE (NEXT)
		 (NUMBER (L13))
		 (t (RETURN-FROM CHECK NIL)))))
      (L13))))


(LAMBDA (SEQ)
  (BLOCK CHECK
    (LABELS ((END ()
	       (NULL SEQ))
	     (NEXT ()
	       (IF (END)
		   (RETURN-FROM CHECK NIL)
		   (POP SEQ))))
      (TAGBODY
	 (GO 13)
       13
	 (WHEN (END) (RETURN-FROM CHECK T))
	 (TYPECASE (NEXT)
	   (SYMBOL (GO 14))
	   (NUMBER (GO 13)))
	 (RETURN-FROM CHECK NIL)
       14
	 (TYPECASE (NEXT)
	   (NUMBER (GO 13)))
	 (RETURN-FROM CHECK NIL)))))

(defvar *data* '("ababababzabab"
		 "ababababzabababab"
		 "ababababzabababab"
		 "ababababzzzzabababab"
		 "abababababababzzzzzzabababab"
		 "ababababababababababzzzzzzabababab"
		 "ababababababababababababababzzzzzzabababab"
		 "ababababzzzzzzababababababababzzzzzzabababab"
		 ))

(defun test-it-rte ()
  (count-if (lambda (str)
	      (typep str '(rte (:0-* (member #\a #\b))
                               (:0-* (eql #\z))
                               (:0-* (member #\a #\b)))))
	    *data*))


(defvar *test-scanner*  (cl-ppcre:create-scanner "^(ab)*z*(ab)*$"))
(defun test-it-ppcre ()
  (count-if (lambda (str)
	      (cl-ppcre:scan *test-scanner* str))
	    *data*))

(time (test-it 100))

