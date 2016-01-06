(LAMBDA (SEQ)
  (DECLARE (TYPE SEQUENCE SEQ)
           (OPTIMIZE (SPEED 0) (DEBUG 3) (SAFETY 3)))
  (BLOCK CHECK
    (TYPECASE SEQ
      (LIST
       (TAGBODY
	  (GO 11)
        11
	  (WHEN (NULL SEQ) (RETURN-FROM CHECK T))
	  (TYPECASE
	      (IF (NULL SEQ)
		  (RETURN-FROM CHECK NIL)
		  (POP SEQ))
	    ((MEMBER #\a #\b) (GO 14))
	    ((EQL #\c) (GO 12)))
	  (RETURN-FROM CHECK NIL)
        12
	  (WHEN (NULL SEQ) (RETURN-FROM CHECK T))
	  (TYPECASE
	      (IF (NULL SEQ)
		  (RETURN-FROM CHECK NIL)
		  (POP SEQ))
	    ((MEMBER #\a #\b) (GO 13))
	    ((EQL #\c) (GO 12)))
	  (RETURN-FROM CHECK NIL)
        13
	  (WHEN (NULL SEQ) (RETURN-FROM CHECK T))
	  (TYPECASE
	      (IF (NULL SEQ)
		  (RETURN-FROM CHECK NIL)
		  (POP SEQ))
	    ((MEMBER #\a #\b) (GO 13)))
	  (RETURN-FROM CHECK NIL)
        14
	  (WHEN (NULL SEQ) (RETURN-FROM CHECK T))
	  (TYPECASE
	      (IF (NULL SEQ)
		  (RETURN-FROM CHECK NIL)
		  (POP SEQ))
	    ((MEMBER #\a #\b) (GO 14))
	    ((EQL #\c) (GO 12)))
	  (RETURN-FROM CHECK NIL)))
      (SIMPLE-VECTOR
       (LET ((I 0))
         (TAGBODY
	    (GO 11)
          11
	    (WHEN (>= I (LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (SVREF SEQ I) (INCF I)))
	      ((MEMBER #\a #\b) (GO 14))
	      ((EQL #\c) (GO 12)))
	    (RETURN-FROM CHECK NIL)
          12
	    (WHEN (>= I (LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (SVREF SEQ I) (INCF I)))
	      ((MEMBER #\a #\b) (GO 13))
	      ((EQL #\c) (GO 12)))
	    (RETURN-FROM CHECK NIL)
          13
	    (WHEN (>= I (LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (SVREF SEQ I) (INCF I)))
	      ((MEMBER #\a #\b) (GO 13)))
	    (RETURN-FROM CHECK NIL)
          14
	    (WHEN (>= I (LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (SVREF SEQ I) (INCF I)))
	      ((MEMBER #\a #\b) (GO 14))
	      ((EQL #\c) (GO 12)))
	    (RETURN-FROM CHECK NIL))))
      (VECTOR
       (LET ((I 0))
         (TAGBODY
	    (GO 11)
          11
	    (WHEN (>= I (LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (AREF SEQ I) (INCF I)))
	      ((MEMBER #\a #\b) (GO 14))
	      ((EQL #\c) (GO 12)))
	    (RETURN-FROM CHECK NIL)
          12
	    (WHEN (>= I (LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (AREF SEQ I) (INCF I)))
	      ((MEMBER #\a #\b) (GO 13))
	      ((EQL #\c) (GO 12)))
	    (RETURN-FROM CHECK NIL)
          13
	    (WHEN (>= I (LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (AREF SEQ I) (INCF I)))
	      ((MEMBER #\a #\b) (GO 13)))
	    (RETURN-FROM CHECK NIL)
          14
	    (WHEN (>= I (LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (AREF SEQ I) (INCF I)))
	      ((MEMBER #\a #\b) (GO 14))
	      ((EQL #\c) (GO 12)))
	    (RETURN-FROM CHECK NIL))))
      (SEQUENCE
       (LET ((I 0))
         (TAGBODY
	    (GO 11)
          11
	    (WHEN (>= I (SB-SEQUENCE:LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (SB-SEQUENCE:LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (SB-SEQUENCE:ELT SEQ I) (INCF I)))
	      ((MEMBER #\a #\b) (GO 14))
	      ((EQL #\c) (GO 12)))
	    (RETURN-FROM CHECK NIL)
          12
	    (WHEN (>= I (SB-SEQUENCE:LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (SB-SEQUENCE:LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (SB-SEQUENCE:ELT SEQ I) (INCF I)))
	      ((MEMBER #\a #\b) (GO 13))
	      ((EQL #\c) (GO 12)))
	    (RETURN-FROM CHECK NIL)
          13
	    (WHEN (>= I (SB-SEQUENCE:LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (SB-SEQUENCE:LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (SB-SEQUENCE:ELT SEQ I) (INCF I)))
	      ((MEMBER #\a #\b) (GO 13)))
	    (RETURN-FROM CHECK NIL)
          14
	    (WHEN (>= I (SB-SEQUENCE:LENGTH SEQ)) (RETURN-FROM CHECK T))
	    (TYPECASE
		(IF (>= I (SB-SEQUENCE:LENGTH SEQ))
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (SB-SEQUENCE:ELT SEQ I) (INCF I)))
	      ((MEMBER #\a #\b) (GO 14))
	      ((EQL #\c) (GO 12)))
	    (RETURN-FROM CHECK NIL))))
      (T NIL))))



(LAMBDA (SEQ)
  (DECLARE (TYPE SEQUENCE SEQ)
           (OPTIMIZE (SPEED 0) (DEBUG 3) (SAFETY 3)))
  (BLOCK CHECK
    (TYPECASE SEQ
      (LIST
       ... )
      (SIMPLE-VECTOR
       ... )
      (VECTOR
       (LET ((I 0) (LEN (LENGTH SEQ)))
         (DECLARE (TYPE FIXNUM I LEN))
         (TAGBODY
	    (GO 57)
          57
	    (WHEN (>= I LEN) (RETURN-FROM CHECK T))
	    (CASE
		(IF (>= I LEN)
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (AREF SEQ I) (INCF I)))

	      ((#\a #\b) (GO 60))
	      ((#\c) (GO 58)))
	    (RETURN-FROM CHECK NIL)
          58
	    (WHEN (>= I LEN) (RETURN-FROM CHECK T))
	    (CASE
		(IF (>= I LEN)
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (AREF SEQ I) (INCF I)))
	      ((#\a #\b) (GO 59))
	      ((#\c) (GO 58)))
	    (RETURN-FROM CHECK NIL)
          59
	    (WHEN (>= I LEN) (RETURN-FROM CHECK T))
	    (CASE
		(IF (>= I LEN)
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (AREF SEQ I) (INCF I)))
	      ((#\a #\b) (GO 59)))
	    (RETURN-FROM CHECK NIL)
          60
	    (WHEN (>= I LEN) (RETURN-FROM CHECK T))
	    (CASE
		(IF (>= I LEN)
		    (RETURN-FROM CHECK NIL)
		    (PROG1 (AREF SEQ I) (INCF I)))
	      ((#\a #\b) (GO 60))
	      ((#\c) (GO 58)))
	    (RETURN-FROM CHECK NIL))))

      (SEQUENCE
       ...)
      (T NIL))))
