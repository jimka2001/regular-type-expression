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

(define-test type/regexp-parsing
  ;; implicitly asserts that they expressions parse at all
  (rte:regexp-to-rte "a+b")
  (rte:regexp-to-rte "ab+x")
  (rte:regexp-to-rte "abcde")
  (rte:regexp-to-rte "a+bc")
  (rte:regexp-to-rte "a+bc*")
  (rte:regexp-to-rte "ab+c*")
  (rte:regexp-to-rte "a|b")
  (rte:regexp-to-rte "a|bc")
  (rte:regexp-to-rte "ab|c")
  (rte:regexp-to-rte "a(b|c)")
  (rte:regexp-to-rte "(a+bc)*")
  (rte:regexp-to-rte "(a+bc)*")
  (rte:regexp-to-rte "ab+b(c*)"))


(define-test type/regexp-parsing
  ;; a+b
  (assert-true (rte:match-sequence "ab"  (rte:regexp-to-rte "a+b")))
  (assert-true (rte:match-sequence "aab"  (rte:regexp-to-rte "a+b")))
  (assert-true (rte:match-sequence "aaab"  (rte:regexp-to-rte "a+b")))
  (assert-false (rte:match-sequence "aaabb"  (rte:regexp-to-rte "a+b")))
  (assert-false (rte:match-sequence "bb"  (rte:regexp-to-rte "a+b")))
  (assert-false (rte:match-sequence "b"  (rte:regexp-to-rte "a+b")))
  (assert-false (rte:match-sequence "abc"  (rte:regexp-to-rte "a+b")))
  
  ;; ab|c
  (assert-true (rte:match-sequence "ab" (rte:regexp-to-rte "ab|c")))
  (assert-true (rte:match-sequence "c" (rte:regexp-to-rte "ab|c")))

  (assert-false (rte:match-sequence "ac" (rte:regexp-to-rte "ab|c")))
  (assert-false (rte:match-sequence "" (rte:regexp-to-rte "ab|c")))

  ;; & and []
  (assert-true (rte:match-sequence "b" (rte:regexp-to-rte "[abc]&[bcd]")))
  (assert-true (rte:match-sequence "bccb" (rte:regexp-to-rte "([abc]&[bcd])+")))

  ;; [^]
  (assert-true (rte:match-sequence "abcd" (rte:regexp-to-rte "[^xyz]+")))
  (assert-false (rte:match-sequence "abxcd" (rte:regexp-to-rte "[^xyz]+")))

  ;; .
  (assert-true (rte:match-sequence "abxab"  (rte:regexp-to-rte "ab.ab")))
  (assert-false (rte:match-sequence "ab"  (rte:regexp-to-rte "ab.")))
  
  )
