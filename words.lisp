(in-package #:wordle-solve)

(defun word-bag-index (word)
  (destructuring-bind (a b c d e) (sort (map 'list #'char-index word)
                                        #'<)
    (+ a
       (* b (+ b 1) 1/2)
       (* c (+ c 1) (+ c 2) 1/6)
       (* d (+ d 1) (+ d 2) (+ d 3) 1/24)
       (* e (+ e 1) (+ e 2) (+ e 3) (+ e 4) 1/120))))
