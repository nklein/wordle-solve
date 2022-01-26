(in-package #:wordle-solve)

(defun guess (words &optional (keep 1))
  (entropy-guess words keep))
