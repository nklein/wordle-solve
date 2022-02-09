(in-package #:wordle-solve)

(defun guess (words &key answers &allow-other-keys)
  (entropy-guess words :answers answers))
