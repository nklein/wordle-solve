(in-package #:wordle-solve)

(defun char-index (ch)
  (- (char-code ch) (char-code #\a)))

(defun index-char (ii)
  (code-char (+ ii (char-code #\a))))
