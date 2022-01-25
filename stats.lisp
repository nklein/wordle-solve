(in-package #:wordle-solve)

(defun calculate-tree-stats (words)
  (let ((max 0)
        (max-word "")
        (sum 0))
    (dolist (target words (values (coerce (/ sum (length words)) 'double-float)
                                  max
                                  max-word))
      (let ((score (play-game words target)))
        (incf sum score)
        (when (< max score)
          (setf max score
                max-word target))))))
