(in-package #:wordle-solve)

(defun calculate-tree-stats (words &key (guesser #'guess))
  (let ((max 0)
        (max-word "")
        (sum 0)
        (max-cntr (/ (length words) 100))
        (cntr 0))
    (dolist (target words (values (coerce (/ sum (length words)) 'double-float)
                                  max
                                  max-word))
      (let ((score (play-game words target :guesser guesser)))
        (when (<= max-cntr (incf cntr))
          (format t "~A: ~A~%" target score)
          (incf cntr (- max-cntr)))
        (incf sum score)
        (when (< max score)
          (setf max score
                max-word target))))))
