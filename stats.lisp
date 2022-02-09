(in-package #:wordle-solve)

(defun calculate-tree-stats (words &key (guesser #'guess) answers initial-guess)
  (let* ((answers (or answers
                      words))
         (max 0)
         (max-word "")
         (sum 0)
         (max-cntr (max (/ (length answers) 100) 10))
         (cntr 0)
         (iterator (make-game-iterator words
                                       :guesser guesser
                                       :answers answers
                                       :initial-guess initial-guess)))
    (dolist (target answers (values (coerce (/ sum (length answers)) 'double-float)
                                    max
                                    max-word))
      (let ((score (play-game target :iterator iterator)))
        (when (<= max-cntr (incf cntr))
          (format t "~A: ~A   @ ~A~%" max-word max target)
          (decf cntr max-cntr))
        (incf sum score)
        (when (< max score)
          (setf max score
                max-word target))))))
