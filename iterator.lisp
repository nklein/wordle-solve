(in-package #:wordle-solve)

(defun make-game-iterator (dictionary &key (guesser #'guess))
  (let ((words dictionary)
        (guess nil))
    (lambda (&optional (result "ggggg"))
      (check-type result string)
      (cond
        ((string= result "ggggg")
         (setq words dictionary
               guess (funcall guesser words)))
        (t
         (setq words (filter words (list guess result))
               guess (funcall guesser words)))))))
