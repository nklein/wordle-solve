(in-package #:wordle-solve)

(defun filter-words-and-answers (words answers &rest filter-info)
  (let* ((new-words (filter* words filter-info))
         (new-answers (if (eql words answers)
                          new-words
                          (filter* answers filter-info))))
    (values new-words new-answers)))

(defun make-game-iterator (dictionary &key (guesser #'guess) initial-guess answers)
  (let* ((words dictionary)
         (all-answers (or answers
                          words))
         (answers all-answers)
         (guess nil))
    (lambda (&optional (result "ggggg"))
      (check-type result string)
      (flet ((next-guess ()
               (if (null (rest answers))
                   (first answers)
                   (funcall guesser words :answers answers))))
        (cond
          ((string= result "ggggg")
           (setq words dictionary
                 answers all-answers
                 guess (or initial-guess (next-guess))))
          (t
           (multiple-value-setq (words answers)
             (filter-words-and-answers words answers (list guess result)))
           (setq guess (next-guess))))))))
