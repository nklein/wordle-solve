(in-package #:wordle-solve)

(defun play-game (words target &key (guesser #'entropy-guess))
  (loop :with guess-results := nil
     :for n :from 1
     :for guess := (funcall guesser (filter* words guess-results))
     :for result := (score-guess guess target)
     :until (null guess)
     :do (setf guess-results (append guess-results
                                     (list `(,guess ,result))))
     :until (string= guess target)
     :finally (return (values (when guess
                                n)
                              guess-results))))
