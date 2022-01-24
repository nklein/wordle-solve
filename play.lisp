(in-package #:wordle-solve)

(defun play-game (words target)
  (loop :with guess-results := nil
     :for n :from 1
     :for guess := (guess (apply #'filter words guess-results))
     :for result := (score-guess guess target)
     :until (null guess)
     :do (setf guess-results (append guess-results
                                     (list `(,guess ,result))))
     :until (string= guess target)
     :finally (return (values (when guess
                                n)
                              guess-results))))
