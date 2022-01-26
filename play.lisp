(in-package #:wordle-solve)

(defun play-game (words target &key (guesser #'entropy-guess))
  (loop :with iterator := (make-game-iterator words :guesser guesser)
     :for n :from 1
     :for guess := (funcall iterator) :then (funcall iterator result)
     :for result := (score-guess guess target)
     :until (null guess)
     :collect (list guess result) :into guess-results
     :until (string= guess target)
     :finally (return (values (when guess
                                n)
                              guess-results))))
