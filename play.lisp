(in-package #:wordle-solve)

(defun play-game (target &key (guesser #'entropy-guess) words answers iterator)
  (assert (or iterator
              (and words guesser)))
  (loop :with game-iterator := (or iterator
                                   (make-game-iterator words :guesser guesser :answers answers))
     :for n :from 1
     :for guess := (funcall game-iterator) :then (funcall game-iterator result)
     :for result := (score-guess guess target)
     :until (null guess)
     :collect (list guess result) :into guess-results
     :until (string= guess target)
     :finally (return (values (when guess
                                n)
                              guess-results))))
