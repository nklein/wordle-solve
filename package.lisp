(defpackage #:wordle-solve
  (:use #:cl)
  (:export #:read-words)
  (:export #:entropy-guess)
  (:export #:elimination-guess)
  (:export #:guess)
  (:export #:filter
           #:filter*)
  (:export #:score-guess)
  (:export #:play-game))
