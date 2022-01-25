(defpackage #:wordle-solve
  (:use #:cl)
  (:export #:read-words)
  (:export #:guess)
  (:export #:filter
           #:filter*)
  (:export #:score-guess)
  (:export #:play-game))
