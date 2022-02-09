(defpackage #:wordle-solve
  (:use #:cl)
  (:export #:read-words)
  (:export #:entropy-guess)
  (:export #:elimination-guess)
  (:export #:greedy-guess)
  (:export #:guess)
  (:export #:filter
           #:filter*)
  (:export #:score-guess)
  (:export #:make-game-iterator)
  (:export #:precompute-guesser-tree
           #:prune-guess-tree
           #:make-guess-tree-iterator)
  (:export #:play-game)
  (:export #:calculate-tree-stats))
