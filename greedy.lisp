(in-package #:wordle-solve)

;;; This file specifies a guesser that guesses the exact word
;;; in the dictionary that has the highest expectation of
;;; minimizing the number of words still in the dictionary
;;; after the guess.
;;;
;;; This differs from the elimination because in elimination it
;;; makes probabilistic guesses based on individual letter positions.
;;; This brute-forces using whole words at a time.

(defun greedy-guess (words &optional (keep 1))
  (values (track-best:with-track-best (:keep keep :order-by-fn #'<)
            (when words
              (loop :with nn := (length words)
                 :for guess :in words
                 :for remaining := (/ (loop :for target :in words
                                         :for result := (score-guess guess target)
                                         :summing (length (filter words (list guess result))))
                                      nn)
                 :do (track-best:track guess remaining))))))
