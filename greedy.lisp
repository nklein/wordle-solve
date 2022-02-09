(in-package #:wordle-solve)

;;; This file specifies a guesser that guesses the exact word
;;; in the dictionary that has the highest expectation of
;;; minimizing the number of words still in the dictionary
;;; after the guess.
;;;
;;; This differs from the elimination because in elimination it
;;; makes probabilistic guesses based on individual letter positions.
;;; This brute-forces using whole words at a time.

(defun greedy-guess (words &key (keep 1) answers)
  (let ((answers (or answers
                     words)))
    (values (track-best:with-track-best (:keep keep :order-by-fn #'preferred<)
              (when words
                (loop :with nn := (length answers)
                   :for guess :in words
                   :for remaining := (/ (loop :for target :in answers
                                           :for result := (score-guess guess target)
                                           :summing (length (filter answers (list guess result))))
                                        nn)
                   :for score := (make-preferred remaining
                                                 (or (eql words answers)
                                                     (not (null (member guess answers :test #'string=)))))
                   :do (track-best:track guess score)))))))
