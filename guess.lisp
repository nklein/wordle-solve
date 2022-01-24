(in-package #:wordle-solve)

(defun guess (words &optional (keep 1))
  (let* ((positional-entropies (calc-positional-entropies words))
         (indexes (calc-bag-indexes words))
         (bag-entropies (calc-bag-entropies words indexes)))
    (values (track-best:with-track-best (:keep keep)
              (loop :for word :in words
                 :for index :in indexes
                 :for p-ent := (loop :for ii :below 5
                                  :summing (aref positional-entropies ii (char-index (char word ii))))
                 :for w-ent := (aref bag-entropies index)
                 :do (track-best:track word (+ (/ p-ent 5)
                                               w-ent)))))))
