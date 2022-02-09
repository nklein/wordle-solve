(in-package #:wordle-solve)

;;; This file specifies a guesser that attempts to guess the word
;;; with the maximum entropy at each letter position and as a set of letters.

(defun calc-positional-hists (words)
  (loop :with hists := (make-array '(5 26) :element-type 'integer :initial-element 0)
     :for word :in words
     :do (loop :for ii :below 5
            :for index := (char-index (char word ii))
            :do (incf (aref hists ii index)))
     :finally (return hists)))

(defun positional-hists-to-positional-entropies (hists)
  (loop :with ents := (make-array '(5 26) :element-type 'double-float :initial-element 0d0)
     :for ii :below 5
     :for ss := (loop :for jj :below 26
                   :summing (aref hists ii jj))
     :when (plusp ss)
     :do (loop :for jj :below 26
            :for pp := (/ (aref hists ii jj) ss)
            :do (setf (aref ents ii jj) (if (plusp pp)
                                            (- (* pp (log pp 2.0d0)))
                                            0d0)))
     :finally (return ents)))


(defun calc-positional-entropies (words)
  (positional-hists-to-positional-entropies (calc-positional-hists words)))

(defun calc-bag-indexes (words)
  (mapcar #'word-bag-index words))

(defun calc-bag-hist (words &optional (indexes (calc-bag-indexes words)))
  (loop :with max := (1+ (word-bag-index "zzzzz"))
     :with hist := (make-array (list max) :element-type 'integer :initial-element 0)
     :for index :in indexes
     :do (incf (aref hist index))
     :finally (return hist)))

(defun bag-hist-to-bag-entropies (hist)
  (loop :with max := (array-dimension hist 0)
     :with ents := (make-array (list max) :element-type 'double-float :initial-element 0d0)
     :with ss := (max (loop :for ii :below max
                         :summing (aref hist ii))
                      1)
     :for ii :below max
     :for pp := (/ (aref hist ii) ss)
     :do (setf (aref ents ii) (if (plusp pp)
                                          (- (* pp (log pp 2.0d0)))
                                          0d0))
     :finally (return ents)))

(defun calc-bag-entropies (words &optional (indexes (calc-bag-indexes words)))
  (bag-hist-to-bag-entropies (calc-bag-hist words indexes)))

(defun entropy-guess (words &key (keep 1) answers)
  (let* ((answers (or answers
                      words))
         (positional-entropies (calc-positional-entropies answers))
         (indexes (calc-bag-indexes answers))
         (bag-entropies (calc-bag-entropies answers indexes)))
    (values (track-best:with-track-best (:keep keep :order-by-fn #'preferred>)
              (loop :with fudge := (sqrt (length words))
                 :for word :in words
                 :for p-ent := (loop :for ii :below 5
                                  :summing (aref positional-entropies ii (char-index (char word ii))))
                 :for w-ent := (aref bag-entropies (word-bag-index word))
                 :for score := (make-preferred (+ (* p-ent 1)
                                                  (* w-ent fudge))
                                               (or (eql words answers)
                                                   (not (null (find word answers :test #'string=)))))
                 :do (track-best:track word score))))))
