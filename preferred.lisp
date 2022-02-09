(in-package :wordle-solve)

;;;
;;; The functions here are used so that when two words score the same,
;;; we can give preference to the one that was in the answers list.
;;;

(declaim (inline make-preferred
                 preferred-score
                 preferred-answerp))
(declaim (inline preferred-score))
(defstruct (preferred (:constructor make-preferred (score answerp)))
  (score 0 :type number :read-only t)
  (answerp 0 :type boolean :read-only t))

(defmacro with-preferred ((score answerp) preferred &body body)
  (let ((p (gensym "PREFERRED-")))
    `(let* ((,p ,preferred)
            (,score (preferred-score ,p))
            (,answerp (preferred-answerp ,p)))
       ,@body)))

(defun preferred< (p1 p2)
  "If p1's score is less than the p2, return true. If the scores are equal, p1 is an answer and p2 is not, return true. Otherwise return false."
  (with-preferred (s1 a1) p1
    (with-preferred (s2 a2) p2
      (or (and (eql a1 a2)
               (< s1 s2))
          (and (not (< s2 s1))
               a1
               (not a2))))))

(defun preferred> (p1 p2)
  "If p1's score is greater than the p2, return true. If the scores are equal, p1 is an answer and p2 is not, return true. Otherwise return false."
  (with-preferred (s1 a1) p1
    (with-preferred (s2 a2) p2
      (or (and (eql a1 a2)
               (> s1 s2))
          (and (not (> s2 s1))
               a1
               (not a2))))))
