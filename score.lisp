(in-package #:wordle-solve)

(defun %find-greens (guess target)
  (loop :for gg :in guess
     :for tt :in target
     :collecting (char= gg tt)))

(defun %incorporate-yellows (guess target greens)

  (loop :with target := (loop :for tt :in target
                           :for gg :in greens
                           :unless gg
                           :collect tt)
     :for green :in greens
     :for gg :in guess
     :for found := (and (not green)
                        (find gg target))
     :when found
       :do (setf target (remove gg target :test #'char= :count 1))
     :collecting (cond
                   (green #\g)
                   (found #\y)
                   (t #\b))))

(defun score-guess (guess target)
  (let ((guess (coerce guess 'list))
        (target (coerce target 'list)))
    (coerce (%incorporate-yellows guess
                                  target
                                  (%find-greens guess target))
            'string)))
