(in-package #:wordle-solve)

(defun %find-greens (guess target)
  (loop :for gg :in guess
     :for tt :in target
     :collecting (char= gg tt)))

(defun %incorporate-yellows (guess target greens)

  (labels ((remove-nth (list n)
             (cond
               ((or (null list)
                    (zerop n))
                (cdr list))
               (t
                (prog1
                    list
                  (setf (cdr list) (remove-nth (cdr list) (1- n))))))))
    (loop :with target := (loop :for tt :in target
                             :for gg :in greens
                             :unless gg
                             :collect tt)
       :for green :in greens
       :for gg :in guess
       :for pos := (and (not green)
                        (position gg target))
       :when pos
       :do (setf target (remove-nth target pos))
       :collecting (cond
                     (green #\g)
                     (pos #\y)
                     (t #\b)))))

(defun score-guess (guess target)
  (let ((guess (coerce guess 'list))
        (target (coerce target 'list)))
    (coerce (%incorporate-yellows guess
                                  target
                                  (%find-greens guess target))
            'string)))
