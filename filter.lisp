(in-package #:wordle-solve)

(defun calculate-yellow-lookahead (ch greens guess-result-list)
  (flet ((count-yellow-or-green-chs (guess-result)
           (destructuring-bind (guess result) guess-result
             (loop :for gg :in (coerce guess 'list)
                :for rr :in (coerce result 'list)
                :when (and (char= gg ch)
                           (char/= rr #\b))
                :sum 1))))
    (let ((n (max (count ch greens :test #'eql)
                  (reduce #'max (mapcar #'count-yellow-or-green-chs guess-result-list)))))
      `(:POSITIVE-LOOKAHEAD
        (:GREEDY-REPETITION ,n ,n (:SEQUENCE (:NON-GREEDY-REPETITION 0 nil :EVERYTHING) ,ch))))))

(defun calculate-filter-regex (greens yellows blacks guess-result-list)
  `(:SEQUENCE
    ,@(loop :for ch :in (remove-duplicates (reduce #'append yellows))
         :collecting (calculate-yellow-lookahead ch greens guess-result-list))
    :MODELESS-START-ANCHOR
    ,@(loop :for gg :in greens
         :for yy :in yellows
         :collecting (or gg
                         (if (or yy blacks)
                             `(:INVERTED-CHAR-CLASS ,@(remove-duplicates (append yy blacks)))
                             '(:CHAR-CLASS (:RANGE #\a #\z)))))
    :MODELESS-END-ANCHOR-NO-NEWLINE))

(defun collect-greens (guess-result-list)
  (loop :with greens := (loop :repeat 5 :collecting nil)
     :for (guess result) :in guess-result-list
     :do (loop :for ch :in (coerce guess 'list)
            :for rr :in (coerce result 'list)
            :for ii :from 0
            :when (char= rr #\g)
            :do (setf (nth ii greens) ch))
     :finally (return greens)))

(defun collect-yellows (guess-result-list)
  (loop :with yellows := (loop :repeat 5 :collecting nil)
     :for (guess result) :in guess-result-list
     :do (loop :for ch :in (coerce guess 'list)
            :for rr :in (coerce result 'list)
            :for ii :from 0
            :when (char= rr #\y)
            :do (push ch (nth ii yellows)))
     :finally (return (mapcar (lambda (ys)
                                (remove-duplicates ys :test #'char=))
                              yellows))))

(defun collect-blacks (guess-result-list yellows)
  (loop :for (guess result) :in guess-result-list
     :appending (loop :for ch :in (coerce guess 'list)
                   :for rr :in (coerce result 'list)
                   :for yy :in yellows
                   :when (and (char= rr #\b)
                              (not (find rr yy :test #'char=)))
                   :collecting ch)))

(defun calculate-filter-regex-from-guesses (guess-result-list)
  (let* ((greens (collect-greens guess-result-list))
         (yellows (collect-yellows guess-result-list))
         (blacks (collect-blacks guess-result-list yellows)))
    (calculate-filter-regex greens yellows blacks guess-result-list)))

(defun filter (words &rest guess-result-list)
  "This takes a series of '(guess result) items where the guess is a word guessed and a result is a string of the form /[byg]{5}/ where b means the letter was black, y means the letter was yellow, and g means the letter was green."
  (let ((regex (cl-ppcre:create-scanner (calculate-filter-regex-from-guesses guess-result-list))))
    (loop :for word :in words
       :when (funcall regex word 0 5)
       :collect word)))
