(in-package #:wordle-solve)

(defun calculate-yellow-lookahead (ch greens yellows)
  (flet ((count-ch (list)
           (count ch list :test #'eql)))
    `(:POSITIVE-LOOKAHEAD
      (:SEQUENCE
       ,@(loop
            :repeat (max (count-ch greens)
                         (reduce #'max (mapcar #'count-ch yellows)))
            :appending `((:GREEDY-REPETITION 0 nil :EVERYTHING) ,ch))))))

(defun calculate-filter-regex (greens yellows blacks)
  `(:SEQUENCE
    ,@(loop :for ch :in (remove-duplicates (reduce #'append yellows))
         :collecting (calculate-yellow-lookahead ch greens yellows))
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

(defun collect-blacks (guess-result-list all-yellows)
  (remove-if (lambda (ch)
               (find ch all-yellows))
             (loop :for (guess result) :in guess-result-list
                :appending (loop :for ch :in (coerce guess 'list)
                              :for rr :in (coerce result 'list)
                              :when (char= rr #\b)
                              :collecting ch))))

(defun calculate-filter-regex-from-guesses (guess-result-list)
  (let* ((greens (collect-greens guess-result-list))
         (yellows (collect-yellows guess-result-list))
         (blacks (collect-blacks guess-result-list (reduce #'append yellows))))
    (calculate-filter-regex greens yellows blacks)))

(defun filter (words &rest guess-result-list)
  "This takes a series of '(guess result) items where the guess is a word guessed and a result is a string of the form /[byg]{5}/ where b means the letter was black, y means the letter was yellow, and g means the letter was green."
  (let ((regex (cl-ppcre:create-scanner (calculate-filter-regex-from-guesses guess-result-list))))
    (loop :for word :in words
       :when (funcall regex word 0 5)
       :collect word)))
