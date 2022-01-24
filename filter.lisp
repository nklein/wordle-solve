(in-package #:wordle-solve)

(defun calculate-filter-regex (positions positional-nots somewheres)
  `(:SEQUENCE
    ,@(loop :for ch :in (remove-duplicates somewheres :test #'char=)
         :collecting `(:POSITIVE-LOOKAHEAD
                       (:SEQUENCE
                        (:GREEDY-REPETITION 0 nil :EVERYTHING)
                        ,ch)))
    :MODELESS-START-ANCHOR
    ,@(loop :for pp :in positions
         :for nots :in positional-nots
         :collecting (or pp
                         (if nots
                             `(:INVERTED-CHAR-CLASS ,@(remove-duplicates nots))
                             '(:CHAR-CLASS (:RANGE #\a #\z)))))
    :MODELESS-END-ANCHOR-NO-NEWLINE))

(defun calculate-filter-regex-from-guesses (guess-result-list)
  (loop :with somewheres := nil
     :with positions := (loop :repeat 5 :collecting nil)
     :with positional-nots := (loop :repeat 5 :collecting nil)
     :for (guess result) :in guess-result-list
     :do (loop :for ch :in (coerce guess 'list)
            :for rr :in (coerce result 'list)
            :for ii :below 5
            :do (ecase rr
                  (#\g
                   (setf (nth ii positions) ch))
                  (#\y
                   (push ch (nth ii positional-nots))
                   (push ch somewheres))
                  (#\b
                   ;; do in subsequent loop
                   )))
     :do (loop :for ch :in (coerce guess 'list)
            :for rr :in (coerce result 'list)
            :for ii :below 5
            :when (and (char= rr #\b)
                       (not (find ch somewheres)))
            :do (loop :for jj :below 5
                   :do (push ch (nth jj positional-nots))))
     :finally (return (calculate-filter-regex positions
                                              positional-nots
                                              somewheres))))

(defun filter (words &rest guess-result-list)
  "This takes a series of '(guess result) items where the guess is a word guessed and a result is a string of the form /[byg]{5}/ where b means the letter was black, y means the letter was yellow, and g means the letter was green."
  (let ((regex (cl-ppcre:create-scanner (calculate-filter-regex-from-guesses guess-result-list))))
    (loop :for word :in words
       :when (funcall regex word 0 5)
       :collect word)))
