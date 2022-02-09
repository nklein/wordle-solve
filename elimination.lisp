(in-package #:wordle-solve)

;;; This file specifies a guesser that attempts to guess to maximize
;;; the expected number of words that will be eliminated.

(defun make-positional-green-scanner (pos ch)
  (cl-ppcre:create-scanner `(:SEQUENCE
                             :MODELESS-START-ANCHOR
                             ,@(loop :for ii :below pos :collecting :EVERYTHING)
                             ,ch
                             ,@(loop :for ii :from (1+ pos) :below 5 :collecting :EVERYTHING)
                             :MODELESS-END-ANCHOR-NO-NEWLINE)))

(defun make-positional-yellow-scanner-tree (pos ch)
  `(:SEQUENCE
    (:POSITIVE-LOOKAHEAD
     (:SEQUENCE (:GREEDY-REPETITION 0 nil :EVERYTHING) ,ch)
     :MODELESS-START-ANCHOR
     ,@(loop :for ii :below pos :collecting :EVERYTHING)
     (:INVERTED-CHAR-CLASS ,ch)
     ,@(loop :for ii :from (1+ pos) :below 5 :collecting :EVERYTHING)
     :MODELESS-END-ANCHOR-NO-NEWLINE)))

(defun make-positional-yellow-scanner (pos ch)
  (cl-ppcre:create-scanner (make-positional-yellow-scanner-tree pos ch)))

(defun calc-positional-result-hists (words)
  (loop :with hists := (make-array '(5 26 3) :element-type 'integer :initial-element 0)
     :for ii :below 5
     :do (loop :for jj :below 26
            :for ch := (index-char jj)
            :for greenp := (make-positional-green-scanner ii ch)
            :for yellowp := (make-positional-yellow-scanner ii ch)
            :do (loop :for word :in words
                   :for color := (cond
                                   ((funcall greenp word 0 5)
                                    0)
                                   ((funcall yellowp word 0 5)
                                    1)
                                   (t
                                    2))
                   :do (incf (aref hists ii jj color))))
     :finally (return hists)))

(defun calc-positional-result-expected-keeps (hists)
  (loop :with keeps := (make-array '(5 26) :element-type 'double-float :initial-element 0d0)
     :for ii :below 5
     :do (loop :for jj :below 26
            :for ss := (loop :for cc :below 3 :summing (aref hists ii jj cc))
            :when (plusp ss)
            :do (let ((gg (/ (aref hists ii jj 0) ss))
                      (yy (/ (aref hists ii jj 1) ss))
                      (bb (/ (aref hists ii jj 2) ss)))
                  ;; we keep a word if it is green or yell.
                  (setf (aref keeps ii jj) (+ 0d0
                                              (* gg gg) ;; probability of green * kept if green
                                              (* yy yy) ;; probability of yellow * kept if yellow
                                              (* bb bb)))))  ;; probability of black * kept if black
     :finally (return keeps)))

(defun elimination-guess (words &key (keep 1) answers)
  (let* ((answers (or answers
                      words))
         (hists (calc-positional-result-hists answers))
         (positional-result-expected-keeps (calc-positional-result-expected-keeps hists)))
    (values (track-best:with-track-best (:keep keep :order-by-fn #'preferred>)
              (loop :for word :in words
                 :for positional-keeps := (loop :for ii :below 5
                                             :collecting (aref positional-result-expected-keeps
                                                               ii
                                                               (char-index (char word ii))))
                 :for elims := (- 1
                                  (reduce #'* positional-keeps))
                 :for score := (make-preferred elims
                                               (or (eql answers word)
                                                   (not (null (member word answers :test #'string=)))))
                 :do (track-best:track word score))))))
