(in-package #:wordle-solve)

(defun read-words (filename)
  "Read the words from the given FILENAME, one word per line. Reject any words that aren't five letters or aren't all lowercase."
  (let ((regex (cl-ppcre:create-scanner '(:SEQUENCE
                                          :MODELESS-START-ANCHOR
                                          (:CHAR-CLASS (:RANGE #\a #\z))
                                          (:CHAR-CLASS (:RANGE #\a #\z))
                                          (:CHAR-CLASS (:RANGE #\a #\z))
                                          (:CHAR-CLASS (:RANGE #\a #\z))
                                          (:CHAR-CLASS (:RANGE #\a #\z))
                                          :MODELESS-END-ANCHOR-NO-NEWLINE))))
    (with-open-file (ff filename :direction :input)
      (loop :for word := (read-line ff nil)
         :while word
         :when (and (= (length word) 5)
                    (funcall regex word 0 5))
         :collect word))))
