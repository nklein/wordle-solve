
(asdf:defsystem #:wordle-solve
    :author "Patrick Stein <pat@nklein.com>"
    :maintainer "Patrick Stein <pat@nklein.com>"
    :version "0.1.20220223"
    :licence "UNLICENSE"
    :depends-on (#:cl-ppcre #:track-best)
    :components ((:static-file "UNLICENSE.txt")
                 (:static-file "README.md")
                 (:file "package")
                 (:file "read" :depends-on ("package"))
                 (:file "chars" :depends-on ("package"))
                 (:file "words" :depends-on ("package"
                                             "chars"))
                 (:file "entropy" :depends-on ("package"
                                               "chars"
                                               "words"))
                 (:file "guess" :depends-on ("package"
                                             "chars"
                                             "words"
                                             "entropy"))
                 (:file "filter" :depends-on ("package"))
                 (:file "score" :depends-on ("package"))
                 (:file "play" :depends-on ("package"
                                            "score"))))
