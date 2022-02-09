
(asdf:defsystem #:wordle-solve
    :author "Patrick Stein <pat@nklein.com>"
    :maintainer "Patrick Stein <pat@nklein.com>"
    :version "1.0.20220209"
    :licence "UNLICENSE"
    :depends-on (#:cl-ppcre #:track-best)
    :components ((:static-file "UNLICENSE.txt")
                 (:static-file "README.md")
                 (:file "package")
                 (:file "read" :depends-on ("package"))
                 (:file "chars" :depends-on ("package"))
                 (:file "words" :depends-on ("package"
                                             "chars"))
                 (:file "preferred" :depends-on ("package"))
                 (:file "entropy" :depends-on ("package"
                                               "chars"
                                               "words"
                                               "preferred"))
                 (:file "elimination" :depends-on ("package"
                                                   "chars"
                                                   "words"
                                                   "preferred"))
                 (:file "greedy" :depends-on ("package"
                                              "chars"
                                              "words"
                                              "preferred"
                                              "filter"
                                              "score"))
                 (:file "guess" :depends-on ("package"
                                             "chars"
                                             "words"
                                             "entropy"))
                 (:file "filter" :depends-on ("package"))
                 (:file "score" :depends-on ("package"))
                 (:file "iterator" :depends-on ("package"
                                                "filter"
                                                "score"))
                 (:file "play" :depends-on ("package"
                                            "score"
                                            "entropy"
                                            "iterator"))
                 (:file "precomputed" :depends-on ("package"
                                                   "filter"
                                                   "score"))
                 (:file "stats" :depends-on ("package"
                                             "iterator"
                                             "play"))))
