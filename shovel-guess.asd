;;;; shovel-guess.asd

(asdf:defsystem #:shovel-guess
  :serial t
  :description "Describe shovel-guess here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:shovel
               #:hunchentoot
               #:sqlite)
  :components ((:file "package")
               (:file "shovel-guess")))

