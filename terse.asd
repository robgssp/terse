(asdf:defsystem "terse"
  :description "The Terse programming language."
  :version "0.1"
  :author "Robert Glossop <correnos@gmail.com>"
  :license "Public Domain"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "parser")
               (:file "runtime")
               (:file "main")))