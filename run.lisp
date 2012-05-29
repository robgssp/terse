#!/usr/bin/sbcl --script

(with-output-to-string (*standard-output*)
  (require 'asdf)
  (require 'terse))

(defun main ()
  (if (< (length sb-ext:*posix-argv*) 2)
      (format t "No file specified.")
      (terse::main (elt sb-ext:*posix-argv* 1))))

(main)
