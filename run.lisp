#!/usr/bin/sbcl --script

(require 'asdf)
(require 'terse)

(defun main ()
  (terse::main (elt sb-ext:*posix-argv* 1)))

(main)
