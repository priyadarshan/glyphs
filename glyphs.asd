;;;; glyphs.asd

(asdf:defsystem #:glyphs
  :serial t
  :description "Glyphs to reduce Common Lisp verbosity"
  :author "Matthew Carter <m@ahungry.com>"
  :license "todo"
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "glyphs")))

