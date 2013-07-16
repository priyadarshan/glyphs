;;;; glyphs.lisp

(in-package #:glyphs)

;;; "glyphs" goes here. Hacks and glory await!

(defmacro ƒ (name &rest rest)
  "Similar to defun, requires using x as the default case"
  `(defun ,name (&optional glyphs:x)
     (let ((glyphs:x (or glyphs:x t)))
       (cond 
	 ,@(loop for arg in rest
	      for iter from 0
	      when (and (symbolp arg) (string= '→ arg))
	      collect `(,(if (consp (nth (1- iter) rest))
			     `,(nth (1- iter) rest)
			      `(equal glyphs:x ,(nth (1- iter) rest)))
			,(nth (1+ iter) rest)))))))

(defmacro λ (&rest rest)
  "Similar to lambda, requires using x as the default case"
  `(lambda (&optional glyphs:x)
       (cond 
	 ,@(loop for arg in rest
	      for iter from 0
	      when (and (symbolp arg) (string= '→ arg))
	      collect `(,(if (consp (nth (1- iter) rest))
			     `,(nth (1- iter) rest)
			      `(equal glyphs:x ,(nth (1- iter) rest)))
			,(nth (1+ iter) rest))))))

(defmacro gregex (reg)
  "Easily find regex matches in a string"
  `(progn (defparameter glyphs:*gregex* ,(car reg))
  (cl-ppcre:scan glyphs:*gregex* glyphs:x)))

(set-macro-character
 #\/
 (lambda (stream char)
   (declare (ignore char))
   (let* ((reglist (read-delimited-list #\/ stream t)))
     `(gregex ,reglist))))

(defmacro gregex-replace (reg)
  "Easily run some regex replacements"
  `(cl-ppcre:regex-replace-all glyphs:*gregex* glyphs:x ,(car reg)))

(set-macro-character
 #\|
 (lambda (stream char)
   (declare (ignore char))
   (let* ((replace-list (read-delimited-list #\| stream t)))
     `(gregex-replace ,replace-list))))
