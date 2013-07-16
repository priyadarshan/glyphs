;;;; glyphs.lisp

(in-package #:glyphs)

;;; "glyphs" goes here. Hacks and glory await!

(defmacro ƒ (name &rest rest)
  "Similar to defun, requires using x as the default case"
  `(defun ,name (&optional x y z)
     (let ((x (or x t)))
       (cond 
	 ,@(loop for arg in rest
	      for iter from 0
	      when (and (symbolp arg) (equal '→ (intern (string arg))))
	      collect `(,(if (consp (nth (- iter 1) rest))
			     `,(nth (- iter 1) rest)
			      `(equal x ,(nth (- iter 1) rest)))
			,(nth (+ iter 1) rest)))))))

(defmacro λ (&rest rest)
  "Similar to lambda, requires using x as the default case"
  `(lambda (&optional x y z)
       (cond 
	 ,@(loop for arg in rest
	      for iter from 0
	      when (and (symbolp arg) (equal '→ (intern (string arg))))
	      collect `(,(if (consp (nth (- iter 1) rest))
			     `,(nth (- iter 1) rest)
			      `(equal x ,(nth (- iter 1) rest)))
			,(nth (+ iter 1) rest))))))

(defmacro gregex (reg)
  "Easily find regex matches in a string"
  `(progn (defparameter *regex* ,(car reg))
  (cl-ppcre:scan *regex* x)))

(set-macro-character
 #\/
 (lambda (stream char)
   (declare (ignore char))
   (let* ((reglist (read-delimited-list #\/ stream t)))
     `(gregex ,reglist))))

(defmacro gregex-replace (reg)
  "Easily run some regex replacements"
  `(cl-ppcre:regex-replace-all *regex* x ,(car reg)))

(set-macro-character
 #\|
 (lambda (stream char)
   (declare (ignore char))
   (let* ((replace-list (read-delimited-list #\| stream t)))
     `(gregex-replace ,replace-list))))
