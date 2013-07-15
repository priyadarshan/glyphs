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
			,(nth (+ iter 1) rest)))))))))

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
			,(nth (+ iter 1) rest)))))))))

