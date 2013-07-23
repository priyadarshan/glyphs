;; Glyphs - Reducing verbosity in Common Lisp
;; Copyright (C) 2013 Matthew Carter
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; glyphs.lisp

(in-package #:glyphs)

;;; "glyphs" goes here. Hacks and glory await!

(defmacro ƒ (name &rest rest)
  "Similar to defun, requires using x as the default case"
  `(defun ,name (&optional glyphs:α)
     (let ((glyphs:α (or glyphs:α t)))
       (cond 
	 ,@(loop for arg in rest
	      for iter from 0
	      when (and (symbolp arg) (string= '→ arg))
	      collect `(,(if (consp (nth (1- iter) rest))
			     `,(nth (1- iter) rest)
			      `(equal glyphs:α ,(nth (1- iter) rest)))
			,(nth (1+ iter) rest)))))))

(parenscript:defmacro+ps ƒƒ (name &rest rest)
  "PS - Similar to defun, requires using x as the default case"
  `(defun ,name (&optional glyphs:α)
     (cond 
       ,@(loop for arg in rest
	    for iter from 0
	    when (and (symbolp arg) (string= '→ arg))
	    collect `(,(if (consp (nth (1- iter) rest))
			   `,(nth (1- iter) rest)
			   `(equal glyphs:α ,(nth (1- iter) rest)))
		       ,(nth (1+ iter) rest))))))

(defmacro λ (&rest rest)
  "Similar to lambda, requires using x as the default case"
  `(lambda (&optional glyphs:α)
     (cond 
       ,@(loop for arg in rest
	    for iter from 0
	    when (and (symbolp arg) (string= '→ arg))
	    collect `(,(if (consp (nth (1- iter) rest))
			   `,(nth (1- iter) rest)
			   `(equal glyphs:α ,(nth (1- iter) rest)))
		       ,(nth (1+ iter) rest))))))

(parenscript:defmacro+ps λλ (&rest rest)
  "PS - Similar to lambda, requires using x as the default case"
  `(lambda (&optional glyphs:α)
     (cond 
       ,@(loop for arg in rest
	    for iter from 0
	    when (and (symbolp arg) (string= '→ arg))
	    collect `(,(if (consp (nth (1- iter) rest))
			   `,(nth (1- iter) rest)
			   `(equal glyphs:α ,(nth (1- iter) rest)))
		       ,(nth (1+ iter) rest))))))

(defmacro gregex (reg)
  "Easily find regex matches in a string"
  `(progn (defparameter glyphs:*ψ* ,(car reg))
	  (cl-ppcre:scan glyphs:*ψ* glyphs:α)))

(set-macro-character
 #\~
 (lambda (stream char)
   (declare (ignore char))
   (let* ((reglist (read-delimited-list #\~ stream t)))
     `(gregex ,reglist))))

(defmacro gregex-replace (reg)
  "Easily run some regex replacements"
  `(cl-ppcre:regex-replace-all glyphs:*ψ* glyphs:α ,(car reg)))

(set-macro-character
 #\|
 (lambda (stream char)
   (declare (ignore char))
   (let* ((replace-list (read-delimited-list #\| stream t)))
     `(gregex-replace ,replace-list))))
