glyphs
======

A little experiment in reducing verbosity in Common Lisp, inspired by
BODOL (https://github.com/bodil/BODOL - no affiliation).

To try it out in your REPL you can use (ql:quickload :glyphs)
if you have added to your ASDF load path in local projects.

# Examples

## Factorial example with glyphs function macro:

```lisp
(ƒ factorial
   0 → 1
   x → (* x (factorial (- x 1))))
```

Is equivalent to:
```lisp
(defun factorial (x)
  (cond ((equal x 0) 1)
	(x (* x (factorial (- x 1))))))
```

## Basic map with glyphs lambda macro to compare strings

```lisp
(mapcar (λ "cat" → (print "Cats rock")
           "dog" → (print "Dogs do too!")) '("cat" "dog" "mouse"))
```

Is equivalent to:
```lisp
(mapcar (lambda (x)
              (cond ((equal x "cat") (print "Cats rock"))
                    ((equal x "dog") (print "Dogs do too!")) '("cat" "dog" "mouse"))))

# To come...

I plan to add additional features to the glyph macros so that calls such as:
```lisp
(ƒ double-odds-half-evens
   oddp → (* x 2)
   evenp → (/ x 2))
```
will be functional, as well as some sort of shorthand for regex matching/replacing.

# Currently used glyphs and bindings for them (more to come)

## Emacs bindings

Add to `.emacs'

```lisp
;; Keybindings for glyphs
(global-set-key (kbd "M-l") (lambda () (interactive) (insert "\u03bb"))) ; λ lambda
(global-set-key (kbd "M-f") (lambda () (interactive) (insert "\u0192"))) ; ƒ function
(global-set-key (kbd "M--") (lambda () (interactive) (insert "\u2192"))) ; → right arrow
```

## Vim bindings

Add to `.vimrc'

```vim
" Keybindings for glyphs
:inoremap <A-l> <C-v>u3bb<Space>   ; λ lambda 
:inoremap <A-f> <C-v>u192<Space>   ; ƒ function
:inoremap <A--> <C-v>u2192<Space>  ; → right arrow
```

## Mac OS X keybindings

Add to `~/Library/KeyBindings/DefaultKeyBinding.dict'

```
{
"~l" = ("insertText:", "\U03BB"); /* alt + l ~> λ lambda */
"~f" = ("insertText:", "\U0192"); /* alt + f ~> ƒ function */
"~-" = ("insertText:", "\U2192"); /* alt + - ~> → right arrow */
}
```

# License

See LICENSE.md
