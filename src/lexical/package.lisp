(defpackage :burning-lexical
  (:use :common-lisp)
  (:export regular-to-tree
           ||
	   ?
	   ?repeat
	   repeat
	   make-lexeme
	   deflexeme

	   lexic
	   nullable
	   first-pos
	   last-pos

	   make-lexic
	   expression
	   follow-vector
	   value-vector
	   next-vector
	   
	   deflexic
	   create-state-machine
	   machine-values
	   machine-transitions))