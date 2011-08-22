(defpackage :burning-lexical
  (:use :common-lisp)
  (:export make-iterator
	   eof-p
	   with-input-iterator
	   get-next
	   commit
	   reset

	   character-node
	   range-node
	   final-node
	   empty-node
	   star-node
	   and-node
	   or-node
	   node=
	   
	   eval-regular
	   :char
           
           regular-to-tree
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
	   translation
	   follow-vector
	   value-vector
	   next-vector
	   
	   deflexic
	   create-state-machine
	   values
	   transitions
	   machine-value
	   get-token
	   print-stream))