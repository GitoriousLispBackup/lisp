(defpackage :burning-syntax
  (:use :common-lisp :burning-lexical)
  (:export make-rule
	   rule-productions
	   rule-result
	   aux-rules

	   make-grammar
	   grammar-productions
	   grammar-terminals
	   grammar-non-terminals))