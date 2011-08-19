(in-package :burning-lexical-test)

(defcase lexical-test)

(deflexeme if ("if"))
(deflexeme then ("then"))
(deflexeme word ((+ (- "a" "z"))))
(deflexeme integer ((? (|| "-" "+")) (+ (- "0" "9"))))
(deflexeme spaces ((* " ")))
(deflexeme r-word ((+ (- "а" "я"))))

(deflexic sample-lexic (make-lexeme 'sample '((* (- "a" "b")) "abb")))
(deflexic my-lexic if then word integer spaces r-word)

(deftest lexical-test state-machine-creating ()
  (let ((machine (create-state-machine sample-lexic)))
    (!equalp (slot-value machine 'values)
	     #(nil nil nil sample))
    (!equalp (slot-value machine 'transitions)
	     #(((0 . 1) (1 . 0))
	       ((0 . 1) (1 . 2))
	       ((0 . 1) (1 . 3))
	       ((0 . 1) (1 . 0))))))

(deftest lexical-test state-machine-value ()
  (let ((machine (create-state-machine my-lexic)))
    (!eq (machine-value machine "if") 'if)
    (!eq (machine-value machine "then") 'then)
    (!eq (machine-value machine "abbasgheqtaba") 'word)
    (!eq (machine-value machine "213548906") 'integer)
    (!eq (machine-value machine "+11412341230") 'integer)
    (!eq (machine-value machine "-110111235123451451450") 'integer)
    (!eq (machine-value machine "анализатор") 'r-word)
    (!eq (machine-value machine "") 'spaces)
    (!eq (machine-value machine "    ") 'spaces)))
