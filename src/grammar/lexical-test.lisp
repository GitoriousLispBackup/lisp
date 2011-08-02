(asdf:oos 'asdf:load-op 'burning-grammar)
(asdf:oos 'asdf:load-op 'burning-testing)

(use-package :burning-testing)

(defcase lexical-test)

(deftest lexical-test simple-tree ()
  (!equal (regular-to-tree '(#\a)) '(#\a)))

(deftest lexical-test simple-list-tree ()
  (!equal (regular-to-tree '(#\a #\b #\c))
          '(and (#\a) (and (#\b) (#\c)))))

(deftest lexical-test empty-string-tree ()
  (!equal (regular-to-tree '("")) ()))

(deftest lexical-test simple-string-tree ()
  (!equal (regular-to-tree '("ab"))
          '(and (#\a) (#\b))))

(deftest lexical-test two-strings-tree ()
  (!equal (regular-to-tree '("ab" "c"))
          '(and (and (#\a) (#\b)) (#\c))))

(deftest lexical-test star-tree ()
  (!equal (regular-to-tree '((* "a") "b"))
          '(and (star (#\a)) (#\b)))
  (!equal (regular-to-tree '((* "ab" "c")))
          '(star (and (and (#\a) (#\b)) (#\c)))))

(deftest lexical-test positive-tree ()
  (!equal (regular-to-tree '((+ "ab")))
          '(and (and (#\a) (#\b)) (star (and (#\a) (#\b))))))

(deftest lexical-test or-tree ()
  (!equal (regular-to-tree '((|| "a" "b")))
          '(or (#\a) (#\b)))
  (!equal (regular-to-tree '((|| "ab" "c")))
          '(or (and (#\a) (#\b)) (#\c)))
  (!equal (regular-to-tree '((|| "a" "b" (* "c"))))
          '(or (#\a) (or (#\b) (star (#\c))))))

(deftest lexical-test maybe-tree ()
  (!equal (regular-to-tree '((? "ab" "c")))
          '(or () (and (and (#\a) (#\b)) (#\c)))))

(deftest lexical-test ?repeat-tree ()
  (!equal (regular-to-tree '((?repeat ("a") 2)))
          '(and (or () (#\a)) (or () (#\a)))))

(deftest lexical-test repeat-tree ()
  (!equal (regular-to-tree '((repeat ("ab") 2)))
	  '(and (and (#\a) (#\b)) (and (#\a) (#\b))))
  (!equal (regular-to-tree '((repeat ("a") 1 2)))
          '(and (#\a) (or () (#\a)))))

(deftest lexical-test range-tree ()
  (!equal (regular-to-tree '((- "a" "d")))
	  '(or (#\a) (or (#\b) (or (#\c) (#\d))))))

(deftest lexical-test lexeme-making ()
  (!equal (make-lexeme 'sample '("abc"))
	  '(and (and (#\a) (and (#\b) (#\c))) (final sample))))

(deftest lexical-test simple-positions-adding ()
  (!equal (add-positions (make-lexeme 'sample '("a")) (integer-generator 0))
	  '(and (#\a 0) (final sample 1))))

(deftest lexical-test complex-positions-adding ()
  (!equal (add-positions (make-lexeme 'sample '((* "a") "bc")) (integer-generator 0))
	  '(and (and (star (#\a 0)) (and (#\b 1) (#\c 2))) (final sample 3))))

(deftest lexical-test lexical-making ()
  (let ((lexeme1 (make-lexeme 'sample1 '("s1")))
	(lexeme2 (make-lexeme 'sample2 '("s2"))))
    (!equalp (make-lexic lexeme1 lexeme2)
	     (make-instance 'lexic 
			    :expression '(or (and (and (#\s 0) (#\1 1)) (final sample1 2))
					  (and (and (#\s 3) (#\2 4)) (final sample2 5)))
			    :follow-vector (make-array 6)))))

(deftest lexical-test deflexeme-test ()
  (!equal (macroexpand-1 '(deflexeme if ("if")))
	  '(defparameter if (make-lexeme 'if '("if")))))

(deftest lexical-test nullable-test ()
  (!eq (nullable (regular-to-tree '((- "a" "d")))) nil)
  (!eq (nullable (regular-to-tree '((* "a")))) t)
  (!eq (nullable (regular-to-tree '((? "ab")))) t)
  (!eq (nullable (regular-to-tree '((* "ab") (? "cd") "ef"))) nil)
  (!eq (nullable (regular-to-tree '((* "ab") (? "cd") (|| "ef" "gh" (? "ij"))))) t))

(deflexeme if ("if"))
(deflexeme then ("then"))
(deflexeme word ((+ (- "a" "z"))))
(deflexeme integer ((? (|| "-" "+")) (+ (- "0" "9"))))

(deftest lexical-test first-test ()
  (!= (first-pos (make-lexical if)) '(0))
  (!= (first-pos (make-lexical then)) '(0))
  (!= (first-pos (make-lexical word)) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 
				    14 15 16 17 18 19 20 21 22 23 24 25)))