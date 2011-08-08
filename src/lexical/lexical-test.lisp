(in-package :burning-lexical-test)

(defcase lexical-test)

(deftest lexical-test lexical-making ()
  (let* ((lexeme1 (make-lexeme 'sample1 '("s1")))
	 (lexeme2 (make-lexeme 'sample2 '("s2")))
	 (lexic (make-lexic lexeme1 lexeme2)))
    (!equalp (expression lexic) '(or (and (and (#\s 0) (#\1 1)) (burning-lexical::final sample1 2))
				     (and (and (#\s 3) (#\2 4)) (burning-lexical::final sample2 5))))
    (!equalp (follow-vector lexic) (make-array 6))
    (!equalp (value-vector lexic) (make-array 6))
    (!equalp (next-vector lexic) (make-array 6))))

(deftest lexical-test deflexeme-test ()
  (!equal (macroexpand-1 '(deflexeme if ("if")))
	  '(defparameter if (make-lexeme 'if '("if")))))

(deftest lexical-test nullable-test ()
  (!eq (nullable (regular-to-tree '((- "a" "d")))) nil)
  (!eq (nullable (regular-to-tree '((* "a")))) t)
  (!eq (nullable (regular-to-tree '((? "ab")))) t)
  (!eq (nullable (regular-to-tree '((* "ab") (? "cd") "ef"))) nil)
  (!eq (nullable (regular-to-tree '((* "ab") (? "cd") (|| "ef" "gh" (? "ij"))))) t)
  (!eq (nullable '(burning-lexical::final end)) nil))

(deflexeme if ("aba"))
(deflexeme then ("abbab"))
(deflexeme word ((+ (- "a" "b"))))
(deflexeme integer ((? (|| "-" "+")) (+ (- "0" "1"))))
(deflexeme spaces ((* " ")))

(deftest lexical-test first-test ()
  (!equal (first-pos (expression (make-lexic if))) '(0))
  (!equal (first-pos (expression (make-lexic word))) 
          '(0 1))
  (!equal (first-pos (expression (make-lexic integer)))
	  '(0 1 2 3))
  (!equal (first-pos (expression (make-lexic spaces)))
	  '(0 1)))

(deftest lexical-test last-test ()
  (!equal (last-pos (expression (make-lexic if))) '(3))
  (!equal (last-pos (expression (make-lexic (second word))))
	  '(0 1 2 3))
  (!equal (last-pos (expression (make-lexic (second integer))))
	  '(2 3 4 5)))

(deflexic sample-lexic (make-lexeme 'sample '((* (|| "a" "b")) "abb")))
(deflexic my-lexic if then word integer spaces)

(deftest lexical-test follow-test ()
  (!equalp (follow-vector sample-lexic)
	   #((0 1 2) (0 1 2) (3) (4) (5) ())))

(deftest lexical-test lexic-generation ()
  (!equalp (expression sample-lexic) '(and (and (burning-lexical::star (or (#\a 0) (#\b 1))) 
					        (and (#\a 2) (and (#\b 3) (#\b 4))))
				           (burning-lexical::final sample 5)))
  (!equalp (follow-vector sample-lexic) #((0 1 2) (0 1 2) (3) (4) (5) ()))
  (!equalp (value-vector sample-lexic) #(nil nil nil nil nil sample))
  (!equalp (next-vector sample-lexic) #(#\a #\b #\a #\b #\b nil)))

(deftest lexical-test state-machine-creating ()
  (let ((machine (create-state-machine sample-lexic)))
    (!equalp (machine-values machine)
	     #(nil nil nil sample))
    (!equalp (machine-transitions machine)
	     #(((#\a . 1) (#\b . 0))
	       ((#\a . 1) (#\b . 2))
	       ((#\a . 1) (#\b . 3))
	       ((#\a . 1) (#\b . 0))))))
