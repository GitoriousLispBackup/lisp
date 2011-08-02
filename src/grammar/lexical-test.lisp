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