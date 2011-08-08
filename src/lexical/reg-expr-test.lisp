(in-package :burning-lexical-test)

(defcase regexpr-test)

(deftest regexpr-test simple-tree ()
  (!equal (burning-lexical::regular-to-tree '(#\a)) '(#\a)))

(deftest regexpr-test simple-list-tree ()
  (!equal (burning-lexical::regular-to-tree '(#\a #\b #\c))
          '(and (#\a) (and (#\b) (#\c)))))

(deftest regexpr-test empty-string-tree ()
  (!equal (burning-lexical::regular-to-tree '("")) ()))

(deftest regexpr-test simple-string-tree ()
  (!equal (burning-lexical::regular-to-tree '("ab"))
          '(and (#\a) (#\b))))

(deftest regexpr-test two-strings-tree ()
  (!equal (burning-lexical::regular-to-tree '("ab" "c"))
          '(and (and (#\a) (#\b)) (#\c))))

(deftest regexpr-test star-tree ()
  (!equal (burning-lexical::regular-to-tree '((* "a") "b"))
          '(and (burning-lexical::star (#\a)) (#\b)))
  (!equal (burning-lexical::regular-to-tree '((* "ab" "c")))
          '(burning-lexical::star (and (and (#\a) (#\b)) (#\c)))))

(deftest regexpr-test positive-tree ()
  (!equal (burning-lexical::regular-to-tree '((+ "ab")))
          '(and (and (#\a) (#\b)) (burning-lexical::star (and (#\a) (#\b))))))

(deftest regexpr-test or-tree ()
  (!equal (burning-lexical::regular-to-tree '((|| "a" "b")))
          '(or (#\a) (#\b)))
  (!equal (burning-lexical::regular-to-tree '((|| "ab" "c")))
          '(or (and (#\a) (#\b)) (#\c)))
  (!equal (burning-lexical::regular-to-tree '((|| "a" "b" (* "c"))))
          '(or (#\a) (or (#\b) (burning-lexical::star (#\c))))))

(deftest regexpr-test maybe-tree ()
  (!equal (burning-lexical::regular-to-tree '((? "ab" "c")))
          '(or () (and (and (#\a) (#\b)) (#\c)))))

(deftest regexpr-test ?repeat-tree ()
  (!equal (burning-lexical::regular-to-tree '((?repeat ("a") 2)))
          '(and (or () (#\a)) (or () (#\a)))))

(deftest regexpr-test repeat-tree ()
  (!equal (burning-lexical::regular-to-tree '((repeat ("ab") 2)))
	  '(and (and (#\a) (#\b)) (and (#\a) (#\b))))
  (!equal (burning-lexical::regular-to-tree '((repeat ("a") 1 2)))
          '(and (#\a) (or () (#\a)))))

(deftest regexpr-test range-tree ()
  (!equal (burning-lexical::regular-to-tree '((- "a" "d")))
	  '(or (#\a) (or (#\b) (or (#\c) (#\d))))))

(deftest regexpr-test lexeme-making ()
  (!equal (make-lexeme 'sample '("abc"))
	  '(and (and (#\a) (and (#\b) (#\c))) (burning-lexical::final sample))))