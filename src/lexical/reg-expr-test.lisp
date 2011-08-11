(in-package :burning-lexical-test)

(defcase regexpr-test)

(deftest regexpr-test simple-tree ()
  (!equal (burning-lexical::regular-to-tree '(#\a)) '(#\a #\a)))

(deftest regexpr-test simple-list-tree ()
  (!equal (burning-lexical::regular-to-tree '(#\a #\b #\c))
          '(and (#\a #\a) (and (#\b #\b) (#\c #\c)))))

(deftest regexpr-test empty-string-tree ()
  (!equal (burning-lexical::regular-to-tree '("")) ()))

(deftest regexpr-test simple-string-tree ()
  (!equal (burning-lexical::regular-to-tree '("ab"))
          '(and (#\a #\a) (#\b #\b))))

(deftest regexpr-test two-strings-tree ()
  (!equal (burning-lexical::regular-to-tree '("ab" "c"))
          '(and (and (#\a #\a) (#\b #\b)) (#\c #\c))))

(deftest regexpr-test star-tree ()
  (!equal (burning-lexical::regular-to-tree '((* "a") "b"))
          '(and (burning-lexical::star (#\a #\a)) (#\b #\b)))
  (!equal (burning-lexical::regular-to-tree '((* "ab" "c")))
          '(burning-lexical::star (and (and (#\a #\a) (#\b #\b)) (#\c #\c)))))

(deftest regexpr-test positive-tree ()
  (!equal (burning-lexical::regular-to-tree '((+ "ab")))
          '(and (and (#\a #\a) (#\b #\b)) (burning-lexical::star (and (#\a #\a) (#\b #\b))))))

(deftest regexpr-test or-tree ()
  (!equal (burning-lexical::regular-to-tree '((|| "a" "b")))
          '(or (#\a #\a) (#\b #\b)))
  (!equal (burning-lexical::regular-to-tree '((|| "ab" "c")))
          '(or (and (#\a #\a) (#\b #\b)) (#\c #\c)))
  (!equal (burning-lexical::regular-to-tree '((|| "a" "b" (* "c"))))
          '(or (#\a #\a) (or (#\b #\b) (burning-lexical::star (#\c #\c))))))

(deftest regexpr-test maybe-tree ()
  (!equal (burning-lexical::regular-to-tree '((? "ab" "c")))
          '(or () (and (and (#\a #\a) (#\b #\b)) (#\c #\c)))))

(deftest regexpr-test ?repeat-tree ()
  (!equal (burning-lexical::regular-to-tree '((?repeat ("a") 2)))
          '(and (or () (#\a #\a)) (or () (#\a #\a)))))

(deftest regexpr-test repeat-tree ()
  (!equal (burning-lexical::regular-to-tree '((repeat ("ab") 2)))
	  '(and (and (#\a #\a) (#\b #\b)) (and (#\a #\a) (#\b #\b))))
  (!equal (burning-lexical::regular-to-tree '((repeat ("a") 1 2)))
          '(and (#\a #\a) (or () (#\a #\a)))))

(deftest regexpr-test range-tree ()
  (!equal (burning-lexical::regular-to-tree '((- "a" "d")))
	  '(#\a #\d)))

(deftest regexpr-test lexeme-making ()
  (!equal (make-lexeme 'sample '("abc"))
	  '(and (and (#\a #\a) (and (#\b #\b) (#\c #\c))) (burning-lexical::final sample))))