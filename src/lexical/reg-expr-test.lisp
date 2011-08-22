(in-package :burning-lexical-test)

(defcase regexpr-test)

(deftest regexpr-test simple-tree ()
  (!node= (burning-lexical::regular-to-tree '(#\a)) (character-node #\a)))

(deftest regexpr-test simple-list-tree ()
  (!node= (burning-lexical::regular-to-tree '(#\a #\b #\c))
	  (and-node (character-node #\a) (and-node (character-node #\b) (character-node #\c)))))

(deftest regexpr-test empty-string-tree ()
  (!equal (burning-lexical::regular-to-tree '("")) ()))

(deftest regexpr-test simple-string-tree ()
  (!node= (burning-lexical::regular-to-tree '("ab"))
          (and-node (character-node #\a) (character-node #\b))))

(deftest regexpr-test two-strings-tree ()
  (!node= (burning-lexical::regular-to-tree '("ab" "c"))
          (and-node (and-node (character-node #\a) (character-node #\b)) (character-node #\c))))

(deftest regexpr-test star-tree ()
  (!node= (burning-lexical::regular-to-tree '((* "a") "b"))
          (and-node (star-node (character-node #\a)) (character-node #\b)))
  (!node= (burning-lexical::regular-to-tree '((* "ab" "c")))
          (star-node (and-node (and-node (character-node #\a) (character-node #\b)) (character-node #\c)))))

(deftest regexpr-test positive-tree ()
  (!node= (burning-lexical::regular-to-tree '((+ "ab")))
          (and-node (and-node (character-node #\a) (character-node #\b)) 
		    (star-node (and-node (character-node #\a) (character-node #\b))))))

(deftest regexpr-test or-tree ()
  (!node= (burning-lexical::regular-to-tree '((|| "a" "b")))
          (or-node (character-node #\a) (character-node #\b)))
  (!node= (burning-lexical::regular-to-tree '((|| "ab" "c")))
          (or-node (and-node (character-node #\a) (character-node #\b)) (character-node #\c)))
  (!node= (burning-lexical::regular-to-tree '((|| "a" "b" (* "c"))))
          (or-node (character-node #\a) (or-node (character-node #\b) (star-node (character-node #\c))))))

(deftest regexpr-test maybe-tree ()
  (!node= (burning-lexical::regular-to-tree '((? "ab" "c")))
          (or-node (empty-node) (and-node (and-node (character-node #\a) 
						    (character-node #\b)) (character-node #\c )))))

(deftest regexpr-test ?repeat-tree ()
  (!node= (burning-lexical::regular-to-tree '((?repeat ("a") 2)))
          (and-node (or-node (empty-node) (character-node #\a)) (or-node (empty-node) (character-node #\a)))))

(deftest regexpr-test repeat-tree ()
  (!node= (burning-lexical::regular-to-tree '((repeat ("ab") 2)))
	  (and-node (and-node (character-node #\a) (character-node #\b)) 
		    (and-node (character-node #\a) (character-node #\b))))
  (!node= (burning-lexical::regular-to-tree '((repeat ("a") 1 2)))
          (and-node (character-node #\a) (or-node (empty-node) (character-node #\a)))))

(deftest regexpr-test range-tree ()
  (!node= (burning-lexical::regular-to-tree '((- "a" "d")))
	  (range-node #\a #\d)))

(deftest regexpr-test lexeme-making ()
  (!node= (make-lexeme 'sample '("abc"))
	  (and-node (and-node (character-node #\a) (and-node (character-node #\b) (character-node #\c))) 
		    (final-node 'sample))))