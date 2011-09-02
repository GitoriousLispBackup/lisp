(in-package :burning-syntax-test)

(defun !symbol= (symbol1 symbol2)
  (cond
    ((eq symbol1 ':gensym) t)
    ((eq symbol2 ':gensym) t)
    (t (!equal symbol1 symbol2))))

(defun !production= (prod1 prod2)
  (!= (length prod1) (length prod2))
  (mapc #'!symbol= prod1 prod2))

(defun !productions= (prods1 prods2)
  (!= (length prods1) (length prods2))
  (mapc #'!production= prods1 prods2))

(defcase core-test)

(deftest core-test rule-to-productions-test ()
  (let ((rule (make-rule 'simple 'simple1 'simple2)))
    (!productions= (burning-syntax::rule-to-productions rule) 
		   '((simple simple1 simple2))))
  (let ((rule (make-rule 'simple '(:|| a b) '(:|| c d))))
    (!productions= (burning-syntax::rule-to-productions rule) 
		   '((simple a c) (simple a d) (simple b c) (simple b d))))
  (let ((rule (make-rule 'simple '(:* a b) '(:* c d))))
    (!productions= (burning-syntax::rule-to-productions rule) 
		   '((simple :gensym :gensym)
		     (:gensym)
		     (:gensym c d :gensym)
		     (:gensym)
		     (:gensym a b :gensym)))))

(deflexeme sample (:and (:char #\H) (:char #\i)))

(deftest core-test productions-terminals ()
  (!equal (burning-syntax::production-terminals '(x y z sample))
	  '(sample))
  (!equal (burning-syntax::production-terminals '(x sample y sample z sample bla-bla-bla))
	  '(sample)))

(deftest core-test production-non-terminals ()
  (!equal (burning-syntax::production-non-terminals '(x sample y sample x sample u sample z))
	  '(y x u z))
  (!error (burning-syntax::production-non-terminals '(sample sample))
	  "Production's result must be non-terminal."))

(deflexeme lexeme1 (:empty))
(deflexeme lexeme2 (:empty))
(deflexeme lexeme3 (:empty))
(deflexeme lexeme4 (:empty))
(deflexeme lexeme5 (:empty))

(deftest core-test grammar-making ()
  (let ((rule1 (make-rule 'rule1 'lexeme1 'lexeme2 'rule1 'lexeme1))
	(rule2 (make-rule 'rule2 '(:* rule1 lexeme1) 'lexeme3 '(:|| lexeme1 lexeme2))))
    (let ((grammar (make-grammar `(,rule1))))
      (!productions= (grammar-productions grammar) '((rule1 lexeme1 lexeme2 rule1 lexeme1)))
      (!equal (grammar-terminals grammar) '(:eps lexeme2 lexeme1))
      (!equal (grammar-non-terminals grammar) '(rule1)))
    (let ((grammar (make-grammar `(,rule1 ,rule2))))
      (!productions= (grammar-productions grammar)
		     '((rule1 lexeme1 lexeme2 rule1 lexeme1)
		       (rule2 :gensym lexeme3 lexeme1)
		       (rule2 :gensym lexeme3 lexeme2)
		       (:gensym)
		       (:gensym rule1 lexeme1 :gensym)))
      (!equal (grammar-terminals grammar) '(:eps lexeme3 lexeme2 lexeme1))
      (!production= (grammar-non-terminals grammar) '(rule2 rule1 :gensym)))))

(deflexeme a (:empty))
(deflexeme b (:empty))
(deflexeme c (:empty))
(deflexeme d (:empty))
(deflexeme e (:empty))

(deftest core-test nullable-test ()
  (let ((grammar (make-grammar `(,(make-rule 'rule1 'lexeme1)
				  ,(make-rule 'rule1 :eps)))))
    (!t (nullable-p 'rule1 grammar)))
  (let ((grammar (make-grammar `(,(make-rule 'x 'a)
				  ,(make-rule 'x 'y 'b)
				  ,(make-rule 'y 'c)
				  ,(make-rule 'y 'x 'd)))))
    (!eq (nullable-p 'x grammar) nil)
    (!eq (nullable-p 'y grammar) nil))
  (let ((grammar (make-grammar `(,(make-rule 'x 'y 'z)
				  ,(make-rule 'y 'x)
				  ,(make-rule 'x 'z)
				  ,(make-rule 'z :eps)))))
    (!t (nullable-p 'x grammar))
    (!t (nullable-p 'y grammar))
    (!t (nullable-p 'z grammar))))

(deftest core-test first-test ()
  (let ((grammar (make-grammar `(,(make-rule 'rule1 'rule1 'lexeme1)
				  ,(make-rule 'rule1 'lexeme2)
				  ,(make-rule 'rule1 :eps)))))
    (!equal (production-first '(rule1) grammar) '(lexeme1 lexeme2)))
  (let ((grammar (make-grammar `(,(make-rule 'x 'a)
				  ,(make-rule 'x 'y 'b)
				  ,(make-rule 'y 'c)
			       ,(make-rule 'y 'x 'd)))))
    (!equal (production-first '(x) grammar) '(a c))
    (!equal (production-first '(y) grammar) '(c a)))
  (let ((grammar (make-grammar `(,(make-rule 'x 'a)
				  ,(make-rule 'x 'y 'b)
				  ,(make-rule 'y 'z 'c)
				  ,(make-rule 'y 'd)
				  ,(make-rule 'z 'y 'e)))))
    (!equal (production-first '(x) grammar) '(a d))
    (!equal (production-first '(y) grammar) '(d))
    (!equal (production-first '(z) grammar) '(d)))
  (let ((grammar (make-grammar `(,(make-rule 'x 'a)
				  ,(make-rule 'x 'y 'b)
				  ,(make-rule 'y 'z)
				  ,(make-rule 'y 'd)
				  ,(make-rule 'z 'y 'e)
				  ,(make-rule 'z :eps)))))
    (!equal (production-first '(x) grammar) '(a e d b))
    (!equal (production-first '(y) grammar) '(e d))
    (!equal (production-first '(z) grammar) '(d e))))

(defgrammar my-grammar
    ((x :eps)
     (x a)
     (x (y b))
     (y c)
     (y (x d)))
  :start x)

(deftest core-test point-making ()
  (!equal (burning-syntax::make-point '(x a b c))
	  '((x a b c) (a b c) ()))
  (!equal (burning-syntax::make-points 'x my-grammar)
	  '(((x) () ()) ((x a) (a) ()) ((x y b) (y b) ()))))

(deftest core-test point-closure-test ()
  (!equal (burning-syntax::point-closure (burning-syntax::make-point '(s x)) my-grammar)
	  '(x y)))

(deftest core-test point-joining ()
  (!equal (burning-syntax::join-points (list (list '(x a b c) '(c) ())
					     (list '(x a b) '(b) ())
					     (list '(x a b) '(b) ())
					     (list '(x a b) '(a b) ())
					     (list '(x a b c) '(b c) ())))
	  '(((x a b) (a b) ())
	    ((x a b) (b) ())
	    ((x a b c) (b c) ())
	    ((x a b c) (c) ()))))

(deftest core-test point-goto ()
  (let ((point (burning-syntax::make-point '(s x))))
    (!equal (burning-syntax::point-goto point 'x)
	    '(((s x) () ())))))

(deftest core-test points-closure ()
  (let ((point1 (burning-syntax::make-point '(x)))
	(point2 (burning-syntax::make-point '(x y b))))
    (!equal (burning-syntax::points-closure `((,point1 ,point2) ()) my-grammar)
	    '((((x) () ())
	       ((x y b) (y b) ()))
	      (x y)))))

(deftest core-test points-goto ()
  (let ((points '((((x) () ())
		   ((x y b) (y b) ()))
		  (x y))))
    (!equal (burning-syntax::points-goto points 'x my-grammar)
	    '((((y x d) (d) ()))
	      ()))
    (!equal (burning-syntax::points-goto points 'y my-grammar)
	    '((((x y b) (b) ()))
	      ()))
    (!equal (burning-syntax::points-goto points 'a my-grammar)
	    '((((x a) () ()))
	      ()))
    (!equal (burning-syntax::points-goto points 'c my-grammar)
	    '((((y c) () ()))
	      ()))))

(deflexeme id (:empty))
(deflexeme + (:empty))
(deflexeme * (:empty))
(deflexeme ob (:empty))
(deflexeme cb (:empty))

(defgrammar expression-grammar
    ((x ((:|| (x + t) t)))
     (t ((:|| (t * f) f)))
     (f ((:|| ( ob x cb ) id))))
  :start x)


    
	  