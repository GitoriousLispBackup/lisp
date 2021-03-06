(in-package :burning-syntax-test)

(defcase lr-parser-test)

(defun table-goto (state table)
  (aref (slot-value table 'burning-syntax::gotos) state))

(defun table-action (state table)
  (aref (slot-value table 'burning-syntax::actions) state))

(deftest lr-parser-test table-test ()
  (let ((table (make-lr-table pointer-grammar)))
    (!equal (table-goto 0 table) '((l . 7)(r . 6)(s . 5)))
    (!equal (table-goto 7 table) '())
    (!equal (table-goto 6 table) '())
    (!equal (table-goto 5 table) '())
    (!equal (table-goto 1 table) '((l . 4)(r . 3)))
    (!equal (table-goto 4 table) '())
    (!equal (table-goto 3 table) '())
    (!equal (table-goto 2 table) '())
    (!equal (table-goto 8 table) '((l . 4)(r . 9)))
    (!equal (table-goto 9 table) '()))
  (let ((table (make-lr-table my-grammar)))
    (!equal (table-goto 0 table) '((x . 5)(y . 3)))
    (!equal (table-goto 5 table) '())
    (!equal (table-goto 3 table) '())
    (!equal (table-goto 1 table) '())
    (!equal (table-goto 2 table) '())
    (!equal (table-goto 4 table) '())
    (!equal (table-goto 6 table) '()))
  (let ((table (make-lr-table expression-grammar)))
    (!equal (table-goto 0 table) '((f . 9)(x . 11)(t . 3)))
    (!equal (table-goto 11 table) '())
    (!equal (table-goto 3 table) '())
    (!equal (table-goto 1 table) '((f . 9)(x . 6)(t . 3)))
    (!equal (table-goto 6 table) '())
    (!equal (table-goto 2 table) '())
    (!equal (table-goto 4 table) '((f . 5)))
    (!equal (table-goto 7 table) '((f . 9)(t . 8)))
    (!equal (table-goto 8 table) '())
    (!equal (table-goto 5 table) '())
    (!equal (table-goto 9 table) '())
    (!equal (table-goto 10 table) '())))

(deflexeme a (:char #\a))
(defgrammar wrong-grammar1 
    ((s ((:|| x y)))
     (x a)
     (y a))
  :start s)

(defgrammar wrong-grammar2
    ((s ((:|| (a s a) a))))
  :start s)

(defun move-action (symbol state)
  `(,symbol burning-syntax::move . ,state))

(defun merge-action (symbol production)
  `(,symbol burning-syntax::merge ,@production))

(defun accept-action (production)
  `(:eps burning-syntax::accept ,@production))

(deftest lr-parser-test goto-test ()
  (let ((table (make-lr-table pointer-grammar)))
    (!equal (table-action 0 table) `(,(move-action 'id 2) ,(move-action '* 1)))
    (!equal (table-action 1 table) `(,(move-action 'id 2) ,(move-action '* 1)))
    (!equal (table-action 2 table) `(,(merge-action '= '(l id)) ,(merge-action ':eps '(l id))))
    (!equal (table-action 3 table) `(,(merge-action '= '(l * r)) ,(merge-action ':eps '(l * r))))
    (!equal (table-action 4 table) `(,(merge-action '= '(r l)) ,(merge-action ':eps '(r l))))
    (!equal (table-action 5 table) `(,(accept-action '(:start s))))
    (!equal (table-action 6 table) `(,(merge-action ':eps '(s r))))
    (!equal (table-action 7 table) `(,(move-action '= 8) ,(merge-action ':eps '(r l))))
    (!equal (table-action 8 table) `(,(move-action 'id 2) ,(move-action '* 1)))
    (!equal (table-action 9 table) `(,(merge-action ':eps '(s l = r)))))
  (let ((table (make-lr-table my-grammar)))
    (!equal (table-action 0 table) `(,(merge-action 'd '(x)) ,(move-action 'c 2) 
				      ,(move-action 'a 1) ,(merge-action ':eps '(x))))
    (!equal (table-action 1 table) `(,(merge-action 'd '(x a)) ,(merge-action ':eps '(x a))))
    (!equal (table-action 2 table) `(,(merge-action 'b '(y c))))
    (!equal (table-action 3 table) `(,(move-action 'b 4)))
    (!equal (table-action 4 table) `(,(merge-action 'd '(x y b)) ,(merge-action ':eps '(x y b))))
    (!equal (table-action 5 table) `(,(move-action 'd 6) ,(accept-action '(:start x))))
    (!equal (table-action 6 table) `(,(merge-action 'b '(y x d)))))
  (let ((table (make-lr-table expression-grammar)))
    (!equal (table-action 0 table) `(,(move-action 'id 2) ,(move-action 'ob 1)))
    (!equal (table-action 1 table) `(,(move-action 'id 2) ,(move-action 'ob 1)))
    (!equal (table-action 2 table) `(,(merge-action 'cb '(f id)) ,(merge-action '* '(f id))
				      ,(merge-action '+ '(f id)) ,(merge-action ':eps '(f id))))
    (!equal (table-action 3 table) `(,(merge-action 'cb '(x t)) ,(move-action '* 4) 
				      ,(merge-action '+ '(x t)) ,(merge-action ':eps '(x t))))
    (!equal (table-action 4 table) `(,(move-action 'id 2) ,(move-action 'ob 1)))
    (!equal (table-action 5 table) `(,(merge-action 'cb '(t t * f)) ,(merge-action '* '(t t * f))
				      ,(merge-action '+ '(t t * f)) ,(merge-action ':eps '(t t * f))))
    (!equal (table-action 6 table) `(,(move-action 'cb 10) ,(move-action '+ 7)))
    (!equal (table-action 7 table) `(,(move-action 'id 2) ,(move-action 'ob 1)))
    (!equal (table-action 8 table) `(,(merge-action 'cb '(x x + t)) ,(move-action '* 4)
				      ,(merge-action '+ '(x x + t)) ,(merge-action ':eps '(x x + t))))
    (!equal (table-action 9 table) `(,(merge-action 'cb '(t f)) ,(merge-action '* '(t f)) 
				      ,(merge-action '+ '(t f)) ,(merge-action ':eps '(t f))))
    (!equal (table-action 10 table) `(,(merge-action 'cb '(f ob x cb)) ,(merge-action '* '(f ob x cb))
				       ,(merge-action '+ '(f ob x cb)) ,(merge-action ':eps '(f ob x cb))))
    (!equal (table-action 11 table) `(,(move-action '+ 7) ,(accept-action '(:start x))))))

(deftest lr-parser-test pop-production ()
  (let ((parser (make-instance 'lr-parser)))
    (burning-syntax::push-state-to-parser 'a "a" parser)
    (burning-syntax::push-state-to-parser 'b "b" parser)
    (burning-syntax::push-state-to-parser 'c "c" parser)
    (!equal (burning-syntax::pop-production '(d a b c) parser)
	    '(d (a . "a") (b . "b") (c . "c")))))

(deftest lr-parser-test parse-test ()
  (let ((parser (make-lr-parser (make-lr-table expression-grammar))))
    (!eq (parser-next '(id . "id1") parser) nil)
    (!eq (parser-next '(* . "*") parser) nil)
    (!eq (parser-next '(ob . "(") parser) nil)
    (!eq (parser-next '(id . "id2") parser) nil)
    (!eq (parser-next '(+ . "+") parser) nil)
    (!eq (parser-next '(id . "id3") parser) nil)
    (!eq (parser-next '(* . "*") parser) nil)
    (!eq (parser-next '(id . "id4") parser) nil)
    (!eq (parser-next '(cb . ")") parser) nil)
    (!t (parser-next '(:eps . "") parser))
    (!equal (parser-value parser)
	    '(:start 
	      (x (t (t (f (id . "id1")))
		  (* . "*")
		  (f (ob . "(")
		     (x (x (t (f (id . "id2"))))
			(+ . "+")
			(t (t (f (id . "id3")))
			   (* . "*")
			   (f (id . "id4"))))
		     (cb . ")")))))))
  (let ((parser (make-lr-parser (make-lr-table pointer-grammar))))
    (!eq (parser-next '(* . "*") parser) nil)
    (!eq (parser-next '(* . "*") parser) nil)
    (!eq (parser-next '(* . "*") parser) nil)
    (!eq (parser-next '(id . "arg1") parser) nil)
    (!eq (parser-next '(= . "=") parser) nil)
    (!eq (parser-next '(* . "*") parser) nil)
    (!eq (parser-next '(* . "*") parser) nil)
    (!eq (parser-next '(* . "*") parser) nil)
    (!eq (parser-next '(id . "arg2") parser) nil)
    (!t (parser-next '(:eps . "") parser))
    (!equal (parser-value parser)
	    '(:start
	      (s (l (* . "*") (r (l (* . "*") (r (l (* . "*") (r (l (id . "arg1"))))))))
	       (= . "=")
	       (r (l (* . "*") (r (l (* . "*") (r (l (* . "*") (r (l (id . "arg2")))))))))))))
  (let ((parser (make-lr-parser (make-lr-table my-grammar))))
    (!eq (parser-next '(a . "a") parser) nil)
    (!eq (parser-next '(d . "d") parser) nil)
    (!eq (parser-next '(b . "b") parser) nil)
    (!t (parser-next '(:eps . "") parser))
    (!equal (parser-value parser)
	    '(:start 
	      (x (y (x (a . "a")) (d . "d")) (b . "b"))))
    (!eq (parser-next '(c . "c") parser) nil)
    (!eq (parser-next '(b . "b") parser) nil)
    (!t (parser-next '(:eps . "") parser))
    (!equal (parser-value parser)
	    '(:start
	      (x (y (c . "c")) (b . "b"))))
    (!t (parser-next '(:eps . "") parser))
    (!equal (parser-value parser)
	    '(:start
	      (x)))))
		  
(deflexic my-lexic
    a b c d)