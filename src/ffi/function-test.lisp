(in-package :burning-ffi-test)

(defcase function-test)

(load-ffi '(:library "glib-2.0")
	  '(:library "simple" :path "~/lisp/src/ffi/c/")
	  '(:function ("int_sum" :int) ((a b :int)))
	  '(:function ("short_sum" :short) ((a b :short)))
	  '(:function ("long_sum" :long) ((a b :long)))
	  '(:function ("uint_sum" :uint) ((a b :uint)))
	  '(:function ("ushort_sum" :ushort) ((a b :ushort)))
	  '(:function ("ulong_sum" :ulong) ((a b :ulong)))
	  '(:function ("float_sum" :float) ((a b :float)))
	  '(:function ("double_sum" :double) ((a b :double)))
	  '(:function ("bool_and" :bool) ((a b :bool)))
	  '(:function ("sum_generated" :int) ((generator (:function :int)))))

(deftest function-test wrong-type-test ()
  (!error (load-ffi '(:function ("bla-bla" :very-wrong-type) ())) "Wrong ffi type - VERY-WRONG-TYPE."))

(deftest function-test sum-test ()
  (!= (int-sum -2 -2) -4)
  (!= (short-sum -2048 -1024) -3072)
  (!= (long-sum -123 -234) -357)
  (!= (uint-sum 123 234) 357)
  (!= (ushort-sum 1024 2048) 3072)
  (!= (ulong-sum 100000 200000) 300000)
  (!= (float-sum 1.23 3.21) 4.44)
  (!= (double-sum 1.2345d0 5.4321d0) 6.6666d0)
  (!t (bool-and t t))
  (!null (bool-and t nil))
  (!null (bool-and nil t))
  (!null (bool-and nil nil)))
  
