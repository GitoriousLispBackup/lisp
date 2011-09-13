(in-package :burning-xml)

(deflexeme open-bracket (:char #\<))
(deflexeme id (:and (:or (:char #\_) (:letter))
		    (:star (:or (:char #\_) (:or (:letter) (:digit))))))
(deflexeme closing-bracket (:char #\>))
(deflexeme tag-closing-bracket (:and (:char #\/) (:char #\>)))
(deflexeme close-tag-open-bracket (:and (:char #\<) (:char #\/)))
(deflexeme spaces (:positive (:space)) :skipped t)
(deflexeme eq (:char #\=))
(deflexeme string (:and (:char #\")
			(:and (:star (:any))
			      (:char #\"))) :minimal t)

(deflexeme integer (:and (:maybe (:or (:char #\-)
				      (:char #\+)))
			 (:positive (:digit))))

(deflexeme float (:and (:maybe (:or (:char #\-)
				    (:char #\+)))
		       (:or (:and (:char #\n)
				  (:and (:char #\a)
					(:char #\n)))
			    (:and (:and (:positive (:digit))
					(:and (:char #\.)
					      (:star (:digit))))
				  (:maybe (:and (:or (:char #\e) (:char #\E))
						(:and (:maybe (:or (:char #\-) (:char #\+)))
						      (:positive (:digit)))))))))
				     

(deflexic xml-lexic 
    open-bracket 
  id 
  closing-bracket 
  spaces 
  eq 
  string 
  integer 
  float
  tag-closing-bracket 
  close-tag-open-bracket)

(defgrammar xml-grammar
    ((xml (open-tag xml-nodes closing-tag))
     (xml short-tag)
     (open-tag (open-bracket id attributes closing-bracket))
     (attributes :eps)
     (attributes (id eq attribute-value attributes))
     (attribute-value integer)
     (attribute-value string)
     (attribute-value float)
     (closing-tag (close-tag-open-bracket id closing-bracket))
     (short-tag (open-bracket id attributes tag-closing-bracket))
     (xml-nodes :eps)
     (xml-nodes (xml xml-nodes)))
  :start xml)

(defparameter xml-machine (create-state-machine xml-lexic))
(defparameter xml-parser (make-lr-parser (make-lr-table xml-grammar)))

