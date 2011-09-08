(in-package :burning-xml)

(deflexeme open-bracket (:char #\<))
(deflexeme id (:positive (:or (:char #\_) (:letter))))
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

(deflexic xml-lexic 
    open-bracket 
  id 
  closing-bracket 
  spaces 
  eq 
  string 
  integer 
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
     (closing-tag (close-tag-open-bracket id closing-bracket))
     (short-tag (open-bracket id attributes tag-closing-bracket))
     (xml-nodes :eps)
     (xml-nodes (xml xml-nodes)))
  :start xml)

(defun test-xml ()
  (let ((xml-machine (create-state-machine xml-lexic))
	(output-file (open "/home/sopindm/models/report.xml.parsed" :direction :output :if-exists :overwrite
			   :if-does-not-exist :create))
	(parser (make-lr-parser (make-lr-table xml-grammar))))
    (with-open-file (stream "/home/sopindm/models/report.xml")
      (with-input-iterator (iterator stream)
	(write (parse-input iterator xml-machine parser) :stream output-file)
	(close output-file)))))