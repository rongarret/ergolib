
; Tweak CL-WHO so that symbols get output with HTML-RENDER and scripts are at end

(defun tree-to-template (tree)
  "Transforms an HTML tree into an intermediate format - mainly a
  flattened list of strings. Utility function used by TREE-TO-COMMANDS-AUX."
  (loop for element in tree
    if (or (keywordp element)
           (and (listp element)
                (keywordp (first element)))
           (and (listp element)
                (listp (first element))
                (keywordp (first (first element)))))
    ;; the syntax for a tag - process it
    nconc (cl-who::process-tag element #'tree-to-template)
    ;; list - insert as sexp
    else if (consp element)
    collect `(let ((cl-who::*indent* ,cl-who::*indent*))
               nil ;; If the element is (declare ...) it
               ;; won't be interpreted as a declaration and an
               ;; appropriate error could be signaled
               ,element)
    ;; Symbol, call HTML-RENDER
    else if (symbolp element)
    collect `(html-render ,element)
    ;; something else - insert verbatim
    else collect element))

(defun cl-who::tree-to-template (tree) (tree-to-template tree))

(defmethod who:convert-tag-to-string-list ((tag (eql :script)) attr-list body body-fn)
  (declare (ignorable attr-list body body-fn))
  `((%script ,(call-next-method))))

(defvar *scripts*)

(defmacro %script (forms)
  `(let ((s (with-output-to-string (*standard-output*)
              (htm ,@forms))))
     (if (boundp '*scripts*)
       (progn (pushnew s *scripts* :test 'equal) (princ " "))
       (princ s))))

(defmacro page1 (&body body)
  `(without-length-restrictions
    (let ((*scripts* nil))
      (with-html-output (*standard-output* nil :indent *indent* :prologue t)
        ,@body
        (for script in (reverse *scripts*) do (princ script))))
    (values)))

; Make ESC not barf on non-strings
(defun who:escape-string (string &key (test *escape-char-p*))
  (declare (optimize speed))
  "Escape all characters in STRING which pass TEST. This function is
not guaranteed to return a fresh string.  Note that you can pass NIL
for STRING which'll just be returned."
  (if (not (stringp string)) (setf string (->string string)))
  (let ((first-pos (position-if test string))
        (format-string (if (eq who::*html-mode* :xml) "&#x~x;" "&#~d;")))
    (if (not first-pos)
      ;; nothing to do, just return STRING
      string
      (with-output-to-string (s)
        (loop with len = (length string)
              for old-pos = 0 then (1+ pos)
              for pos = first-pos
                  then (position-if test string :start old-pos)
              ;; now the characters from OLD-POS to (excluding) POS
              ;; don't have to be escaped while the next character has to
              for char = (and pos (char string pos))
              while pos
              do (write-sequence string s :start old-pos :end pos)
                 (case char
                   ((#\<)
                     (write-sequence "&lt;" s))
                   ((#\>)
                     (write-sequence "&gt;" s))
                   ((#\&)
                     (write-sequence "&amp;" s))
                   ((#\')
                     (write-sequence "&#039;" s))
                   ((#\")
                     (write-sequence "&quot;" s))
                   (otherwise
                     (format s format-string (char-code char))))
              while (< (1+ pos) len)
              finally (unless pos
                        (write-sequence string s :start old-pos)))))))
