
(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  PARCIL - A Parser for C syntax In Lisp
;;;  version 0.1b
;;;
;;;  copyright (c) 1992 by Erann Gat, all rights reserved
;;;  copyright (c) 2007, 2009 by Ron Garret, all rights reserved
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;  Revision history:
;;;  v0.1a - Initial release
;;;  Holy shit, has it really been fifteen years since I wrote this?
;;;  9/4/09 - Complete overhaul and integration with symbol reader macros
;;;

;;;  PARCIL is a parser for a subset of the syntax for the C programming
;;;  language.  It is a recursive descent parser which makes it
;;;  fairly brittle and difficult to modify.  However, it does make it fairly
;;;  fast, and it also allows the parser to deal with lots of C idiosyncrasies
;;;  which are difficult to implement in general-purpose parsers, e.g. operator
;;;  precedence, prefix and postfix operators, etc.
;;;
;;;  PARCIL supports all syntax defined in section 18.1 of the original Kernighan
;;;  and Ritchie book, plus all C numerical syntax including floats and radix
;;;  syntax (i.e. 0xnnn, 0bnnn, and 0onnn).  In addition, PARCIL supports multiple
;;;  array subscripts.  There is also a preliminary version of {} blocks, but it
;;;  doesn't quite do the right thing.  Parcil also allows strings to be delimited
;;;  using single quotes as well as double quotes (but you must use the same type
;;;  to close the string as you did to open it).

(eval-when (:compile-toplevel :load-toplevel :execute)

(require :globals)

(defmacro iterate (name args &rest body)
  `(labels ((,name ,(mapcar #'car args) ,@body))
     (,name ,@(mapcar #'cadr args))))

(defmacro aif (condition &optional (then nil then-p) &rest more)
  (if then-p
    `(let ((it ,condition)) (if it ,then ,(if more `(aif ,@more))))
    condition))
)

(defv $parcil-stream t)

;;;  The PARCIL tokenizer.  (FSA?  What's an FSA?)
;;;

(defun peek () (peek-char nil $parcil-stream nil nil))

(defun readc () (read-char-no-hang $parcil-stream nil nil))

(defun eof? ()
  (let ((c (readc)))
    (if c (progn (unread-char c  $parcil-stream) nil) t)))

(defun consume-whitespace ()
  (let ((c (readc)))
    (cond ((whitespacep c) (consume-whitespace))
          ((null c) nil)
          (t (unread-char c $parcil-stream)))))

(defun peekt ()
  (consume-whitespace)
  (and (listen $parcil-stream) (peek))) ;  (peek-char t *parcil-stream* nil nil)))

(defun letter? (c) (and c (alpha-char-p c)))

(defun constituent? (c) (or (letter? c) (eql c #\_)))

(defun digit? (c) (and c (digit-char-p c)))

(defun opchar? (c) (and c (not (or (digit? c) (constituent? c)))))

(defun ident? (thing)
  (and thing
       (symbolp thing)
       (constituent? (char (symbol-name thing) 0))))

; Infix operators in priority order
(defv $binary-ops
  '((\. -> \( \[) (* / %) (+ -) (<< >>) (< > <= >=) (== !=) (&) (^) (\|) (&&) (\|\|)
    (= += -= *= /= %= &= ^= \|= >>= <<=)))

;;; Any binary operator in this alist will be renamed in the parsed version.
(defv $binop-translations
  '((\. . struct-ref) (= . setf) (% . mod) (<< . ashl) (>> . ashr)
    (& . logand) (^ . logxor) (\| . logior) (&& . and) (\|\| . or)
    (\( . %infix-call) ([ . %infix-aref)))

(defmacro %infix-call (op args) (cons op args))
(defmacro %infix-aref (a indices) (list* 'aref a indices))

;;;  Any prefix unary operator included in this table will be renamed in the parsed
;;;  version.  (Postfix ++ and -- are handled specially, in PARSE-TERM.)
(defv $unary-op-translations
  '((* . deref) (& . address-of) (- . -) (! . not) (~ . lognot) (++ . incf) (-- . decf)))

(defun binop? (s) (member s $binary-ops :test #'member))

(defun op-priority (s)
  (aif (position s $binary-ops :test #'member) (- 40 it)))

(defun translate-binop (op) (or (cdr (assoc op $binop-translations)) op))

(defun translate-unary-op (op) (cdr (assoc op $unary-op-translations)))

(defun syntax-error (&rest args)
  (error "Infix syntax error: ~A" (apply 'format nil args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Tokenizer/lexer
;;;
(defun parse-fixnum (&optional (base 10))
  (iterate loop1 ( (n 0) )
    (if (eof?)
      n
      (let* ( (c (char-downcase (peek)))
              (d (cond ( (digit? c) (- (char-code c) (char-code #\0)) )
                       ( (letter? c) (- (char-code c) (char-code #\a) -10) )
                       (t nil))) )
        (cond ( (null d) n )
              ( (or (>= d base) (eql c #\_))
                (syntax-error "Unexpected character '~A' while parsing integer"
                              (peek)))
              (t (readc) (loop1 (+ d (* n base)))))))))

(defun parse-atom ()
  (consume-whitespace)
  (if (eof?)
    nil
    (let ( (c (peek)) )
      (cond ( (letter? c) (parse-symbol) )
            ( (eql c #\0)
	      (readc)
	      (if (letter? (peek))
		  (parse-radix-integer)
		(parse-number)) )
            ( (digit? c) (parse-number) )
            ( (or (eql c #\") (eql c #\')) (parse-string c) )
            ( (eql c #\«) (parse-balanced-string #\« #\») )
            ( (eql c #\“) (parse-balanced-string #\“ #\”) )
            (t (parse-operator))))))

(defun parse-symbol ()
  (intern
   (string-upcase
    (with-output-to-string (s)
      (loop while (let ( (c (peek)) ) (and c (or (letter? c) (digit? c) (eql c #\_))))
        do (princ (readc) s))
      s))))

(defun parse-radix-integer ()
  (parse-fixnum (ecase (readc) (#\x 16) (#\o 8) (#\b 2))))

(defun parse-number ()
  (let* ( (n1 (parse-fixnum))
          (c (peek)) )
    (prog ( (d 0.1) )
      (if (eql c #\.) (go decimal))
      (if (or (eql c #\e) (eql c #\E)) (go expt))
      (return n1)
    decimal
      (readc)
      (let ( (c (peek)) )
        (when (digit? c)
          (incf n1 (* d (- (char-code c) (char-code #\0))))
          (setf d (/ d 10))
          (go decimal))
        (if (or (eql c #\e) (eql c #\E)) (go expt))
        (return n1))
    expt
      (readc)
      (let ( (e (parse-fixnum)) )
        (return (* n1 (expt 10 e)))))))

(defun parse-string (terminator)
  (readc)
  (with-output-to-string (s)
    (iterate loop ()
      (let ( (c (readc)) )
        (when (eql c terminator) (return-from loop s))
        (princ c s)
        (loop)))))
  
(defun parse-balanced-string (c1 c2)
  (readc)
  (with-output-to-string (s)
    (loop for c = (readc)
      with cnt = 1
      if (eql c c1) do (incf cnt)
      else if (eql c c2) do (decf cnt)
      until (and (eql c c2) (eql cnt 0))
      do (princ c s))))

(defun parse-operator ()
  (let* ( (c (readc))
          (s (intern (format nil "~A~A" c (peek)))) )
    (cond ( (member s '(<< >>))
            (readc)
            (if (eql (peek) #\=)
              (intern (format nil "~A~A" s (readc)))
              s) )
          ( (member s '(++ -- << >> -> <= >= != == &&
                        += -= *= /= %= &= ^= \|= \|\|))
            (readc)
            s )
          (t (intern (string c))))))

(defv $next nil)

(defun scan ()
  (setf $next (parse-atom)))

(defun scan-op ()
  (setf $next (and (opchar? (peekt)) (parse-atom))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The recursive-descent parser.  Look Ma, no tables!
;;;

(defun parse-primary-expression ()
  (cond ((or (ident? $next) (numberp $next) (stringp $next))
         $next)
        ((eq $next '\( )
         (cons 'progn (parse-list)))
        ((eq $next '\{ )
         (cons 'progn (parse-list)))
        (t (syntax-error "Unexpected primary expression: ~A" $next))))

(defun parse-postfix-expression ()
  (iterate loop ((e (parse-primary-expression)))
    (scan-op)
    (cond ((eq $next '++) (loop `(prog1 ,e (incf ,e))))
          ((eq $next '--) (loop `(prog1 ,e (decf ,e))))
          (t e))))

(defun parse-term ()
  (let ((op (translate-unary-op $next)))
    (cond (op (scan) (list op (parse-postfix-expression)))
          (t (parse-postfix-expression)))))

;;; This function parses expressions, which consist of pairs of terms separated
;;; by binary operators.
;;;
(defun parse-expression (&optional (priority -1))
  (iterate loop ( (result (parse-term)) )
    (let ( (op (translate-binop $next))
           (new-priority (op-priority $next)) )
      (cond
       ((and (member $next '([ \()) (> new-priority priority))
        (let ( (args (parse-list)))
          (scan)
          (loop (list op result args))))
       ((and (binop? $next) (> new-priority priority))
        (scan)
        (loop (list op result (parse-expression new-priority))))
       (t result)))))

;;; This function parses delimiter-separated lists of expressions.
;;;
(defun parse-list (&optional (separator '\,))
  (let ((terminator (cdr (assoc $next '((\( . \)) ([ . ]) ({ . }))))))
    (if (not terminator) (syntax-error "Don't know how to balance ~A" $next))
    (scan)
    (iterate loop ()
      (cond ((null $next) (syntax-error "Missing ~S" terminator) )
            ((eq $next terminator) nil)
            (t (let ( (arg1 (parse-expression)) )
                 (unless (or (eq $next separator) (eq $next terminator))
                   (syntax-error "Expected '~A' or '~A', got '~A'"
                                 separator terminator $next))
                 (if (eq $next separator) (scan))
                 (cons arg1 (loop))))))))

;;;;;;;;;;;;;;;;
;;;
;;; Top level
;;;
(defun read-infix (stream)
  (dlet (($parcil-stream stream))
    (scan)
    (parse-expression)))

(defun parse-infix-string (s)
  (with-input-from-string (s s)
    (dlet (($parcil-stream s))
      (iterate loop ()
        (scan)
        (cons (parse-expression)
              (and (not (eof?)) (loop)))))))

;;; Readtable integration
(require :symbol-reader-macros)
(defreadtable infix (:fuze symbol-reader-macros prefix-calls))
(in-readtable infix)
(defun read-infix-body (s)
  (dlet (($parcil-stream s)) (scan) (parse-list)))
(setf $prefix-funcall-syntax-arg-reader 'read-infix-body)
(defmacro infix (&body body) `(progn ,@body))
