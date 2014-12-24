ergolib
=======
Copyright (c) 2014 by Ron Garret.

This code is free software.  It is distributed under the terms of the BSD three-clause license:

http://opensource.org/licenses/BSD-3-Clause

Summary
-------
Ergolib is a disparate collection of utilities designed to make programming in Common Lisp easier.  The design of ergolib is based on a desire to reduce cognitive load on the programmer.  It deliberately sacrifices run-time efficiency and adherence to certain Common Lisp coding conventions in service of this goal.  In particular, ergolib does not use a build system and does not use packages (though it could be easily modified to do both if anyone wants it to).

Ergolib currently works only in Clozure Common Lisp.  I'm working on a port to SBCL.  If anyone proficient in SBCL wants to volunteer some help that would be much appreciated.

This code has been used in a lot of projects and is fairly mature, but it has never been released before so it probably has some rough edges.  If you encounter any problems please do not hesitate to contact me.

Quickstart: load init.lisp and then do (require :ergolib)

Basics
------
There are some convenience functions that are used throughout ergolib.

FN is short for LAMBDA.

DEFINE-CLASS and DEFINE-METHOD are thin wrappers over DEFCLASS and DEFMETHOD with a syntax that I find easier to remember:

	(DEFINE-CLASS (classname &rest superclasses) &rest slots)
	(DEFINE-METHOD (method-name (&rest args) &body body))

where each ARG in *args* is either a symbol or a list of the form:

	(argname classname &rest slot-names)

Each *SLOT-NAME* is bound in the body of the method using WITH-SLOTS.

There is also a macro called REF with is a universal de-referencer.  Just about anything that can be de-referenced with an index can be de-referenced using REF, so you never have to remember whether to use ELT or NTH or SLOT-VALUE or GETHASH.  Just use REF.  Examples:

	? (ref '(1 2 3) 1)
	2
	? (ref #(1 2 3) 1)
	2
	? (ref "abc" 1)
	#\b
	? (define-class class1 slot1)
	#<STANDARD-CLASS CLASS1>
	? (setf c (make-class1 :slot1 123))
	#<CLASS1 #x30200119EC3D>
	? (ref c 'slot1)
	123
	? (setf h (make-hash-table))
	#<HASH-TABLE :TEST EQL size 0/60 #x30200111E77D>
	? (setf (ref h 'foo) 'baz)
	BAZ
	? (ref h 'foo)
	BAZ
	T

Iterators
---------
The iterator library consists of two main components: the iterator protocol, and a macro called FOR that makes it easy to access that protocol.  The syntax of the FOR macro is:

(FOR var IN expression {IF condition} [DO|COLLECT|VCOLLECT] &body body)

The *expression* can be pretty much anything you want to loop over.  The way the FOR macro works is that it calls the generic function ITERATOR on the value of *expression*.  ITERATOR returns a closure which, when called, returns successive values of the elements of *expression*.  The end of the sequence of values is signalled by returning the result of calling (ITEREND).

Examples:

	? (for item in '(1 2 3) do (print item))
	1 
	2 
	3 
	? (for item in "string" collect item)
	(#\s #\t #\r #\i #\n #\g)
	? (for item in '(1 2 3) if (oddp item) vcollect item)
	#(1 3)

VCOLLECT collects the items as a vector.

The real power of iterators comes when you build generators.  For example, there is a built-in generator called COUNTER:

	(COUNTER &optional start end step)
	
	? (for x in (counter 1 10 2) collect x)
	(1 3 5 7 9)

Iterators can be combined using the ZIP function:

	? (for (char cnt) in (zip "foo" (counter)) collect (cons char cnt))
	((#\f . 0) (#\o . 1) (#\o . 2))

Note that ZIP ends the iteration whenever any of its component iterators ends.  COUNTER by itself starts counting at zero and runs forever.  But note that iterators are lazy, so calling (counter) is NOT an infinite loop!

Iterators can be concatenated using the CAT function:

	? (for x in (cat (counter 1 3) (counter 6 8)) collect x)
	(1 2 6 7)

Or sliced using the SLICE function:

	? (for x in (slice (counter) 10 15) collect x)
	(10 11 12 13 14)

The idiom (FOR X in ITERATOR collect X) can be abbreviated FORCE:

	? (counter 1 10)
	#<COMPILED-LEXICAL-CLOSURE (:INTERNAL COUNTER) #x30200119ED8F>
	? (force *)
	(1 2 3 4 5 6 7 8 9)

There is a built-in iterator for pathnames that iterates over the contents of the file that the pathname points to:

	[bash:~]➔ cat test
	line1
	line2
	line3
	
	? (for c in #P"~/test" collect c)
	(#\l #\i #\n #\e #\1 #\Newline #\l #\i #\n #\e #\2 #\Newline #\l #\i #\n #\e #\3 #\Newline)

The LINES iterator delivers the contents of its argument as lines instead of characters:

	? (force (lines #P"~/test"))
	("line1" "line2" "line3")

N-AT-TIME delivers the elements of an iterator N items at a time:

	? (for (x y z) in (n-at-a-time 3 (counter 0 9)) collect (list x y z))
	((0 1 2) (3 4 5) (6 7 8))

SLICES does the same thing but delivers the results as lists:

	? (force (slices 3 (counter 0 9)))
	((0 1 2) (3 4 5) (6 7 8))

The best way to learn to write your own iterators is to look at the source for the existing ones.  It's pretty straightforward.

Binding-block
-------------
BINDING-BLOCK (or BB for short) is a universal binding construct that replaces LET*, FLET, MULTIPLE-VALUE-BIND and DESTRUCTURING-BIND.  The syntax is:

	(BB &rest clauses)

where each clause in *clauses* is one of:

	(form)
	var form          ; Equivalent to (let ((var form)) ...)
	:db (vars) form   ; (destructuing-bind (vars) form ...)
	:mv (vars) form   ; (multiple-value-bind (vars) form ...)
	:fn name (args) (body) ; (flet ((name (args) (body))) ...)

BB is actually user-extensible using the DEF-BB-CLAUSE macro.  If you want to write your own BB clauses, look at the source for the existing ones.  It's not hard, pretty much like writing any other macro, except that you can't have any optional arguments.  This is because BB parses each clause by the (fixed) number of arguments in the definition of the BB clause.

The main advantage of BB is that it elminates a lot of parens and keeps code from crawling off the right side of the screen.  Personally, I like this style, by hard-core parenthophiles tend not to like it.  If you don't like it, don't use it.  But if you're trying to read my code, you'll find BB all over the place, so I'm documenting it in order to make it easier to figure out what is going on.

PyLink and BashLink
-------------------
PyLink and BashLink are socket-based connections to a python interpreter and a bash shell.  Easiest to explain by example:

	? (require :pylink)
	:PYLINK
	
	? (py "def foo(x): return x*x")
	NIL
	? (py "print foo(123)")
	("15129")
	
	? (require :bashlink)
	:BASHLINK
	
	? (setf b (make-bash-server))
	#<Bash server 22621 :RUNNING>
	? (cmd b "pwd")
	("/")
	NIL
	? (cmd b "cd /etc")
	NIL
	NIL
	? (cmd b "ls host*")
	("hostconfig" "hostconfig.system_default" "hostconfig~orig" "hosts" 	"hosts.equiv" "hosts.umbrella" "hosts~orig")

The interfaces are different (there is only one global python server whereas bash servers are first-class objects) because these two utilities were written at different times for different purposes.  It would be pretty easy to unify them so they both work the same way.

Miscellaneous
---
There are a bunch of miscellaneous things in ergolib which I will only bother documenting if someone asks me to.  But here's a list of what's there along with a brief descritpion:

* RFFI is a foreign function interface that I wrote before I knew about CFFI
* HASHLIB is a library of cryptographic hash functions
* CAS is a content-addressable persistent store
* TSFIFO is a thread-safe FIFO (first-in-first-out queue)
* PATTERN-MATCH is a powerful but light-weight pattern matching library
* HTML-UTILS is exactly what you would expect

If you want more info about any of these, or anything else in ergolib please do not hesitate to contact me.
