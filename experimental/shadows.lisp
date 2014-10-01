
(shadow 'require)

(defmacro require (&rest things)
  `(progn ,@(for thing in things collect `(cl:require ',thing))))

(shadow 'apropos)

(defmacro apropos (symbol &optional package)
  `(cl:apropos ',symbol ',package))

(require :lexicons)

(defmacro def (thing &rest body)
  (if (consp thing)
    `(ldefun ,(car thing) ,(cdr thing) ,@body)
    `(defv ,thing ,@body)))
