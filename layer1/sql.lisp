(require :cffi)
(pushnew #P"/usr/local/lib/mysql/" CFFI:*FOREIGN-LIBRARY-DIRECTORIES* :test 'equal)
(require :cl-mysql)

(require :sqlite)

(require :ergodict)

(defmacro safely (&body body)
  `(let ((*read-eval* nil)) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Database abstract class
;;;
(define-class database db tables)

(defgeneric get-tables (db))

(define-method (reload (db database tables)) (setf tables (get-tables db)))

(define-method (tables (db database tables)) tables)

(define-method (dir (db database tables)) tables)

(define-method (keys (db database tables)) (mapcar 'db-table-name tables))

(define-method ((initialize-instance :after) (db database) &rest args)
  (declare (ignore args))
  (reload db))

(define-method (ref1 (db database tables) key)
  (find key tables :key 'db-table-name :test 'string-equal))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tables
;;;
(define-class db-table db name
  column-names column-symbols column-types column-translations
  colstring primary-key)

(define-method (set-column-translator (table db-table column-names column-translations)
                                      col translator)
  (unless (find col column-names :test 'string-equal)
    (error "No column named ~A in ~A" col table))
  (setf (ref column-translations col) translator))

(defun lispify-sql-name (s)
  (if (string= s "_rowid_")
    '_id
    (intern (string-upcase (substitute #\- #\_ s)))))

(define-method ((initialize-instance :after) (tbl db-table) &rest args)
  (declare (ignore args))
  (reload tbl))

(define-method (reload (tbl db-table column-names column-symbols column-translations
                            colstring))
  (setf colstring (join (for s in column-names collect (format nil "\"~A\"" s)) ","))
  (setf column-symbols (mapcar 'lispify-sql-name column-names))
  (setf column-translations (->))
  tbl)

(define-print-method (db-table db name) "#<DB Table ~A>" name)

(define-method (ref1 (tbl db-table db name colstring primary-key) key)
  (if (null primary-key) (error "~A does not have a primary key" tbl))
  (if (atom key) (setf key (list key)))
  (bb lpk (length primary-key)
      (if (/= (length key) lpk)
        (error "Key for ~A must be a list of length ~A" tbl lpk))
      key (mapcar 'sql-escape key)
      keystring (join (mapcar (fn (k v) (format nil "\"~A\"=~A" k v)) primary-key key) " and ")
      rows (query db "select ~A from \"~A\" where ~A" colstring name keystring)
      (if (cdr rows)
        (error "Primary key query returned multiple rows!  This should never happen.")
        (and rows (make-db-row :table tbl :data (1st rows))))))

(define-method (del (tbl db-table db name primary-key) key)
  (if (null primary-key) (error "~A does not have a primary key" tbl))
  (if (atom key) (setf key (list key)))
  (bb lpk (length primary-key)
      (if (/= (length key) lpk)
        (error "Key for ~A must be a list of length ~A" tbl lpk))
      key (mapcar 'sql-escape key)
      keystring (join (mapcar (fn (k v) (format nil "\"~A\"=~A" k v)) primary-key key)
                      " and ")
      (query db "delete from \"~A\" where ~A" name keystring)))

(define-method (slice (tbl db-table db name colstring primary-key) start &optional end step)
  (if (null primary-key) (error "~A does not nave a primary key" tbl))
  (if step (error "Database table slicing does not support the STEP argument"))
  (if (atom start) (setf start (list start)))
  (if (atom end) (setf end (list end)))
  (bb lpk (length primary-key)
      (if (/= (length start) lpk)
        (error "START parameter for ~A must be a list of length ~A" tbl lpk))
      (if (/= (length end) lpk)
        (error "END parameter for ~A must be a list of length ~A" tbl lpk))
      start (mapcar 'sql-escape start)
      end (mapcar 'sql-escape end)
      keystring (join
                 (append
                  (mapcar (fn (k v) (format nil "~A>=~A" k v)) primary-key start)
                  (mapcar (fn (k v) (format nil "~A<~A" k v)) primary-key end))
                 " and ")      
      rows (query db "select ~A from ~A where ~A" colstring name keystring)
      (for row in rows collect (make-db-row :table tbl :data row))))

(define-method (size (tbl db-table db name))
  (caar (query db "select count(*) from ~A" name)))

(define-method (keys (tbl db-table db name primary-key))
  (query db "select ~A from \"~A\""
         (join (for k in primary-key collect (strcat #\" k #\")) ",") name))

(define-method (columns (tbl db-table column-names)) column-names)

(define-method (types (tbl db-table column-types)) column-types)

(define-method (iterator (tbl db-table))  ; Fix to use a real cursor some day
  (iterator (select tbl)))

(define-method (primary-key (tbl db-table primary-key)) primary-key)

(define-method (select (tbl db-table db name colstring) &optional where)
  (if (consp where) (setf where (sql-render-where-clause where)))
  (for r in (query db "select ~A from '~A' where ~A" colstring name (or where 1))
    collect (make-db-row :table tbl :data r)))

(define-method (sql-render thing) (prin1-to-string thing))
(define-method (sql-render (n null)) nil) ; Maybe should be NULL???
(define-method (sql-render (s symbol)) (princ-to-string s))

(define-method (sql-render (f float))
  (let ((*READ-DEFAULT-FLOAT-FORMAT* (type-of f)))
    (format nil "~F" f)))

(define-method (sql-render (o standard-object))
  (bb id (primary-key o)
      class (class-of o)
      (when (and (null id) (aipk? (primary-key class)))
        (store o)
        (setf id (primary-key o)))
      (format nil "⌘~A~A~A" (class-name class) (if (stringp id) "@" "#") id)))

(defun sql-unrender (thing)
  (mcond (not (stringp thing)) thing
         (not (eql (char thing 0) #\⌘)) (safely (read-from-string thing))
         t (deref-object thing)))

(require :cl-ppcre)

(defun deref-object (s)
  (bb :db (class id_type id) (cl-ppcre:split "(#|@)" (subseq s 1)
                                             :limit 2 :with-registers-p t)
      class (intern class)
      (if (equal id_type "#") (setf id (safely (read-from-string id))))
      (or (unstore class id)
          (error "~A not found" s))))

(defun sql-render-where-clause (form)
  (format nil "(~A ~A ~A)" (sql-render (2nd form)) (1st form) (sql-render (3rd form))))

(define-method (sql-escape (thing null)) (progn "NULL"))  ; Without PROGN it looks like a docstring
(define-method (sql-escape (thing symbol)) (symbol-name thing))
(define-method (sql-escape (thing string))
  (strcat "'" (strsubst thing "'" "''") "'"))
(define-method (sql-escape (thing number)) thing)

;;;;;;;;;;;;;;;;;;;
;;;
;;; Index (should extend to multi-column some day)
;;;
(define-class db-table-index table column)

(define-print-method (db-table-index table column)
                     "#<Index on column ~A of ~A>" column table)

(define-method (index (table db-table) column)
  (make-instance 'db-table-index :table table :column column))

(define-method (ref1 (idx db-table-index table column) key)
  (if (atom column)
    (select table (format nil "~A=~A" column (sql-escape (sql-render key))))
    ; Preliminary extension to multi-column
    (select table (join (for (col k) in (zip column key) collect
                          (format nil "~A=~A" col (sql-escape (sql-render k))))
                        " and "))))

;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rows
;;;
(define-class db-row table data)

(define-method (keys (r db-row table)) (db-table-column-names table))

(define-method (has-key (r db-row) key)
  (position key (keys r) :test 'string-equal))

(define-method (ref1 (r db-row table data) key)
  (aif (has-key r key) (elt data it) (error "No slot named ~A in ~A" key r)))

(define-method (setref (r db-row table data) key value)
  (aif (has-key r key) (setf (elt data it) value) (error "No slot named ~A in ~A" key r)))

(define-method (print-object (r db-row table data) stream)
  (format stream "#<DB-ROW ~A" (db-table-name table))
  (for (k v) in (zip (db-table-column-names table) data)
    do (format stream " ~A=~S" k v))
  (format stream ">"))

(define-method ((initialize-instance :after) (r db-row table) &rest args)
  (declare (ignore args))
  (for (k v) in (db-table-column-translations table)
    (setf (ref r k) (funcall v (ref r k)))))

(define-method (store (row db-row table data))
  (bb db (db-table-db table)
      (query db
             (format nil "insert or replace into ~A (~A) values (~A)"
                     (db-table-name table)
                     (db-table-colstring table)
                     (join (for d in data collect (sql-render d)) ",")))
      (if (and (equal (db-table-primary-key table) '("_rowid_")) (null (ref row '_rowid_)))
        (setf (ref row '_rowid_) (last-inserted-rowid db)))
      row))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Storable objects
;;;
;;; First slot is the primary key.  If its names starts with an underscore then
;;; it will be an integer autoincrement, otherwise it user-settable and not-null.
;;;
;;; NOTE: There are three methods used to get identifiers for objects.  The ID
;;; method returns a unique ID that is safe to use as a column name or HTML
;;; identifier.  The LABEL method returns a human-readable string.  The PRIMARY-KEY
;;; method returns the object's primary key when it is stored in a database.
;;;

(define-method (id-render (s string))
  (substitute-if #\_ (fn (c) (find c " +-=~!@#$%^&*()")) s))

(define-method (id-render (s symbol)) (id-render (symbol-name s)))

(define-method (id-render (i integer)) (princ-to-string i))

(define-method (primary-key (c standard-class))
  (1st (slot-names c)))

(define-method (primary-key (o standard-object))
  (slot-value o (primary-key (class-of o))))

(define-method (label (o standard-object))
  (slot-value o (primary-key (find-class (type-of o)))))

(define-method (id (o standard-object))
  (id-render (slot-value o (primary-key (find-class (type-of o))))))

(defv $oids (make-hash-table :test 'eq :weak :key))

(defun slot-names (class)
  (mapcar 'slot-definition-name (class-slots (if (symbolp class) (find-class class) class))))

(defv $class-tables (->))
(defv $class-caches (->))

(defun make-cache () (make-hash-table :test 'equal :weak :value))

; AIPK = Auto Increment Primary Key
(defun aipk? (pk) (eql (elt (symbol-name pk) 0) #\_))

(defun make-persistent (class db)
  (if (symbolp class) (setf class (find-class class)))
  (bb class-name (class-name class)
      slot-names (slot-names class)
      primary-key (or (1st slot-names)
                      (error "Class must have at least one slot to serve as primary key"))
      (setf slot-names (remove primary-key slot-names))
      primary-key-type (if (aipk? primary-key) "integer primary key" "primary key not null")
      primary-key-string (format nil "'~A' ~A" primary-key primary-key-type)
      column-names (for slot-name in slot-names collect (format nil "'~A'" slot-name))
      colstring (join (cons primary-key-string column-names) ",")
      (reload db)
      table (ref db class-name)
      (if table
        (bb columns (columns table)
            pk (db-table-primary-key table)
            (unless (and (null (cdr pk)) (string= (1st pk) primary-key))
              (error "Incompatible primary keys: ~A ~A" pk primary-key))
            (for slot in slot-names do
              (unless (member slot columns :test 'string-equal)
                (query db "alter table ~A add column ~A" class-name slot))))
        (query db "create table '~A' (~A)" class-name colstring))
      (reload db)
      (setf table (ref db class-name))
      (setf (ref $class-tables class-name) table)
      (setf (ref $class-caches class-name) (make-cache))
      table))

(define-method (menu-item-value (thing standard-object) counter)
  (declare (ignore counter))
  (label thing))

(define-method (objectify (r db-row table data))
  (bb class (find-class (intern (db-table-name table)))
      o (make-instance class)
      pk (primary-key class)
      id (sql-unrender (ref r pk))
      (setf (ref (ref $class-caches (class-name class)) id) o)
      (for slot in (slot-names class) do
        (setf (ref o slot) (sql-unrender (ref r slot))))
      o))

(define-method (objectify (n null)) nil)

(define-method (store (o standard-object))
  (bb class-name (type-of o)
      class (find-class class-name)
      slot-names (slot-names class)
      primary-key (1st slot-names)
      (if (and (not (aipk? primary-key)) (null (slot-value o primary-key)))
        (error "Primary key is not autoincrement and so must not be NIL"))
      colstring (join (for s in slot-names collect (format nil "'~A'" s)) ",")
      table (ref $class-tables class-name)
      data (for slot in slot-names collect (slot-value o slot))
      query-string (format nil "insert or replace into '~A' (~A) values (~A)"
                           (db-table-name table)
                           colstring
                           (join (for d in data collect (sql-escape (sql-render d))) ","))
      (query (db-table-db table) query-string)
      (if (and (aipk? primary-key) (null (slot-value o primary-key)))
        (setf (slot-value o primary-key) (last-inserted-rowid (db-table-db table))))
      id (slot-value o primary-key)
      (setf (ref (ref $class-caches class-name) id) o)
      o))

(define-method (unstore (class-name symbol) id)
  (or (ref (ref $class-caches class-name) id)
      (bb o (objectify (ref (ref $class-tables class-name) (sql-render id)))
          (if o (setf (ref (ref $class-caches class-name) id) o))
          o)
      ; For integer ids that have to go through an HTML rendering
      (bb i (ignore-errors (parse-integer id)) (and i (unstore class-name i)))))

(define-method (iterator (c standard-class))
  (iterator (all-instances c)))

(defun all-instances (class)
  (bb class (if (typep class 'standard-class) class (find-class class))
      classname (class-name class)
      pk (primary-key class)
      cache (ref $class-caches classname)
      (for row in (select (ref $db classname)) collect
        (bb id (sql-unrender (ref row pk))
            (or (ref cache id) (setf (ref cache id) (objectify row)))))))

(defun object-id (o) (slot-value o (primary-key (find-class (type-of o)))))

(define-method (ref1 (c standard-class) id) (unstore (class-name c) id))

(define-method (del (c standard-class) id)
  (bb name (class-name c)
      (del (ref $db name) (sql-render id))
      (del (ref $class-caches name) id)
      t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  MySQL database
;;;
(define-class (mysql-db database))

(defun make-mysql-db (&rest args)
  (bb db (apply 'cl-mysql:connect args)
      (cl-mysql:option :opt-reconnect 1 :database db)
      (make-instance 'mysql-db :db db)))

(define-method (query (_db mysql-db db) arg &rest args)
  (bb r (1st (cl-mysql:query (if args (apply 'format nil arg args) arg) :database db))
      (values (1st r) (2nd r))))

(define-method (get-tables (db mysql-db))
  (for tbl in (query db "show tables") collect
    (bb name (1st tbl)
        cols (query db "describe `~A`" name)
        colnames (mapcar '1st cols)
        coltypes (mapcar '2nd cols)
        primary-key (for c in cols if (string-equal (4th c) "PRI") collect (1st c))
        (make-db-table :db db :name name :column-names colnames :column-types coltypes
                       :primary-key primary-key))))

(define-method (close (_db mysql-db db) &key abort)
  (declare (ignore abort))
  (cl-mysql:disconnect db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SQLite database
;;;
(define-class (sqlite-db database))

(defun make-sqlite-db (path)
  (make-instance 'sqlite-db :db (sqlite:connect (pathname path))))

(define-method (query (db sqlite-db db) arg &rest args)
  (sqlite:execute-to-list db (if args (apply 'format nil arg args) arg)))

(define-method (get-tables (db sqlite-db))
  (for tbl in (query db "select name from sqlite_master where type='table'
        and name not in ('sqlite_sequence')") collect
    (bb name (1st tbl)
        cols (query db "pragma table_info('~A')" name)
        colnames (mapcar '2nd cols)
        coltypes (mapcar '3rd cols)
        primary-key (for c in cols if (= (6th c) 1) collect (2nd c))
        (unless primary-key
          (setf primary-key '("_rowid_"))
          (push "_rowid_" colnames)
          (push "integer" coltypes))
        (make-db-table :db db :name name :column-names colnames :column-types coltypes
                       :primary-key primary-key))))

(define-method (last-inserted-rowid (db sqlite-db db))
  (sqlite:last-insert-rowid db))

(define-method (close (db sqlite-db db) &key abort)
  (declare (ignore abort))
  (sqlite:disconnect db))

(define-method (del (db sqlite-db) key)
  (query db (format nil "drop table if exists '~A'" key)))
