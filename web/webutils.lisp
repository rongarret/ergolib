(require :ergolib)

(require :md5)
(defun md5::md5sum-string (string)
  (with-input-from-string (s string) (md5:md5sum-stream s)))
(export 'md5::md5sum-string :md5)

(require :hunchentoot)

(rename-package :hunchentoot :hunchentoot '(:tbnl :ht))

(import '(ht:SCRIPT-NAME* ht:POST-PARAMETERS* ht:GET-PARAMETERS* ht:*REQUEST*))

(defvar $http-server nil)

(defun ensure-http-server (port)
  (unless $http-server
    (setf $http-server (make-instance 'ht::easy-acceptor :port port))
    (ht:reset-session-secret)
    (ht:start $http-server)))

(setf CHUNGA:*ACCEPT-BOGUS-EOLS* t)

(require :cl-who)
; Interesting name conflict here
(unintern 'fmt)
(use-package :cl-who)
(defun fmt (s &rest args)
  (without-length-restrictions
   (apply 'format nil s args)))
(require :cl-who-patches)

(defmethod who:convert-tag-to-string-list ((tag (eql :comment)) attr-list body body-fn)
  (declare (ignore attr-list body-fn))
  (list (format nil "<!-- ~A -->" (car body))))

(defmethod who:convert-tag-to-string-list ((tag (eql :!doctype)) attr-list body body-fn)
  ; Temporary (I hope) hack because doctype doesn't get handled properly upstream
  (declare (ignore attr-list))
  (cons "<!doctype \"html\">" (funcall body-fn body)))

(defun set-return-code (code) (setf (ht:return-code*) code) nil)

(defun set-header (name value) (setf (ht:header-out name) value))

(defun not-found (request)
  (declare (ignore request))
  (fn ()
    (set-return-code ht::+http-not-found+)
    (set-header :content-type "text/html; charset=utf-8")
    "<h1>404 - NOT FOUND</h1>"))

(defv $pages (->))

(defun page-dispatcher (request)
  (ref $pages (ht:script-name request)))

(defun check-path (s &optional if-directories-do-not-exist)
  (bb ns (namestring s)
      dir-ns (directory-namestring s)
      (if (null (probe-file dir-ns))
        (ecase if-directories-do-not-exist
          (:create (create-directory dir-ns))
          (:error (error "Directory ~A does not exist" dir-ns))
          (nil)))
      (values ns (ccl::%unix-file-kind ns))))

(defv $static-file-path (list (strcat (this-directory) "css/")
                              (strcat (this-directory) "js/")))

(defun static-file-dispatcher (request)
  (for dir in $static-file-path do
    (bb :mv (path kind) (check-path (strcat dir (ht:script-name request)))
        (if (eq kind :file)
          (return-from static-file-dispatcher
            (fn ()
              (set-header :content-type (ht:mime-type path))
              (file-contents path :binary)))))))

(setf ht:*dispatch-table* '(page-dispatcher static-file-dispatcher not-found))

(defun html-escape (s) (cl-who:escape-string s))

(define-method (html-render thing)
  (princ (html-escape (princ-to-string thing))))

(defvar *indent* t)

; WHO is shorthand for With-HTML-Output.  Dumps result to stdout
(defmacro who (&body body)
  `(without-length-restrictions
    (with-html-output (*standard-output* nil :indent *indent*)
      ,@body)
    (values)))

; Like WHO but outputs a prologue
(setf cl-who:*prologue* "<!DOCTYPE html>") ; Note: this gets evaluated at compile time

(defmacro page (&body body)
  `(without-length-restrictions
    (with-html-output (*standard-output* nil :indent *indent* :prologue t)
      ((:html :lang "en") ,@body))
    (values)))

; Like WHO, but return result as string
(defmacro whos (&body body)
  `(with-output-to-string (*standard-output*)
     (who ,@body)))

; Like WHO, but return result as HTML Object (to be output with HTML-RENDER)
(defmacro whobj (&rest body)
  `(html-string (whos ,@body)))

; With Captured Output, for things like DESCRIBE
(defmacro wco (form) 
  `(htm (:pre (esc (with-output-to-string (*standard-output*) ,form)))))

(defun hro (thing) (html-render thing))

(defmacro html (&body body)
  `(with-output-to-string (*standard-output*)
     ,(if (eq (1st body) :page)
        `(page1 ,@(rst body)) ; Use CL-WHO-PATCHES to move scripts to end
       ; `(page ,@(rst body))
        `(who ,@body))))

(defun no-cache ()
  (who (:meta :http-equiv "cache-control" :value "no-cache")
       (:meta :http-equiv "pragma" :value "no-cache")))

(defun error-page ()
  (no-cache)
  "Sorry, an unexpected error occurred.")

(defun development-error-handler (condition)
  (set-header "cache-control" "no-cache")
  (throw 'page-handler-done
    (without-length-restrictions
     (logmsg ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
     (logmsg "ERROR: ~A" (princ-to-string condition))
     (logmsg "User: ~S" (ht:session-value 'user))
     (logmsg "IP: ~S" (try (ht:remote-addr*)))
     (logmsg ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
     (logmsg "Backtrace:~%~A" (get-backtrace))
     (error-page))))

(defun error-handler (condition) (development-error-handler condition))

(defv $enable-access-log nil)

(define-method (ht:acceptor-access-log-destination (acceptor ht:easy-acceptor))
  (and $enable-access-log (logstream)))

(define-method (ht:acceptor-message-log-destination (acceptor ht:easy-acceptor))
  (logstream))

(make-package "URL" :use nil)

(defv $content-type-dict
  (-> :text "text/plain; charset=utf-8"
      :html "text/html; charset=utf-8"
      :js "text/javascript; charset=utf-8"
      :json "application/json"
      :excel "application/vnd.ms-excel"
      :bytes "application/octet-stream"))

(defv $default-content-type "text/plain; charset=utf-8")

(defmacro defurl (url content-type &body body)
  (unless (eql (char url 0) #\/) (setf url (strcat "/" url)))
  (bb name (intern url "URL")
      content-type (ref $content-type-dict content-type
                        (or content-type $default-content-type))
      (with-gensym s
        `(progn
           (setf (ref $pages ,url) ',name)
           (defun ,name ()
             (set-header :content-type ,content-type)
             (catch 'page-handler-done
               (handler-bind ((error 'error-handler))
                 (bb ,s (progn ,@body)
                     ; Hunchentoot will barf it it's not a string
                     (unless (stringp ,s) (setf ,s (princl ,s nil)))
                     ,s))))))))

(defmacro defpanel (url &body body)
  `(defurl ,url :html (html ,@body)))

(defmacro defpage (url &body body)
  `(defurl ,url :html
     (ht:start-session)
     (html :page ,@body)))

(defindent "defpage" 1)
(defindent "defpanel" 1)
(defindent "throw" 1)

(defpage "/test-error" (error "Test error"))

(defun meta-refresh (url &optional (delay 0))
  (without-length-restrictions
   (format nil "<meta http-equiv='refresh' content='~A; url=~A'>" delay url)))

(defun forward (url)
  (set-header :refresh (format nil "0; url=~A" url))
  (throw 'page-handler-done (meta-refresh url)))

(defun stylesheet (path)
  (who (:link :href path :rel "stylesheet" :type "text/css")))

(defmacro generate-info-table (&rest forms)
  `(princ
    (html
     (:table :border 1 :cellspacing 0
           ,@(loop for form in forms collect
               `(:tr
                 (:td (:pre (esc (princ-to-string ',form))))
                 (:td (wco (pprint ,form)))))))))

;;; Application prefix

(require :puri)

(defv $application-prefix "")

(defun application-prefix ()
  (if (boundp 'ht:*request*)
    (or (ht:header-in* :X-application-prefix) "")
    $application-prefix))

(define-method (puri:uri-path (u puri:uri puri::path puri::host))
  (if (and (null puri::host) (eql (elt puri::path 0) #\/))
    (strcat (application-prefix) puri::path)
    puri::path))

(defmacro link (url &body body)
  `(who ((:a :href ,(puri::parse-uri url)) ,@body)))

; Override Puri's URL caching
(define-method ((print-object :before) (u puri:uri string) stream)
  (declare (ignore stream))
  (setf string nil))


;;; Form stuff

(defv $fony-form nil)

(defun get-form-parameters ()
  (or $fony-form
      (and (boundp '*request*)
           (append (get-parameters*) (post-parameters*)))))

(defun getformslot (name)
  (cdr (assoc name (get-form-parameters) :test 'string=)))

(defun getformslot-multi (name)
  (mapcar 'cdr (remove name (get-form-parameters) :test-not 'string= :key 'car)))

(defun formval (spec)
  (if (atom spec) (getformslot spec) (getformslot-multi (car spec))))

(defun formvals (&rest specs)
  (if (null specs)
    (get-form-parameters)
    (for spec in specs collect (formval spec))))

(defun formkeys () (mapcar '1st (get-form-parameters)))

;;; Drag-n-drop

(defun dnd (callback-url &optional (msg "Drop to upload..."))
  (who
   (stylesheet "dnd.css")
   (:script :src "dnd.js")
   (:script
    (fmt "function dnd_init(path) { ergo.dnd.url = '~A' }" callback-url)
    "$(dnd_init);")
   ((:div :id "uploadContainer")
    ((:div :id "uploadOverlay"))
    ((:div :id "uploadStatus")
     (str msg)
     ((:div :id "uploadMessage"))
     ((:div :id "uploadProgress") "Upload Progress:" (:div (:div)))))))

;;; Basic HTML renderings
(define-method (html-render (s string)) (princ (html-escape s)))

(define-method (html-render (f function)) (html-render (funcall f)))

(define-class html-items items)

(define-method (html-render (hi html-items items))
  (for item in items do (terpri) (html-render item))
  (terpri))

(defun html-items (items) (make-html-items :items items))

(define-class html-string string) ; Raw, unescaped HTML

(define-method (html-render (hs html-string string)) (princ string))

(defun html-string (s) (make-html-string :string s))


;;; TAG class
(define-class tag
  name content
  (attrs (make-dictionary :implementation (make-dictionary-implementation 'plist))))

(define-method (html-render (tag tag name attrs content))
  (princ #\<)
  (princ name)
  (for (k v) in attrs do
    (setf v (if (functionp v) (funcall v) v))
    (when v
      (princ #\space)
      (princ k)
      (unless (eq v t)
        (princ "=\"")
        (html-render v)
        (princ #\"))))
  (if content
    (progn
      (princ #\>)
      (html-render content)
      (princ "</")
      (princ name)
      (princ #\>))
    (princ " />"))
  (values))

;;; Links
(define-class (link tag))

(define-method (initialize-instance (l link name attrs content) &rest args &key href)
  (declare (ignore args))
  (call-next-method)
  (setf name "A")
  (setf (ref attrs :href) href))

; (defun link (href content) (make-instance 'link :href href :content content))

;;; Form input abstract class
(define-class (form-input tag) id override default)

(define-method (value* (fi form-input id))
  "Basic method to get current form input.  Can be overridden by subclasses."
  (getformslot id))

(define-method (value (fi form-input default override))
  "Layered method to get current value taking default and override into account.
   Should not be overridden."
  (or override (value* fi) default))

(define-method (make-instance (fi form-input) &rest args)
  (declare (ignore args))
  (error "FORM-INPUT is an abstract class."))

(define-method (initialize-instance (fi form-input name id type attrs) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf name "input")
  (setf (ref attrs :value) (fn () (value fi)))
  (setf (ref attrs :name) (fn () id)))

;;; Hidden input
(define-class (hidden-input form-input))

(define-method (initialize-instance (hi hidden-input attrs) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (ref attrs :type) "hidden"))

(defun hidden-input (&optional (name (gensym)) value)
  (bb hi (make-instance 'hidden-input :id name)
      (if value (setf (ref (ref hi 'attrs) :value) value))
      hi))

;;; File input (requires :enctype "multipart/form-data")
(define-class (file-input tag) id (encoding :utf-8))

(define-method (initialize-instance (fi file-input name id attrs) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf name "input")
  (setf (ref attrs :type) "file")
  (setf (ref attrs :name) (fn () id)))

(define-method (content (fi file-input id encoding))
  (bb filespec (getformslot id)
      (if (atom filespec) nil (file-contents (1st filespec) encoding))))

(define-method (filename (fi file-input id))
  (2nd (getformslot id)))

(defun fileinput (id &optional (encoding :utf-8))
  (make-instance 'file-input :id id :encoding encoding))

(defun multifileinput (id &optional (encoding :utf-8))
  (bb _ (make-instance 'file-input :id id :encoding encoding)
      (setf (ref (tag-attrs _) :multiple) :multiple)
      _))

(define-method (contents (fi file-input id encoding))
  (bb filespecs (remove id (get-form-parameters) :key 'car :test-not 'string-equal)
      (for filespec in filespecs collect
        (if (atom filespec) nil (file-contents (2nd filespec) encoding)))))

(define-method (filetypes (fi file-input id))
  (mapcar '4th (remove id (get-form-parameters) :key 'car :test-not 'string-equal)))

(define-method (iterator (fi file-input))
  (zip (filenames fi) (contents fi) (filetypes fi)))

; Necessary hack for non-ascii filenames.  See http://stackoverflow.com/a/216777
(define-method (filenames (fi file-input id))
  (bb filespecs (remove id (get-form-parameters) :key 'car :test-not 'string-equal)
      (for filespec in filespecs collect
        (or (ignore-errors (bytes-to-string (string-to-bytes (3rd filespec))))
            (bytes-to-string (string-to-bytes (3rd filespec) :latin1))))))

;;; Text input
(define-class (text-input form-input) (type "text"))

(define-method (initialize-instance (ti text-input attrs) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (ref attrs :type) "text"))

(defun textinput (id &key default override)
  (make-instance 'text-input :id id :default default :override override))

;;; Password input
(define-class (password-input form-input))

(define-method (initialize-instance (pwi password-input attrs) &rest args)
  (declare (ignore args))
  (call-next-method)
  (del attrs :value) ; Don't preserve a password input's value across pages
  (setf (ref attrs :type) "password"))

(defun pwinput (id) (make-instance 'password-input :id id))

;;; Text area
(define-class (text-area form-input) cols rows)

(define-method (initialize-instance (ta text-area name id attrs content cols rows)
                &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf name "textarea")
  (setf (ref attrs :name) id)
  (setf (ref attrs :cols) (fn () cols))
  (setf (ref attrs :rows) (fn () rows))
  ; Textareas store their value in their content, not their value attribute
  (del attrs :value)
  (setf content (fn () (value ta))))

(defun textarea (id cols rows) (make-text-area :id id :cols cols :rows rows))

;;; Checkboxes
(define-class (checkbox form-input))

(define-method (initialize-instance (cb checkbox attrs) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (ref attrs :type) "checkbox")
  (setf (ref attrs :checked) (fn () (value cb))))

(define-method (value (cb checkbox id default override))
  (cond ((eq override :off) nil)
        (override t)
        ((get-form-parameters) (not (null (getformslot id))))
        (t (not (member default '(nil :off))))))

(defun checkbox (id) (make-instance 'checkbox :id id))

;;; Radio buttons

(define-class (radio-buttons form-input) items)

(define-method (html-render (rb radio-buttons id items))
  (for (item cnt) in (zip items (counter)) do
    (terpri)
    (format t "<input type=\"radio\" name=\"~A\" value=\"~A\"" id cnt)
    (if (eq (value rb) item) (princ " checked"))
    (print #\>)
    (html-render item)
    (princ "</input>"))
  (terpri))

(define-method (value* (rb radio-buttons id items))
  (aif (getformslot id) (nth (parse-integer it) items)))

(defun radio-buttons (id items)
  (make-radio-buttons :id id :items items))

;;; Menus
;;;
;;; The protocol for menu items is that the respond to two methods:
;;; menu-item-render produces the thing that is displayed in the menu
;;; menu-item-value produces the value for the enclosing select

(define-method (menu-item-render (thing t)) (princ-to-string thing))

(define-method (menu-item-render (thing cons)) ; NOT LIST!  Causes an infinite loop
  (menu-item-render (1st thing)))

(define-method (menu-item-value (thing t) counter)
  (princ-to-string counter))

(define-method (menu-item-value (thing cons) counter)
  (declare (ignore counter))
  (princ-to-string (2nd thing)))

(define-class (menu-item tag) item id parent)

(defgeneric selected-in? (parent item))

(define-method (initialize-instance (mo menu-item attrs name content item id parent)
                                    &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf name "option")
  (setf content (menu-item-render item))
  (setf (ref attrs :value) (princ-to-string id)) ; ???
  (setf (ref attrs :selected) (fn () (selected-in? parent item))))

(defun make-menu-items (things parent)
  (for (thing cnt) in (zip things (counter))
    collect (make-menu-item :item thing
                            :id (menu-item-value thing cnt)
                            :parent parent)))

(define-class (menu form-input) items values)

(define-method (initialize-instance (m menu name attrs content items values) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf name "select")
  (del attrs :value)
  (setf content (html-items (make-menu-items items m)))
  (setf values (for (item cnt) in (zip items (counter)) collect
                 (princ-to-string (menu-item-value item cnt))))
  (values))

(define-method (selected-in? (m menu) thing)
  (eq (value m) thing))

(define-method (value* (m menu id items values))
  (aif (getformslot id) (elt items (position it values :test 'string=))))

(defun menu (id items &rest attrs)
  (make-menu :id id :items items :attrs (apply '-> attrs)))

;;;  Multi-menu
(define-class (multi-menu menu))

(define-method (initialize-instance (mm multi-menu attrs) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf (ref attrs :multiple) t))

(define-method (selected-in? (m multi-menu) thing) (find thing (value m)))

(define-method (value* (mm multi-menu id items))
  (for i in (getformslot-multi id)
    collect (nth (parse-integer i) items)))

(defun multimenu (items &rest attrs)
  (make-multi-menu :items items :attrs (apply '-> attrs)))

;;; AJAX menu
(define-class (ajax-menu menu) object slot)

(defun ajax-menu (object slot items &rest attrs)
  (setf attrs (apply '-> attrs))
  (setf (ref attrs :class) :ajax-menu)
  (make-ajax-menu :id (format nil "~A/~A/~A" (type-of object) (id object) slot)
                  :object object :slot slot
                  :items items :attrs attrs))

(define-method (value* (m ajax-menu object slot))
  (slot-value object slot))

(define-method (selected-in? (m ajax-menu object slot) thing)
  (equal thing (slot-value object slot)))

;;; Tables
(define-class (table tag))

(define-method (initialize-instance (tbl table name) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf name "TABLE"))

(define-class (table-row tag))

(define-method (initialize-instance (tr table-row name) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf name "TR"))

(define-class (table-cell tag))

(define-method (initialize-instance (cell table-cell name) &rest args)
  (declare (ignore args))
  (call-next-method)
  (setf name "TD"))

(defun make-table (content)
  (make-instance 'table
    :content (html-items
              (for row in content collect
                (make-table-row :content
                                (html-items
                                 (for cell in row collect
                                   (make-table-cell :content cell))))))))

;;; Date input
(defv this-year (with-decoded-universal-time (now) y))

(defun next-n-years (n)
  (for y in (counter this-year (+ this-year n)) collect y))

(define-class (date-input form-input)
  (default (now))
  (day (menu "d" (for i in (counter 1 32) collect i) :class "form-control"))
  (month (menu "m" long-month-names :class "form-control"))
  (year (menu "y" (next-n-years 10) :class "form-control")))

(define-method (update-default-and-override-values (d date-input default override
                                                      day month year))
  (if default
    (with-decoded-universal-time default
      (setf (form-input-default day) d)
      (setf (form-input-default month) (ref (menu-items month) (1- mo)))
      (setf (form-input-default year) y)))
  (if override
    (with-decoded-universal-time override
      (setf (form-input-override day) d)
      (setf (form-input-override month) (ref (menu-items month) (1- mo)))
      (setf (form-input-override year) y)))
)

(define-method (initialize-instance (d date-input default day month year) &rest args &key id)
  (declare (ignore args))
  (call-next-method)
  (setf (form-input-id day) (concatenate-symbol id '-d))
  (setf (form-input-id month) (concatenate-symbol id '-mo))
  (setf (form-input-id year) (concatenate-symbol id '-y))
  (update-default-and-override-values d)
)

(define-method (html-render (d date-input day month year))
  (html-render day)
  (html-render month)
  (html-render year))

; For bootstrap:
(define-method (html-render (d date-input day month year))
  (who (:div :class "col-xs-3" (hro day))
       (:div :class "col-xs-4" (hro month))
       (:div :class "col-xs-3" (hro year))))

(define-method (value* (d date-input day month year))
  (encode-universal-time 0 0 12
                         (value day)
                         (1+ (position (value month) (menu-items month)))
                         (value year)))

;;; CSS
(defun css-render (things)
  (format nil "~&~{~A {~%~{  ~A:~A;~%~}}~%~}"  things))
