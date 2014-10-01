(define-class panel name content)

(define-method (label (p panel name)) name)

(define-method (html-render (p panel content)) (html-render content))

(defmacro panel (name &rest content)
  `(make-panel :name ,name :content (html-string (whos ,@content))))

(define-class panelgroup name panels)

(define-method (html-render (group panelgroup panels))
  (who ((:div :class "panelGroup" :id (id group))
        (for panel in panels do
          (unless (stringp panel)
            (htm ((:div :class "panel" :id (id panel) :style "display:none")
                  (hro panel))))))))

(defun panelgroup (panels)
  (make-instance 'panelgroup :name (gensym "PanelGroup") :panels panels))

(defv showpanel-code "dswi.ajax.showPanel(this)")

(define-method (menu-selector (group panelgroup name panels))
  (menu name (for p in panels collect (if (stringp p) p (list (label p) (id p))))
        :class "panelSelector" :onchange showpanel-code))

(define-method (button-selector (group panelgroup name panels))
  (html-items
   (for panel in panels collect
     (whobj ((:button :onclick showpanel-code :name name :value (id panel))
             (str (label panel)))))))

(define-method (radio-selector (group panelgroup name panels))
  (whobj
   ((:div :class "design-tabs toggle-group")
    (for panel in panels do
      (bb id (gensym)
          (htm (:input :type "radio"
                       ; ID links to label, name links to panelgroup, value links to panel
                       :id id :name name :value (id panel)
                       :onchange showpanel-code)
               ((:label :for id) (str (label panel)))))))))

(defv $panels-js "
function init_panels() {
  $('input[type=radio]').eq(0).prop('checked',true);
  $('input[type=radio]').eq(0).change();
  $('select.panelSelector').change();
  $('div.btn-group').each(function(n, bg) { $(bg).find('button').eq(0).click(); });
}

$(init_panels)
  ")

(defun bootstrap-header (title)
  (who
   ((:META :CHARSET "utf-8"))
   ((:META :HTTP-EQUIV "X-UA-Compatible" :CONTENT "IE=edge"))
   ((:META :NAME "viewport" :CONTENT "width=device-width, initial-scale=1.0"))
   (:TITLE (esc title))
   (:COMMENT "Bootstrap core CSS")
   (style "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
   (script "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js")
   ((:A :CLASS "sr-only" :HREF "#content") "Skip navigation") ; For accessibility
   ))

(defun bootstrap-footer ()
  (who
   ((:FOOTER :CLASS "cd-footer" :ROLE "contentinfo")
    ((:DIV :CLASS "container")
     ((:DIV :CLASS "cd-social")
      #+nil((:UL :CLASS "cd-social-buttons")
       (:LI
        ((:A :HREF "http://www.facebook.com/sparkinnovationsinc" :CLASS "facebook-button" :DATA-LINK-COLOR "#0069D6" :DATA-SHOW-COUNT
             "true") "Facebook"))
       (:LI
        ((:A :HREF "http://www.linkedin.com/company/spark-innovations-inc-" :CLASS "linkedin-button" :DATA-LINK-COLOR "#0069D6" :DATA-SHOW-COUNT
             "true") "LinkedIn"))
       ((:LI :CLASS "follow-btn")
        ((:A :HREF "http://twitter.com/sparkinnov8ns" :CLASS "twitter-follow-button" :DATA-LINK-COLOR "#0069D6"
             :DATA-SHOW-COUNT "true")
         "Twitter"))))
     (:P "Designed and built in the Silicon Valley by"
         ((:A :HREF "about.html" :TARGET "_blank") "Spark") ".")
     (:P "Copyright &copy; 2013, Spark Innovations Inc.")
     ((:UL :CLASS "footer-links")
;      (:LI ((:A :HREF "http://carbondata.uservoice.com/") "Feedback"))
;      ((:LI :CLASS "muted") "&middot;") (:LI ((:A :HREF "#") "Blog"))
;      ((:LI :CLASS "muted") "&middot;")
      (:LI ((:A :HREF "/terms-of-use.html") "Terms of Use"))
      ((:LI :CLASS "muted") "&middot;")
      (:LI ((:A :HREF "/privacy.html") "Privacy Policy")))))))

(defun bootstrap-js ()
  (who
   (:script :src "jquery.js")
   (:script :src "dswi.js")
   (:script "dswi.subscribe(dswi)")
   (:script :src "ajax.js")
   (:script :src "bootstrap.min.js")
   (:script (str $panels-js))))

(defmacro bootstrap-page (title &rest body)
  `(progn
     (bootstrap-header ',title)
     (bootstrap-js)
     (who ,@body)
     (bootstrap-footer)))
      
(defindent "bootstrap-page" 1)

(define-method (bootstrap-render (pg panelgroup name panels))
  (who
   ((:UL :CLASS "nav nav-tabs" :ID name)
    ((:LI :CLASS "active")
     ((:A :DATA-TOGGLE "tab" :HREF (strcat "#" (id (1st panels))))
      (esc (label (1st panels)))))
    (for panel in (rst panels) do
      (who
       (:LI ((:A :DATA-TOGGLE "tab" :HREF (strcat "#" (id panel)))
             (esc (label panel)))))))
   ((:DIV :CLASS "tab-content" :STYLE "padding:5px;")
    ((:DIV :CLASS "tab-pane active" :ID (id (1st panels)))
     (hro (1st panels)))
    (for panel in (rst panels) do
      (who
       ((:DIV :CLASS "tab-pane" :ID (id panel))
        (hro panel)))))))

(define-method (html-render (pg panelgroup)) (bootstrap-render pg))

(defmacro bs-modal-dialog (label title &body content)
  `(bb label ,label
       title ,title
       id (gensym "MODAL-")
       aria-id (gensym "MODAL-ARIA-")
       (who
        ((:A :DATA-TOGGLE "modal" :HREF (strcat "#" id) :CLASS
             "btn btn-lg btn-primary") (esc label))
        ((:DIV :CLASS "modal fade" :ID id :TABINDEX "-1" :ROLE "dialog"
               :ARIA-LABELLEDBY aria-id :ARIA-HIDDEN "true")
         ((:DIV :CLASS "modal-dialog")
          ((:DIV :CLASS "modal-content")
           ((:DIV :CLASS "modal-header")
            ((:BUTTON :TYPE "button" :CLASS "close" :DATA-DISMISS "modal" :ARIA-HIDDEN
                      "true") "&times;")
            ((:H4 :ID aria-id :CLASS "modal-title")
             (esc title)))
           ((:DIV :CLASS "modal-body") ,@content)
           ((:DIV :CLASS "modal-footer")
            ((:BUTTON :TYPE "button" :CLASS "btn btn-default" :DATA-DISMISS "modal")
             "Dismiss"))))))))

(defindent "bs-modal-dialog" 2)

(defmacro bs-modal-form (method action label title &body content)
  `(bb label ,label
       title ,title
       id (gensym "MODAL-")
       aria-id (gensym "MODAL-ARIA-")
       (who
        ((:button :DATA-TOGGLE "modal" :HREF (strcat "#" id) :CLASS "btn btn-sm")
         (esc label))
        ((:DIV :CLASS "modal fade" :ID id :TABINDEX "-1" :ROLE "dialog"
               :ARIA-LABELLEDBY aria-id :ARIA-HIDDEN "true")
         ((:DIV :CLASS "modal-dialog")
          ((:DIV :CLASS "modal-content")
           ((:form :method ,method :action ,action)
            ((:DIV :CLASS "modal-header")
             ((:BUTTON :TYPE "button" :CLASS "close" :DATA-DISMISS "modal" :ARIA-HIDDEN
                       "true") "&times;")
             ((:H4 :ID aria-id :CLASS "modal-title")
              (esc title)))
            ((:DIV :CLASS "modal-body") ,@content)
            ((:DIV :CLASS "modal-footer")
             ((:BUTTON :TYPE "button" :CLASS "btn btn-default" :DATA-DISMISS "modal")
              "Cancel")
             ((:INPUT :TYPE "SUBMIT" :CLASS "btn btn-primary"))))))))))

(defindent "bs-modal-form" 4)

(defun bs-menu (id items)
  (who
   ((:SELECT :CLASS "form-control" :NAME id)
    (for item in items do
      (who ((:option :value (id item))
            (esc (label item))))))))

(defun bs-radio-buttons (id items)
  (for item in items do
    (who
     ((:DIV :CLASS "input-group")
      ((:SPAN :CLASS "input-group-addon")
       ((:INPUT :TYPE "radio" :NAME id :VALUE (id item) :ID (id item))))
      ((:LABEL :CLASS "form-control" :FOR (id item)) (esc (label item)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Experiments

(defpage "/bs-test"
  (bootstrap-page "test"
    (bootstrap-render
     (panelgroup
      (list (panel "Panel 1"
                   (bs-menu "m1" '("item1" "item2" "item3"))
                   (bs-menu "m2" '(one two three))
                   )
            (panel "Panel 2"
                   (bs-menu "m3" '("item1" "item2" "item3"))
                   (bs-radio-buttons "rb1" '(foo baz bar)))
            (panel "Panel 3"
                   (bootstrap-render
                    (panelgroup
                     (list (panel "Panel 3a" "Panel 3a content")
                           (panel "Panel 3b" "Panel 3b content")
                           (panel "Panel 3c" "Panel 3c content"))))))))
    (bs-modal-form :post "/show-form-params" "Dialog1" "Test Dialog One"
      (bootstrap-render
       (panelgroup
        (list (panel "Panel 1x"
                     (bs-menu "m1" '("item1" "item2" "item3"))
                     (bs-menu "m2" '(one two three))
                     )
              (panel "Panel 2x"
                     (bs-menu "m3" '("item1" "item2" "item3"))
                     (bs-radio-buttons "rb1" '(foo baz bar)))
              (panel "Panel 3x"
                     (bootstrap-render
                      (panelgroup
                       (list (panel "Panel 3xa" "Panel 3a content")
                             (panel "Panel 3xb" "Panel 3b content")
                             (panel "Panel 3xc" "Panel 3c content")))))))))))

(defpage "/panel-test"
  (bb pg (panelgroup (list (panel "p1" "content1") (panel "p2" "content2")))
      (who (hro (button-selector pg))
           (hro pg)
           (:script :src "jquery.js")
           (:script :src "dswi.js")
           (:script "dswi.subscribe(dswi)")
           (:script :src "ajax.js")
           (:script (str $panels-js)))))

(defpage "/show-form-params" (str (get-form-parameters)))
