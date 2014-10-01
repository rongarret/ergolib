
(require :ergolib)
(require :cl-html-parse)

(import 'html-parse:parse-html)

(defmethod parse-html ((p pathname) &key callback-only callbacks collect-rogue-tags
                       no-body-tags parse-entities)
  (with-open-file (f p)
    (parse-html f
                :callback-only callback-only :callbacks callbacks
                :collect-rogue-tags collect-rogue-tags
                :no-body-tags no-body-tags
                :parse-entities parse-entities
                )))

(defun tree-search (tree str &optional (n 0))
  (mcond (symbolp tree) nil
         (stringp tree) (search str tree)
         (tree-search (car tree) str) (cons n it)
         t (tree-search (cdr tree) str (1+ n))))

(defun indexed-tree-walk (fn tree &optional index (cdr-count 0))
  (funcall fn tree index)
  (when (consp tree)
    (indexed-tree-walk fn (car tree) (cons cdr-count index) 0)
    (indexed-tree-walk fn (cdr tree) index (1+ cdr-count))))

(defun tree-search-all (tree str)
  (with-collector collect
    (indexed-tree-walk
     (fn (tree index)
       (if (and (stringp tree) (search str tree))
         (collect (cons tree (reverse index)))))
     tree)))

(defun tree-search-for-tag (tree tag &key (test 'id=) (key 'identity))
  (with-collector collect
    (indexed-tree-walk
     (fn (tree index)
       (if (and (consp tree) (funcall test tag (funcall key (car tree))))
         (collect (cons tree (reverse index)))))
     tree)))

(defun refchain (tree indices)
  (reduce (fn (tree index) (ref tree index)) indices :initial-value tree))
