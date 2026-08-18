(in-package :lemetnal)

;; organ config
(setf organ:*organ-files*
      `((:path (,(cltpt/file-utils:as-dir-path
                  (cltpt/file-utils:join-paths
                   *brain-dir*
                   "notes")))
         :glob "*.org"
         :format "org-mode")))

(setf organ/capture:*organ-capture-templates*
      `((:key #\d
         :name "default"
         :dir ,*daily-dir*
         :if-new "#+title:
#+date: %(organ/utils:format-timestamp)"
         :entry "
* TODO %organ/capture::cursor"
         :filename "todos.org")
        (:key #\n
         :name "new note"
         :dir ,*notes-dir*
         :if-new "#+title:
#+date: %(organ/utils:format-timestamp)
#+filetags: :note:
#+identifier: %(organ/capture:unique-timestamp \"notes/\")"
         :filename "%(organ/capture:unique-timestamp \"notes/\").org")))

;; only show first instance of a repeated task
(setf organ:*agenda-first-repeat-only* t)

(led-key
 "E"
 (cmd
  (find-file
   (format nil
           "~A/cltpt/tests/test.org"
           (my-getenv "WORK_DIR")))))

(lem:define-command books-find () ()
  (if organ:*organ-files*
      (let* ((rmr (cltpt/roam:roamer-from-files organ:*organ-files*))
             (book-nodes
               (remove-if-not
                (lambda (node)
                  (let ((text-obj (cltpt/roam:node-text-obj node)))
                    (and text-obj
                         (cltpt/base:alist-get
                          (cltpt/base:text-object-property text-obj :keywords-alist)
                          "book_title"))))
                (cltpt/roam:roamer-nodes rmr)))
             (items
               (loop for node in book-nodes
                     for text-obj = (cltpt/roam:node-text-obj node)
                     for kw-alist = (cltpt/base:text-object-property text-obj :keywords-alist)
                     for book-title = (cltpt/base:alist-get kw-alist "book_title")
                     for book-author = (cltpt/base:alist-get kw-alist "book_author")
                     for book-year = (cltpt/base:alist-get kw-alist "book_year")
                     collect (lem/completion-mode:make-completion-item
                              :label book-title
                              :detail (format nil
                                              "~{~A~^  ~}"
                                              (remove nil
                                                      (list book-author
                                                            book-year
                                                            (file-namestring
                                                             (cltpt/roam:node-file node)))))))))
        (if items
            (let* ((choice-str
                     (lem:prompt-for-string
                      "books-find: "
                      :completion-function (lambda (x)
                                             (lem:completion-strings
                                              x
                                              items
                                              :key #'lem/completion-mode:completion-item-label))))
                   (choice-idx (position choice-str
                                         items
                                         :key #'lem/completion-mode:completion-item-label
                                         :test #'string=))
                   (choice (elt book-nodes choice-idx)))
              (let ((buffer (lem:find-file-buffer (cltpt/roam:node-file choice))))
                (lem:switch-to-buffer buffer)))
            (lem:message "no books found.")))
      (lem:message "you must customize *organ-files* first.")))

(led-key
 "r"
 organ:*organ-keymap*)
(led-key "r b" 'books-find)
(led-key "r h" 'organ/organ-mode:organ-open-attach-dir)

(setf organ/organ-mode:*organ-latex-preview-auto* t)

(setf cltpt/latex-previews:*latex-preview-preamble*
      (concatenate
       'string
       cltpt/latex-previews:*latex-preview-preamble*
       "\\usepackage{\\string~/.emacs.d/common}\\usepackage{\\string~/.emacs.d/private}"))

;; prevent transient popup for organ-mode
(defmethod lem/transient:mode-transient-keymap ((mode organ/organ-mode::organ-mode))
  nil)