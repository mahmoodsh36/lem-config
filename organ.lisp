(in-package :lemetnal)

(defun from-work (path)
  (concatenate 'string
               (my-getenv "WORK_DIR")
               "/"
               path))

(defun open-dir (path)
  (lem:switch-to-buffer
   (lem:find-file-buffer
    (uiop:ensure-directory-pathname path))))

;; organ config
(setf organ:*organ-files*
      `((:path (,(cltpt/file-utils:as-dir-path
                  (cltpt/file-utils:join-paths
                   *brain-dir*
                   "notes")))
         :glob "*.org"
         :format "org-mode")))

(setf organ:*agenda-first-repeat-only* t)

;; (setf organ:*organ-files*
;;       `((:path (,(cltpt/file-utils:as-dir-path
;;                   (cltpt/file-utils:join-paths
;;                    (or (sb-ext:posix-getenv "WORK_DIR")
;;                        (error "couldnt get BRAIN_DIR env var"))
;;                    "cltpt/tests/")))
;;          :glob "*.org"
;;          :format "org-mode")))

(led-key
 "E"
 (cmd
  (find-file
   (format nil
           "~A/cltpt/tests/test.org"
           (my-getenv "WORK_DIR")))))

(lem:define-command open-brain-dir () ()
  (open-dir *brain-dir*))

(lem:define-command open-brain-notes-dir () ()
  (open-dir
   (cltpt/file-utils:join-paths
    *brain-dir*
    "notes")))

(lem:define-command open-volume-othermusic-dir () ()
  (open-dir
   (cltpt/file-utils:join-paths
    (require-env "VOLUME_DIR")
    "othermusic")))

(lem:define-command open-volume-music-dir () ()
  (open-dir
   (cltpt/file-utils:join-paths
    (require-env "VOLUME_DIR")
    "music")))

(lem:define-command open-home-dir () ()
  (open-dir
   (or (my-getenv "HOME")
       (error "couldnt get HOME_DIR or HOME env var"))))

;; (defmethod asdf:perform :after ((op asdf:load-op)
;;                                 (system asdf:system))
;;   (when (string= (asdf:component-name system) "organ-mode")
;;     (let ((pkg (find-package "ORGAN")))
;;       (when pkg
;;         ;; (setf (symbol-value (intern "*ORGAN-FILES*" pkg))
;;         ;;       '((:path ("/home/mahmooz/brain/notes/")
;;         ;;          :regex ".*\\.org"
;;         ;;          :format "org-mode")))
;;         (setf (symbol-value (intern "*ORGAN-FILES*" pkg))
;;               `((:path (,(from-work "cltpt/tests/test2.org")
;;                         ,(from-work "cltpt/tests/test.org"))
;;                  :regex ".*\\.org"
;;                  :format "org-mode")))
;;         ))))

;; (setf organ:*organ-files*
;;       `((:path (,(from-work "cltpt/tests/test2.org")
;;                 ,(from-work "cltpt/tests/test.org"))
;;          :regex ".*\\.org"
;;          :format "org-mode")))

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