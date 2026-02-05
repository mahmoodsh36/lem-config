(in-package :lem-user)

(log:info "got here123")

(defun from-work (path)
  (concatenate 'string
               (sb-ext:posix-getenv "WORK_DIR")
               "/"
               path))

;; organ config
(setf organ:*organ-files*
      `((:path (,(cltpt/file-utils:as-dir-path
                  (cltpt/file-utils:join-paths
                   (or (sb-ext:posix-getenv "BRAIN_DIR")
                       (error "couldnt get BRAIN_DIR env var"))
                   "notes")))
         :glob "*.org"
         :format "org-mode")))

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