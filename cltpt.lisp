(in-package :lemetnal)

;; set my custom org-attach file-to-id function
(defun my-id-to-attach-dir (src-file id)
  (cltpt/file-utils:join-paths
   (cltpt/file-utils:file-dirpath src-file)
   "data"
   id))

(setf cltpt/base:*id-to-attach-dir-func* 'my-id-to-attach-dir)

;; make :defines work like :title for blocks
(defmethod cltpt/base:text-object-init :around ((obj cltpt/org-mode:org-block) str1 match)
  (call-next-method)
  (let ((defines (cltpt/org-mode::org-block-keyword-value obj "defines")))
    (when defines
      (let ((node (cltpt/base:text-object-property obj :roam-node)))
        (if node
            (unless (cltpt/roam:node-title node)
              (setf (cltpt/roam:node-title node) defines))
            (setf (cltpt/base:text-object-property obj :roam-node)
                  (cltpt/roam:make-node
                   :id nil
                   :title defines
                   :desc nil
                   :text-obj obj)))))))