(in-package :lemetnal)

;; set my custom org-attach file-to-id function
(defun my-id-to-attach-dir (src-file id)
  (cltpt/file-utils:join-paths
   (cltpt/file-utils:file-dirpath src-file)
   "data"
   id))

(setf cltpt/base:*id-to-attach-dir-func* 'my-id-to-attach-dir)

(defun titles-from-alist (alist)
  (let ((res))
    (dolist (pair alist)
      (when (or (string-equal (car pair) "title")
                (string-equal (car pair) "alias")
                (string-equal (car pair) "defines"))
        (push (cdr pair) res)))
    (nreverse res)))

;; make :defines/:alias work same as :title for blocks
(defmethod cltpt/base:text-object-init :around ((obj cltpt/org-mode:org-block) str1 match)
  (call-next-method)
  (let* ((alist (cltpt/base:text-object-property obj :keywords-alist))
         (terms (titles-from-alist alist)))
    (when terms
      (let ((node (cltpt/base:text-object-property obj :roam-node)))
        (if node
            (setf (cltpt/roam:node-titles node) terms)
            (setf (cltpt/base:text-object-property obj :roam-node)
                  (cltpt/roam:make-node
                   :id nil
                   :titles terms
                   :desc nil
                   :text-obj obj)))))))

;; make #+alias work like #+title for documents
(defmethod cltpt/base:text-object-finalize :around ((obj cltpt/org-mode::org-document))
  (call-next-method)
  (let* ((keywords (cltpt/base:text-object-property obj :keywords-alist))
         (all-terms (titles-from-alist keywords))
         (title (cltpt/base:text-object-property obj :title))
         (node (cltpt/base:text-object-property obj :roam-node)))
    (when all-terms
      (if (null title)
          (progn
            (setf (cltpt/base:text-object-property obj :title) (car all-terms))
            (if node
                (setf (cltpt/roam:node-titles node) all-terms)
                (setf (cltpt/base:text-object-property obj :roam-node)
                      (cltpt/roam:make-node :id nil :titles all-terms :desc nil :text-obj obj))))
          (when node
            (setf (cltpt/roam:node-titles node) all-terms))))))