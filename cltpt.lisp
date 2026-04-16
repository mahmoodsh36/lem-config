(in-package :lemetnal)

;; set my custom org-attach file-to-id function
(defun my-id-to-attach-dir (src-file id)
  (cltpt/file-utils:join-paths
   (cltpt/file-utils:file-dirpath src-file)
   "data"
   id))

(setf cltpt/base:*id-to-attach-dir-func* 'my-id-to-attach-dir)