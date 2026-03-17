(let ((asdf:*central-registry* (cons #P"~/.lem/" asdf:*central-registry*)))
  (asdf:load-system :lem-my-init))

;; load extensions from extra/
(let ((asdf:*central-registry* (cons #P"~/.lem/extra/" asdf:*central-registry*)))
  (handler-case (asdf:load-system :lem-extra)
    (error (c)
      (format t "lem-extra failed to load: ~A~%" c))))