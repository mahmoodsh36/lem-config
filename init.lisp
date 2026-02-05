(let ((asdf:*central-registry* (cons #P"~/.lem/" asdf:*central-registry*)))
  (asdf:load-system :lem-my-init))