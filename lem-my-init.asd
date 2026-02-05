(asdf/parse-defsystem:defsystem "lem-my-init"
  :author "mahmood sheikh"
  :license ""
  :description "my lem config."
  :depends-on ("cltpt")
  :components ((:file "main")
               (:file "organ")))