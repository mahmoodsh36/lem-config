(asdf/parse-defsystem:defsystem "lem-my-init"
  :author "mahmood sheikh"
  :license ""
  :description "my lem config."
  :depends-on ("cltpt" "organ-mode")
  :components ((:file "utils")
               (:file "main")
               (:file "organ")
               (:file "theme")))