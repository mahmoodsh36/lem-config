(asdf/parse-defsystem:defsystem "lem-my-init"
  :author "mahmood sheikh"
  :license ""
  :description "my lem config."
  :depends-on ("cltpt" "organ-mode")
  :serial t
  :components ((:file "utils")
               (:file "main")
               (:file "constants")
               (:file "vi")
               (:file "cltpt")
               (:file "organ")
               (:file "theme")))