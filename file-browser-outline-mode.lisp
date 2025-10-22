(defpackage :file-browser-outline-mode
  (:use :cl :lem)
  (:export :file-browser-outline-mode
           :open-file-browser
           :file-browser-open
           :file-browser-open-in-new-window
           :file-browser-delete
           :file-browser-rename
           :file-browser-new-file
           :file-browser-refresh
           :file-browser-parent-directory

           :file-browser-quit
           :test-file-browser
           :test-file-browser-home
           :test-file-browser-with-test-files))

(in-package :file-browser-outline-mode)

(defvar *file-browser-outline-keymap*
  (lem:make-keymap :name '*file-browser-outline-keymap*
                   :parent organ/outline-mode:*outline-mode-keymap*))

;;; Define custom key bindings
(lem:define-key *file-browser-outline-keymap* "Return" 'file-browser-open)
(lem:define-key *file-browser-outline-keymap* "C-o" 'file-browser-open-in-new-window)
(lem:define-key *file-browser-outline-keymap* "d" 'file-browser-delete)
(lem:define-key *file-browser-outline-keymap* "r" 'file-browser-rename)
(lem:define-key *file-browser-outline-keymap* "n" 'file-browser-new-file)
(lem:define-key *file-browser-outline-keymap* "g" 'file-browser-refresh)
(lem:define-key *file-browser-outline-keymap* "-" 'file-browser-parent-directory)
(lem:define-key *file-browser-outline-keymap* "C-c C-c" 'organ/outline-mode:outline-expand-collapse)
(lem:define-key *file-browser-outline-keymap* "q" 'file-browser-quit)

;;; Define the inherited mode - this is a macro and must be at top-level
(lem:define-major-mode file-browser-outline-mode organ/outline-mode:outline-mode
    (:name "file-browser-outline"
     :keymap *file-browser-outline-keymap*)
  ;; Make buffer writable for file operations
  (setf (buffer-read-only-p (lem:current-buffer)) nil)
  ;; Set up file browser specific buffer values
  (setf (buffer-value (lem:current-buffer) 'current-directory)
        (uiop:getcwd)))

;;; Custom commands for file browser

(lem:define-command file-browser-open () ()
  "Open the file or directory at point."
  (let ((node (organ/outline-mode:node-at-point (lem:current-point))))
    (when (typep node 'file-browser-node)
      (case (file-browser-node-file-type node)
        (:file (find-file (file-browser-node-file-path node)))
        (:directory (file-browser-change-directory (file-browser-node-file-path node)))
        (otherwise (lem:message "Unknown node type: ~A" (file-browser-node-file-type node)))))))

(lem:define-command file-browser-open-in-new-window () ()
  "Open the file at point in a new window."
  (let ((node (organ/outline-mode:node-at-point (lem:current-point))))
    (when (and (typep node 'file-browser-node) 
               (eq (file-browser-node-file-type node) :file))
      ;; This would need window management functions
      (lem:message "Would open ~A in new window" (file-browser-node-file-path node)))))

(lem:define-command file-browser-delete () ()
  "Delete the file or directory at point."
  (let ((node (organ/outline-mode:node-at-point (lem:current-point))))
    (when (typep node 'file-browser-node)
      (let ((path (file-browser-node-file-path node)))
        (when (file-browser-y-or-n-p "Delete ~A? " path)
          (case (file-browser-node-file-type node)
            (:file (delete-file path))
            (:directory (uiop:delete-directory-tree path :validate t)))
          (file-browser-refresh))))))

(lem:define-command file-browser-rename () ()
  "Rename the file or directory at point."
  (let ((node (organ/outline-mode:node-at-point (lem:current-point))))
    (when (typep node 'file-browser-node)
      (let ((old-path (file-browser-node-file-path node))
            (new-name (lem:prompt-for-string "New name: "
                                         :initial-value (file-browser-node-file-name node))))
        (when new-name
          (let ((new-path (merge-pathnames new-name
                                           (directory-namestring old-path))))
            (rename-file old-path new-path)
            (file-browser-refresh)))))))

(lem:define-command file-browser-new-file () ()
  "Create a new file in the current directory."
  (let ((filename (lem:prompt-for-string "New file name: ")))
    (when filename
      (let ((current-dir (buffer-value (lem:current-buffer) 'current-directory))
            (new-path (merge-pathnames filename current-dir)))
        (with-open-file (stream new-path :direction :output :if-exists :error)
          (format stream ""))
        (file-browser-refresh)))))

(lem:define-command file-browser-refresh () ()
  "Refresh the file browser display."
  (let ((buffer (lem:current-buffer))
        (current-dir (buffer-value buffer 'current-directory)))
    (let ((forest (build-file-forest current-dir)))
      (organ/outline-mode:set-outline-forest buffer forest)
      (organ/outline-mode:render-outline buffer forest))))

(lem:define-command file-browser-parent-directory () ()
  "Go to the parent directory."
  (let ((current-dir (buffer-value (lem:current-buffer) 'current-directory)))
    (let ((parent-dir (directory-namestring current-dir)))
      (when (and parent-dir (not (string= parent-dir "")))
        (file-browser-change-directory parent-dir)))))

(lem:define-command file-browser-quit () ()
  "Quit the file browser."
  (lem:kill-buffer (lem:current-buffer)))

(defclass file-browser-node ()
  ((file-type :initarg :file-type
              :accessor file-browser-node-file-type
              :type (member :file :directory)
              :initform :file)
   (file-name :initarg :file-name
              :accessor file-browser-node-file-name
              :type string)
   (file-path :initarg :file-path
              :accessor file-browser-node-file-path
              :type string)
   (children :initarg :children
             :accessor file-browser-node-children
             :initform nil
             :type list)
   (expanded :initarg :expanded
             :accessor file-browser-node-expanded-p
             :initform nil
             :type boolean)
   (parent :initarg :parent
           :accessor file-browser-node-parent
           :initform nil)))

;;; Implement cltpt/tree interface for file-browser-node
(defmethod cltpt/tree:tree-value ((node file-browser-node))
  "Return the value of the file browser node."
  node)

(defmethod cltpt/tree:tree-children ((node file-browser-node))
  "Return the children of the file browser node."
  (file-browser-node-children node))

(defmethod cltpt/tree:tree-parent ((node file-browser-node))
  "Return the parent of the file browser node."
  (file-browser-node-parent node))

;;; Implement cltpt/tree/outline interface for file-browser-node
(defmethod cltpt/tree/outline:should-expand ((node file-browser-node))
  "Check if the file browser node should be expanded."
  (file-browser-node-expanded-p node))

(defmethod cltpt/tree/outline:could-expand ((node file-browser-node))
  "Check if the file browser node could be expanded."
  (eq (file-browser-node-file-type node) :directory))

(defmethod cltpt/tree/outline:outline-text ((node file-browser-node))
  "Return the display text for file browser nodes."
  (file-browser-node-file-name node))

;;; Custom outline-mode-toggle for file browser nodes
(defmethod organ/outline-mode:outline-mode-toggle ((node file-browser-node))
  "Toggle expansion for file browser nodes, loading children on demand."
  (when (eq (file-browser-node-file-type node) :directory)
    ;; Load children on demand before expanding
    (when (and (null (file-browser-node-children node))
               (not (file-browser-node-expanded-p node)))
      (let ((dir-path (file-browser-node-file-path node)))
        (let ((children (build-file-forest dir-path)))
          ;; Set parent relationships for children
          (dolist (child children)
            (setf (file-browser-node-parent child) node))
          (setf (file-browser-node-children node) children))))
    ;; Toggle the expansion state
    (setf (file-browser-node-expanded-p node)
          (not (file-browser-node-expanded-p node)))))

;;; Custom interactive-render-node for file browser nodes
(defmethod organ/outline-mode:interactive-render-node ((node file-browser-node) buffer point depth click-handler)
  "Render file browser nodes with clean display names."
  (let* ((display-name (file-browser-node-file-name node))
         (indent (make-string (* depth 2) :initial-element #\space))
         (line-start-pos (lem:copy-point point :right-inserting)))
    (lem:insert-string point indent)
    ;; add the tree connector symbol based on whether node has children
    (if (cltpt/tree/outline:could-expand node)
        (lem:insert-string point
                           (if (cltpt/tree/outline:should-expand node)
                               "▼ "    ;; down arrow for expanded
                               "▶ "))  ;; right arrow for collapsed
        (lem:insert-string point "  ")) ;; indentation for leaf
    ;; insert the node text with clean display name
    (lem:insert-string point display-name)
    (let ((node-end-pos (lem:copy-point point :left-inserting))) ;; save position before newline
      (lem:insert-character point #\newline)
      ;; set properties for the line
      (lem:put-text-property line-start-pos node-end-pos
                             :clickable click-handler)
      (lem:put-text-property line-start-pos node-end-pos
                             :outline-node node))))

;;; Helper functions

(defun file-browser-change-directory (directory)
  "Change to the specified directory."
  (when (uiop/filesystem:directory-exists-p directory)
    (setf (buffer-value (lem:current-buffer) 'current-directory) directory)
    (file-browser-refresh)))

(defun build-file-forest (directory)
  "Build an outline forest from the directory contents."
  (when (uiop/filesystem:directory-exists-p directory)
    (handler-case
        (let* ((files (uiop/filesystem:directory-files directory))
               (dirs (uiop:subdirectories directory))
               (all-entries (append files dirs))
               (entries (sort all-entries #'string< :key #'namestring)))
          (mapcar (lambda (entry)
                    (let ((namestring (enough-namestring entry directory)))
                      (if (uiop/filesystem:directory-exists-p entry)
                          (make-instance 'file-browser-node
                                         :file-type :directory
                                         :file-name namestring
                                         :file-path (namestring entry)
                                         :children nil
                                         :expanded nil)
                          (make-instance 'file-browser-node
                                         :file-type :file
                                         :file-name namestring
                                         :file-path (namestring entry)))))
                  entries))
      (error (e)
        (lem:message "Error reading directory: ~A" e)
        nil))))

(defun file-browser-y-or-n-p (format-string &rest arguments)
  "Simple y-or-n-p implementation for file browser."
  (let ((response (lem:prompt-for-string (apply #'format nil format-string arguments)
                                    :completion-function
                                    (lambda (input)
                                      (when (member input '("y" "n" "yes" "no") 
                                                    :test #'string-equal)
                                        (list input))))))
    (member (string-downcase response) '("y" "yes"))))

(defun setup-test-environment ()
  "Create a test directory structure for demonstration."
  (let ((test-dir (merge-pathnames "lem-test-files/" (user-homedir-pathname))))
    (ensure-directories-exist test-dir)

    ;; Create some test files
    (with-open-file (f (merge-pathnames "test1.txt" test-dir) :direction :output)
      (format f "This is test file 1"))
    (with-open-file (f (merge-pathnames "test2.txt" test-dir) :direction :output)
      (format f "This is test file 2"))
    (with-open-file (f (merge-pathnames "lisp-test.lisp" test-dir) :direction :output)
      (format f "(defun hello ()~%  (format t \"Hello World!~%\"))"))

    ;; Create subdirectories
    (let ((subdir (merge-pathnames "subdir/" test-dir)))
      (ensure-directories-exist subdir)
      (with-open-file (f (merge-pathnames "subfile.txt" subdir) :direction :output)
        (format f "This is in a subdirectory")))

    (format t "Test environment created at: ~A~%" test-dir)
    test-dir))

;;; Main function to open file browser

(defun open-file-browser (&optional (directory (uiop/os:getcwd)))
  "Open a file browser outline for the specified directory."
  (let ((buffer (lem:make-buffer "*file-browser*")))
    (lem:change-buffer-mode buffer 'file-browser-outline-mode)
    (setf (buffer-value buffer 'current-directory) directory)
    (let ((forest (build-file-forest directory)))
      (organ/outline-mode:set-outline-forest buffer forest)
      (organ/outline-mode:render-outline buffer forest))
    (lem:switch-to-buffer buffer)
    buffer))

;;; Test commands for M-x execution

(lem:define-command test-file-browser () ()
  "Test command to open file browser in current directory."
  (file-browser-outline-mode:open-file-browser (uiop/os:getcwd)))

(lem:define-command test-file-browser-home () ()
  "Test command to open file browser in home directory."
  (file-browser-outline-mode:open-file-browser (user-homedir-pathname)))

(lem:define-command test-file-browser-with-test-files () ()
  "Test command to open file browser with test files."
  (let ((test-dir (file-browser-outline-mode::setup-test-environment)))
    (file-browser-outline-mode:open-file-browser test-dir)))