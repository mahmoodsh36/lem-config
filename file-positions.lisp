(in-package :lemetnal)

;; file-positions: persist cursor (line + charpos) per file
;; saves on kill-buffer and exit, restores on find-file.

(defvar *file-positions-file*
  (merge-pathnames "file-positions.sexp" (lem:lem-home))
  "path to persist per-file cursor positions.")

(defvar *file-positions*
  nil
  "hash table mapping filename to (line charpos).")

(defun file-positions-table ()
  "return positions hash table, loading from disk if needed."
  (or *file-positions*
      (setf *file-positions*
            (handler-case
                (with-open-file (in *file-positions-file* :if-does-not-exist nil)
                  (if (null in)
                      (make-hash-table :test 'equal)
                      (let ((data (read in nil))
                            (ht (make-hash-table :test 'equal)))
                        (when (listp data)
                          (dolist (entry data)
                            (destructuring-bind (path line charpos) entry
                              (setf (gethash path ht) (list line charpos)))))
                        ht)))
              (error (c)
                (format *error-output* "file-positions: load failed ~A~%" c)
                (make-hash-table :test 'equal))))))

(defun save-file-positions-to-disk ()
  (when *file-positions*
    (handler-case
        (progn
          (ensure-directories-exist *file-positions-file*)
          (with-open-file (out *file-positions-file*
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (format out "(")
            (maphash (lambda (k v)
                       (format out "~S~%" (list k (first v) (second v))))
                     *file-positions*)
            (format out ")")))
      (error (c)
        (format *error-output* "file-positions: save failed ~A~%" c)))))

(defun save-file-position-for-buffer (buffer)
  "capture BUFFER's point into hash. called from kill-buffer-hook / exit."
  (ignore-errors
   (let ((filename (lem:buffer-filename buffer)))
     (when filename
       (let ((key (coerce (namestring filename) '(simple-array character (*))))
             (point (lem:buffer-point buffer)))
         (when point
           (let ((line (lem:line-number-at-point point))
                 (charpos (lem:point-charpos point)))
             (setf (gethash key (file-positions-table)) (list line charpos)))))))))

(defun save-all-file-positions ()
  "save positions for all live file buffers and flush."
  (dolist (buffer (lem:buffer-list))
    (save-file-position-for-buffer buffer))
  (save-file-positions-to-disk))

(defun restore-file-position-for-buffer (buffer)
  "restore cursor for BUFFER if we have a saved entry. called from *find-file-hook*."
  (ignore-errors
   (let ((filename (lem:buffer-filename buffer)))
     (when filename
       (let* ((key (coerce (namestring filename) '(simple-array character (*))))
              (entry (gethash key (file-positions-table))))
         (when entry
           (destructuring-bind (line charpos) entry
             (let ((point (lem:buffer-point buffer))
                   (nlines (lem:buffer-nlines buffer)))
               (setf line (min line nlines))
               (when (lem:move-to-line point line)
                 (let ((len (length (lem:line-string point))))
                   (setf (lem:point-charpos point) (min charpos len))))))))))))

;; hooks
(add-hook *find-file-hook* 'restore-file-position-for-buffer)
(add-hook (variable-value 'kill-buffer-hook :global) 'save-file-position-for-buffer)
(add-hook *exit-editor-hook* 'save-all-file-positions)
(add-hook *after-init-hook* (lambda () (file-positions-table)) 0)