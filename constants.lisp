(in-package :lemetnal)

(defparameter *brain-dir*
  (lemetnal/utils:require-env "BRAIN_DIR"))

(defparameter *work-dir*
  (lemetnal/utils:require-env "WORK_DIR"))

(defparameter *volume-dir*
  (lemetnal/utils:require-env "VOL_DIR"))

(defparameter *home-dir*
  (lemetnal/utils:require-env "HOME"))

(defparameter *notes-dir*
  (cltpt/file-utils:as-dir-path
   (cltpt/file-utils:join-paths
    *brain-dir*
    "notes")))

(defparameter *daily-dir*
  (cltpt/file-utils:as-dir-path
   (cltpt/file-utils:join-paths
    *brain-dir*
    "daily")))