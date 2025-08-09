(defpackage :my-buffer
  (:use :cl :lem :lem-sdl2 :lem-sdl2/graphics))
(in-package :my-buffer)

(defclass my-buffer (text-buffer) ())

(defmethod render :before (texture window (buffer my-buffer))
  (sdl2:set-render-target (current-renderer) texture)
  (lem-sdl2/display::set-render-color lem-sdl2/display::*display* (lem-sdl2/display::display-background-color lem-sdl2/display::*display*))
  (sdl2:with-rects ((rect 0
                          0
                          (* (lem-sdl2::char-width)
                             (window-width window))
                          (* (lem-sdl2::char-height)
                             (1- (window-height window)))))
    (sdl2:render-fill-rect (current-renderer) rect)))

(defun open-my-buffer (pathname)
  (let ((image (load-image "/home/mahmooz/dl/icon-for-lem.png"))
        (buffer (lem:make-buffer (file-namestring pathname)
                                 :directory (expand-file-name
                                             (namestring (uiop:pathname-directory-pathname pathname))))))
    (draw-image buffer image :x 0 :y 0 :width 50 :height 50)
    buffer))

(defclass my-find-file-executor (lem:find-file-executor) ())

(defmethod lem:execute-find-file ((executor my-find-file-executor) mode pathname)
  (cond ((member (pathname-type pathname)
                 '("org")
                 :test #'equal)
         (open-my-buffer pathname))
        (t
         (call-next-method))))

;; (setf lem:*find-file-executor* (make-instance 'my-find-file-executor))