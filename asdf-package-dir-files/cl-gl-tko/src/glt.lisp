(defpackage #:glt
  (:use :common-lisp :cl-user :sb-ext :simple-tk :tk-user :cffi :cl-opengl)
  (:shadowing-import-from :tk-user
			  :scale)
  (:export :window-swap-buffers
	   :window-gl-set-current
           :swap-all-buffers
	   :window-bind-gl))

(in-package #:glt)

(define-foreign-library user32 (:windows "user32.dll"))
(define-foreign-library gdi32  (:windows "gdi32.dll"))
(use-foreign-library user32)
(use-foreign-library gdi32)

(defvar *hdc-ptrs* (make-hash-table :test #'equal))
(defvar *rc-ptr*   nil)

(defcfun ("GetDC" get-dc)
    :int
  (hwnd :int))

(defcfun ("ChoosePixelFormat" choose-pixel-format)
    :int
  (hdc  :int)
  (ppfd :pointer))

(defcfun ("SetPixelFormat" set-pixel-format)
    :boolean
  (hdc  :int)
  (fmt  :int)
  (ppfd :pointer))

(defcfun ("SwapBuffers" swap-buffers)
    :boolean
  (hdc :int))

(define-foreign-library opengl (:windows "opengl32.dll"))
(use-foreign-library opengl)

(defcfun ("wglCreateContext" wgl-create-context)
    :int
  (arg :int))

(defcfun ("wglMakeCurrent" wgl-make-current)
    :boolean
  (dc :int)
  (rc :int))

;;; matrixmode
(defconstant +GL_MODELVIEW+              #x00001700)
(defconstant +GL_PROJECTION+             #x00001701)
(defconstant +GL_TEXTURE+                #x00001702)

;;; enum PFDFlags
(defconstant +PFD_DOUBLEBUFFER+          #x00000001)
(defconstant +PFD_STEREO+                #x00000002)
(defconstant +PFD_DRAW_TO_WINDOW+        #x00000004)
(defconstant +PFD_DRAW_TO_BITMAP+        #x00000008)
(defconstant +PFD_SUPPORT_GDI+           #x00000010)
(defconstant +PFD_SUPPORT_OPENGL+        #x00000020)
(defconstant +PFD_GENERIC_FORMAT+        #x00000040)
(defconstant +PFD_NEED_PALETTE+          #x00000080)
(defconstant +PFD_NEED_SYSTEM_PALETTE+   #x00000100)
(defconstant +PFD_SWAP_EXCHANGE+         #x00000200)
(defconstant +PFD_SWAP_COPY+             #x00000400)
(defconstant +PFD_SWAP_LAYER_BUFFERS+    #x00000800)
(defconstant +PFD_GENERIC_ACCELERATED+   #x00001000)
(defconstant +PFD_SUPPORT_DIRECTDRAW+    #x00002000)
(defconstant +PFD_DIRECT3D_ACCELERATED+  #x00004000)
(defconstant +PFD_SUPPORT_COMPOSITION+   #x00008000)
(defconstant +PFD_DEPTH_DONTCARE+        #x20000000)
(defconstant +PFD_DOUBLEBUFFER_DONTCARE+ #x40000000)
(defconstant +PFD_STEREO_DONTCARE+       #x80000000)

(defconstant +PFD_MAIN_PLANE+      0)
(defconstant +PFD_OVERLAY_PLANE+   1)
(defconstant +PFD_UNDERLAY_PLANE+ -1)

;;; enum PFDPixelType 
(defconstant +PFD_TYPE_RGBA+       0)
(defconstant +PFD_TYPE_COLORINDEX+ 1)

(defcstruct pixel-format-descriptor
  (n-size             :uint16)
  (n-version          :uint16)
  (dw-flags           :uint32)
  (i-pixel-type       :unsigned-char)
  (c-color-bits       :unsigned-char)
  (c-red-bits         :unsigned-char)
  (c-red-shift        :unsigned-char)
  (c-green-bits       :unsigned-char)
  (c-green-shift      :unsigned-char)
  (c-blue-bits        :unsigned-char)
  (c-blue-shift       :unsigned-char)
  (c-alpha-shift      :unsigned-char)
  (c-accum-bits       :unsigned-char)
  (c-accum-red-bits   :unsigned-char)
  (c-accum-green-bits :unsigned-char)
  (c-accum-blue-bits  :unsigned-char)
  (c-accum-alpha-bits :unsigned-char)
  (c-depth-bits       :unsigned-char)
  (c-stencil-bits     :unsigned-char)
  (c-aux-buffers      :unsigned-char)
  (i-layer-type       :unsigned-char)
  (b-reserved         :unsigned-char)
  (dw-layer-mask      :uint32)
  (dw-visible-mask    :uint32)
  (dw-damage-mask     :uint32))

;; (defcstruct tcl-two-pointer-struct
;;   (ptr-1 :pointer)
;;   (ptr-2 :pointer))

;; (defcstruct tcl-pointer-and-long-struct
;;   (ptr   :pointer)
;;   (value :ulong))

;; (defcunion tcl-obj-union
;;   (long-value           :ulong)
;;   (double-value         :uint32)
;;   (other-value-ptr      :pointer)
;;   (wide-value           :uint64)
;;   (two-pointer-value    (:struct tcl-two-pointer-struct))
;;   (pointer-and-long-rep (:struct tcl-pointer-and-long-struct)))

;; (defcstruct tcl-obj
;;   (ref-count    :int)
;;   (bytes        :pointer)
;;   (length       :int)
;;   (type-ptr     :pointer)
;;   (internal-rep (:union tcl-obj-union)))

(defcallback set-render-window
    :int
    ((client_data :int)
     (interp      :pointer)
     (argc        :int)
     (argv        :pointer))
  "[TCL CALLBACK] Register window display context and setup pixel format"
  (declare (ignore client_data argc))
  
  ;;; Get & hash Handle_DisplayContext by obj's Handle_WiNDow
  (let (hdc-ptr*)
    (with-foreign-object (hwnd* :int)
      (tk::tcl-get-int-from-obj interp
				(mem-aref argv :pointer 2)
				hwnd*)
      (setf hdc-ptr* (gethash (mem-ref hwnd* :int) *hdc-ptrs*))
      (when (null hdc-ptr*)
	(setf hdc-ptr* (foreign-alloc :int))	
	(setf (mem-ref hdc-ptr* :int)
	      (get-dc (mem-ref hwnd* :int))))
      (setf (gethash (mem-ref hwnd* :int) *hdc-ptrs*) hdc-ptr*))

    ;;; Set the pixel format of the HDC
    (with-foreign-object (pfd* '(:struct pixel-format-descriptor))
      (mapcar #'(lambda (x) (setf (foreign-slot-value
			      pfd* '(:struct pixel-format-descriptor) x)
			     0))
	      (list 'n-size
		    'n-version          
		    'dw-flags           
		    'i-pixel-type       
		    'c-color-bits      
		    'c-red-bits        
		    'c-red-shift       
		    'c-green-bits      
		    'c-green-shift     
		    'c-blue-bits       
		    'c-blue-shift      
		    'c-alpha-shift     
		    'c-accum-bits      
		    'c-accum-red-bits  
		    'c-accum-green-bits
		    'c-accum-blue-bits 
		    'c-accum-alpha-bits
		    'c-depth-bits   
		    'c-stencil-bits 
		    'c-aux-buffers  
		    'i-layer-type   
		    'b-reserved     
		    'dw-layer-mask  
		    'dw-visible-mask
		    'dw-damage-mask))
      (mapcar #'(lambda (pair)
		  (setf (foreign-slot-value
			 pfd* '(:struct pixel-format-descriptor) (car pair))
			(cdr pair)))
	      (list (cons 'n-version    1)
		    (cons 'dw-flags     (logior +PFD_DRAW_TO_WINDOW+
						+PFD_SUPPORT_OPENGL+
						+PFD_DOUBLEBUFFER+))
		    (cons 'i-pixel-type +PFD_TYPE_RGBA+)
		    (cons 'c-color-bits 24)
		    (cons 'c-depth-bits 16)
		    (cons 'i-layer-type +PFD_MAIN_PLANE+)))
      (with-foreign-object (pixel-format* :uint)
	(setf (mem-ref pixel-format* :uint)
	      (choose-pixel-format (mem-ref hdc-ptr* :int) pfd*))
	(set-pixel-format (mem-ref hdc-ptr*      :int)
			  (mem-ref pixel-format* :uint)
			  pfd*)))

     ;;; Create GL RenderingContext if one doesn't exist
    (when (null *rc-ptr*)
      (setf *rc-ptr* (foreign-alloc :int))
      (setf (mem-ref *rc-ptr* :int)
	    (wgl-create-context (mem-ref hdc-ptr* :int)))))
  
  tk::+TCL-OK+)

(defcallback resize
    :int
    ((client_data :int)
     (interp      :pointer)
     (argc        :int)
     (argv        :pointer))
  "[TCL CALLBACK] RESIZE a GL window"
  (declare (ignore client_data argc))
  (with-foreign-object   (w* :int)
    (with-foreign-object (h* :int)
      (tk::tcl-get-int-from-obj interp (mem-aref argv :pointer 1) w*)
      (tk::tcl-get-int-from-obj interp (mem-aref argv :pointer 2) h*)
      (gl::viewport 0 0 (mem-ref w* :int) (mem-ref h* :int))
      (gl::matrix-mode +GL_PROJECTION+)
      (gl::load-identity)
      (with-foreign-object (dx* :float)
	(let ((dx (/ (float (mem-ref w* :int))
		     (mem-ref h* :int))))
	  (setf (mem-ref dx* :float) dx)
	  (gl::ortho (* -2 dx) (*  2 dx)
		     -2        2
		     -2        2)))))
  tk::+TCL-OK+)

(defcallback swap-window-buffers
    :int
    ((client_data :int)
     (interp      :pointer)
     (argc        :int)
     (argv        :pointer))
  "[TCL CALLBACK] SWAP BUFFERS of a GL window"
  (declare (ignore client_data argc))
  (let (hdc-ptr*)
    (with-foreign-object (hwnd* :int)
      (tk::tcl-get-int-from-obj interp
				(mem-aref argv :pointer 2)
				hwnd*)
      (setf hdc-ptr* (gethash (mem-ref hwnd* :int) *hdc-ptrs*)))
    (when (not (null hdc-ptr*))
      (swap-buffers (mem-ref hdc-ptr* :int))))
  tk::+TCL-OK+)

(defcallback set-current-context
    :int
    ((client_data :int)
     (interp      :pointer)
     (argc        :int)
     (argv        :pointer))
  "[TCL CALLBACK] Make window the CURRENT GL CONTEXT"
  (declare (ignore client_data argc))
  (let (hdc-ptr*)
    (with-foreign-object (hwnd* :int)
      (tk::tcl-get-int-from-obj interp
				(mem-aref argv :pointer 2)
				hwnd*)
      (setf hdc-ptr* (gethash (mem-ref hwnd* :int) *hdc-ptrs*)))
    (when (and (not (null *rc-ptr*)) (not (null hdc-ptr*)))
      (wgl-make-current (mem-ref hdc-ptr* :int)
			(mem-ref *rc-ptr* :int))))
  tk::+TCL-OK+)

(defun init-tcl-gl-callbacks ()
  "Setup Tcl callbacks required for basic OpenGL integration"
  (tk::register-init-procs
   #'(lambda (&optional interp)
       (tk::tcl-create-obj-command interp
				   "SetRenderWindow"
				   (tk::callback set-render-window)
				   (tk::null-pointer) (tk::null-pointer)))
   #'(lambda (&optional interp)
       (tk::tcl-create-obj-command interp
				   "Resize"
				   (tk::callback resize)
				   (tk::null-pointer) (tk::null-pointer)))
   #'(lambda (&optional interp)
       (tk::tcl-create-obj-command interp
				   "SwapBuffers"
				   (tk::callback swap-window-buffers)
				   (tk::null-pointer) (tk::null-pointer)))
   #'(lambda (&optional interp)
       (tk::tcl-create-obj-command interp
				   "SetCurrentContext"
				   (tk::callback set-current-context)
				   (tk::null-pointer) (tk::null-pointer)))))

(init-tcl-gl-callbacks)

(defun window-swap-buffers (w)
  "Swap buffers of GL window W"
  (tk::send-command "SwapBuffers ~a [winfo id ~a]"
		    (tk:window-path w)
		    (tk:window-path w)))

(defun swap-all-buffers ()
  "Swap buffers on every window with a registered GL display context"
  (maphash #'(lambda (k v*) (declare (ignore k))
		(swap-buffers (mem-ref v* :int)))
	   *hdc-ptrs*))

(defun window-bind-gl (w)
  "Set window W as a registered GL display context & bind resize event"
  (tk::send-command "SetRenderWindow ~a [winfo id ~a]"
		    (tk:window-path w)
		    (tk:window-path w))
  (tk::send-command "bind ~a <Configure> {Resize %w %h}"
		    (tk:window-path w)))

(defun window-gl-set-current (w)
  "Set window W as current GL display context"
  (tk::send-command "SetCurrentContext ~a [winfo id ~a]"
		    (tk:window-path w)
		    (tk:window-path w)))
