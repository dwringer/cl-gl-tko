(defpackage #:demopanels (:use :common-lisp
			       :tk-user
			       :simple-tk)
	    (:export :msg-panel
		     :see-dismiss-panel
		     :define-demo-window
		     :get-truename-macro))
(in-package #:demopanels)

(defmacro get-truename-macro ()
  `(or ,*compile-file-pathname* ,*load-truename*))

(defvar *code-dialog-file-path* (get-truename-macro))

(defun require-img-package ()
  (tk::send-command "package require Img"))

(defun make-capture-window-proc ()
  (tk::send-command
   (format nil "~{~A~^~%~}"
	   (list
	    "proc CaptureWindow {win {baseImg \"\"} {px 0} {py 0}} {"
	    "   if {$baseImg eq \"\"} {"
	    "     set baseImg [image create photo -format window -data $win]"
	    "     CaptureWindow $win $baseImg"
	    "     return $baseImg"
	    "   }"
	    "   foreach child [winfo children $win] {"
	    "     if {![winfo ismapped $child]} continue"
	    "     set childImg [image create photo -format window -data $child]"
	    "     regexp {\\+(\\d*)\\+(\\d*)} [winfo geometry $child] -> x y"
	    "     $baseImg copy $childImg -to [incr x $px] [incr y $py]"
	    "     image delete $childImg"
	    "     CaptureWindow $child $baseImg $x $y"
	    "   }"
	    " }"))))

(defun make-window-to-file-proc ()
  (tk::send-command
   (format nil "~{~A~^~%~}"
	   (list
	    "proc WindowToFile { win } {"
	    "  set image [CaptureWindow $win]"
	    "  set types {{\"Image Files\" {.png}}}"
	    "  set filename [tk_getSaveFile -filetypes        $types \\"
	    "                               -initialfile capture.png \\"
	    "                               -defaultextension   .png]"
	    "  if {[llength $filename]} {"
	    "      $image write -format png $filename"
	    "  } "
	    "  image delete $image"
	    "}"))))

(defun call-window-to-file-proc (w &optional (update? t))
  (when update? (tk-user::update))
  (tk::send-command (format nil "WindowToFile ~A" (window-path w))))

(defun msg-panel (parent text)
  (let ((f (frame :parent parent)))
    (pack f :side "top"  :fill "x" :anchor "e")
    (let ((msg (label :parent f :wraplength "4i" :justify "left"
		      :text (format nil "~{~A~^ ~}" text))))
      (pack msg :fill "x" :padx 5 :pady 5 :anchor "e"))))

(defun see-dismiss-panel (parent &key root path)
  (let ((f (frame :parent parent)))
    (pack f :side "bottom" :fill "x")
    (when (not (proc-exists "WindowToFile"))
      (demopanels::require-img-package)
      (demopanels::make-capture-window-proc)
      (demopanels::make-window-to-file-proc))
    (let ((sep         (separator :parent   f
				  :orient   "horizontal"))
	  (screen-btn  (button    :parent   f
				  :text     "Screenshot"
				  :image    (image-create-photo
					     :format "png"
					     :file "images/snap.png")
				  :command  #'(lambda ()
						(demopanels::call-window-to-file-proc
						 (window-parent parent)))
				  :compound "left"))
	  (sep2        (separator :parent   f
				  :orient   "vertical"))
	  (dismiss-btn (button    :parent   f
			          :text     "Dismiss"
			          :image    (image-create-photo
					     :format "png"
					     :file "images/delete.png")
				  :command  #'(lambda () (window-destroy
						     (window-parent parent)))
				  :compound "left"))
	  (code-btn    (button    :parent   f
				  :text     "See Code"
				  :image    (image-create-photo
					     :format "png"
					     :file "images/view.png")
				  :command  #'(lambda ()
						(apply
						 #'make-instance
					         (append
						  (list 'code-dialog
							:parent f)
						  (when (not (null path))
						    (list :path path)))))
				  :compound "left")))
      (window-focus code-btn)
      (grid sep         :row 0 :columnspan 6 :sticky "ew"  :pady 5)
      (grid screen-btn  :row 1 :column 0     :sticky "e"   :padx 1)
      (grid sep2        :row 1 :column 1     :sticky "ens" :padx 4 :pady 3)
      (grid code-btn    :row 1 :column 2     :sticky "e")
      (grid dismiss-btn :row 1 :column 3     :sticky "e")
      (grid-rowconfigure    f 0 :weight 1)
      (grid-columnconfigure f 0 :weight 1)
      (when (not (null root))
	(bind-event root
		    "<Return>" #'(lambda (ev)
				   (declare (ignore ev))
				   (button-invoke code-btn)))
	(bind-event root
		    "<Escape>" #'(lambda (ev)
				   (declare (ignore ev))
				   (button-invoke dismiss-btn)))))))

(defclass dialog ()
  ((parent :initarg :parent)
   (top    :initform nil)
   (title  :initarg :title :initform nil)
   (result :initform nil)
   (initial-focus :initform nil)))

(defmethod body ((self dialog) parent)
  (declare (ignore self parent)))

(defmethod destroy ((self dialog))
  (setf (slot-value self 'initial-focus) nil)
  (window-destroy (slot-value self 'top)))

(defmethod cancel ((self dialog))
  (with-slots (parent) self
    (when (not (null parent))
      (window-focus parent)))
  (destroy self))

(defmethod validate ((self dialog)) t)

(defmethod apply_ ((self dialog)))

(defmethod ok ((self dialog) &optional event)
  (declare (ignore event))
  (if (not (validate self))
      (window-focus (slot-value self 'initial-focus))
      (progn
	(window-withdraw (slot-value self 'top))
	(update-idletasks)
	(handler-case (apply_ self)
	  (error (e)
	    (declare (ignore e))))
	(cancel self))))

(defmethod buttonbox ((self dialog))
  (let ((box (frame :parent (slot-value self 'top))))
	
    (pack (button :parent box :text "OK" :width 10
		  :command #'(lambda () (ok self))
		  :default "active")
	  :side "left" :padx 5 :pady 5)
    (pack (button :parent box :text "Cancel" :width 10
		  :command #'(lambda () (cancel self)))
	  :side "left" :padx 5 :pady 5)
    (bind-event (slot-value self 'top) "<Return>"
		#'(lambda () (ok self)))
    (bind-event (slot-value self 'top) "<Escape>"
		#'(lambda () (cancel self)))
    (pack box)))

(defmethod initialize-instance :after ((self dialog) &rest initargs)
  (declare (ignore initargs))
  (let ((top (toplevel :parent (slot-value self 'parent))))
    (setf (slot-value self 'top) top)
    (window-withdraw top)
    (when (window-winfo-viewable top)
      (window-transient top :parent (slot-value self 'parent)))
    (let ((body (frame :parent top)))
      (setf (slot-value self 'initial-focus)
	    (body self body))
      (pack body :padx 5 :pady 5 :expand t :fill "both")
      (with-slots (title) self
	(when (not (null title)) (setf (window-title top) title)))
      (buttonbox self)
      (when (null (slot-value self 'initial-focus))
	(setf (slot-value self 'initial-focus) top))
      (setf (window-delete-fn top) #'(lambda () (cancel self)))
      (with-slots (parent) self
	(when (not (null parent))
 	  (setf (window-geometry top)
 		(format nil "+~a+~a"
			(+ (window-rootx parent) 50)
 			(+ (window-rooty parent) 50)))))
      (window-deiconify top)
      (window-focus (slot-value self 'initial-focus))
      (window-wait-visibility top)
      (setf (window-minsize top)
	    (list 100 (window-height top)))
      (grab-set top)
      (window-wait top))))

(defclass code-dialog (dialog)
  ((path :initform *code-dialog-file-path* :initarg :path)))

(defmethod body ((self code-dialog) parent)
  (let ((filename (slot-value self 'path)))
    (setf (slot-value self 'title) (format nil "Source code: ~A" filename))
    (let ((txt-frame (frame :parent parent)))
      (pack txt-frame :side "top" :expand t :fill "both")
      (let ((text (text :parent txt-frame :height 24 :width 85 :wrap "word"
			:setgrid nil :highlightthickness 0 :pady 2 :padx 3))
	    (xscroll (scrollbar :parent txt-frame :orient "horizontal"))
	    (yscroll (scrollbar :parent txt-frame :orient "vertical")))
	(scrollbar-connect text xscroll)
	(scrollbar-connect text yscroll)
	(grid text    :row 0 :column 0 :sticky "enws")
	(grid yscroll :row 0 :column 1 :sticky "enws")
	(grid-rowconfigure    txt-frame 0 :weight 1)
	(grid-columnconfigure txt-frame 0 :weight 1)
	(text-delete text "0.0" "end")
	(text-insert text "end"
		     (with-open-file (inf filename :direction :input)
		       (do ((acc nil)
			    (next (read-line inf nil nil)
				  (read-line inf nil nil)))
			   ((null next)
			    (format nil "~{~A~^~%~}" (reverse acc)))
			 (push next acc))))))))

(defmethod buttonbox ((self code-dialog))
  (with-slots (top) self
    (let ((box (frame :parent top)))
      (pack (button :parent box :text "Cancel"
		    :command #'(lambda () (cancel self))))
      (bind-event top "<Return>" #'(lambda (ev)
				     (declare (ignore ev))
				     (cancel self)))
      (bind-event top "<Escape>" #'(lambda (ev)
				     (declare (ignore ev))
				     (cancel self)))
      (pack box :side "bottom"))))

(defvar *demo-windows* (make-hash-table :test 'equal))

(defmacro define-demo-window (name
			      (frame-as &key title)
			      (&rest lets)
			      (&rest labels*)
			      (&rest msg)
			      &rest body)
  (let ((_top (gensym)))
    `(let ((load-truename (get-truename-macro)))
       (defun ,name (&key parent root)
	 (let (,@lets)
	   (labels (,@labels*)
	     (if (null parent)
		 (with-toplevel-or-root (,_top :rootarg root :title ,title)
		   (let ((,frame-as (frame :parent ,_top)))
		     (pack ,frame-as :expand t :fill "both")
		     (demopanels:msg-panel ,frame-as (list ,@msg))
		     (demopanels:see-dismiss-panel
		      ,frame-as :root ,_top :path load-truename)
		     ,@body
		     (setf (window-resizable ,_top) (list nil nil))))
		 (let ((,frame-as (frame :parent parent)))
		   (pack ,frame-as :expand t :fill "both")
		   ,@body)))))
       (setf (gethash ,title demopanels::*demo-windows*) #',name))))
