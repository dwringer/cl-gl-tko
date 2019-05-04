;;; Create new & revised cl-simple-tk functions:  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :simple-tk)

(defcfun ("Tcl_CreateObjCommand" tcl-create-obj-command)
    :int
  (interp :pointer) (name :string :encoding :utf-8) (fun :pointer)
  (data :pointer) (del_proc :pointer))

(defcfun ("Tcl_GetIntFromObj" tcl-get-int-from-obj)
    :int
  (interp :pointer) (obj-ptr :pointer) (int-ptr :pointer))

(cffi:define-foreign-library shell32 (:windows "shell32.dll"))
(cffi:use-foreign-library shell32)

(cffi:defcfun ("SetCurrentProcessExplicitAppUserModelID"
	       set-current-process-explicit-app-user-model-id)
    :int (app-id :pointer))

(defun set-app-user-model-id (app-id)
  (let ((slen (length app-id)))
    (cffi:with-foreign-pointer-as-string (app-id* slen)
      (cffi:with-foreign-object (res :int)
	(setf res (set-current-process-explicit-app-user-model-id
		   (cffi:lisp-string-to-foreign app-id app-id* slen)))))))

(tk::create-canvas-method canvas-create-polygon "polygon")

(defun place (w &rest options)
  "PLACE geometry manager."
  (let ((suffix ""))
    (loop for op on options by #'cddr do
	 (setf suffix (concatenate 'string suffix
				   (format nil " -~a ~s"
					   (key-to-string (car op))
					   (option-to-string (cadr op))))))
    (send-command "place ~a~a" (window-path w) suffix)))

(defun place-configure (w &rest options)
  "Configure the place options for the window W"
  (loop for opt on options by #'cddr do
       (send-command "place configure ~a -~a ~s" (window-path w)
                     (key-to-string (car opt))
                     (option-to-string (cadr opt)))))

(export '(canvas-create-polygon set-app-user-model-id))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :common-lisp)

;;; Establish a package to hold userspace code such as helpers and widgets:
(defpackage #:tk-user
  (:use :cl :cl-user :tk)
  (:export

   ;;; Custom Additions:
   :button-panel :font-deformat :font-exists :grid-remove :proc-exists
   :style-configure :style-lookup :textconsole :title-bar-offset
   :update :update-idletasks :wgeom :parse-wgeom :window-attributes
   :window-configure-fn :window-delete-fn :window-overrideredirect
   :window-parent :window-root :window-rootp :window-toplevelp
   :window-wait-visibility :window-winfo-id :window-winfo-ismapped
   :window-winfo-viewable :with-button-panel :with-debug-window
   :with-main-window :with-mfd :with-scrollable-frame
   :with-toplevel-or-root

   ;;; list-demos.lisp
   :list-demos

   ;;; Standard CL-SIMPLE-TK exports:
   :after :after-cancel :after-idle :bind-all :bind-class
   :bind-command :bind-event :boolean-var :button :button-invoke
   :canvas :canvas-addtag :canvas-addtag-above :canvas-addtag-all
   :canvas-addtag-below :canvas-addtag-closest :canvas-addtag-enclosed
   :canvas-addtag-overlapping :canvas-addtag-withtag :canvas-bbox
   :canvas-bind :canvas-canvasx :canvas-canvasy :canvas-coords
   :canvas-create :canvas-create-arc :canvas-create-bitmap
   :canvas-create-image :canvas-create-line :canvas-create-oval
   :canvas-create-photo :canvas-create-poylgon
   :canvas-create-rectangle :canvas-create-text :canvas-create-window
   :canvas-dchars :canvas-delete :canvas-dtag :canvas-find
   :canvas-find-above :canvas-find-all :canvas-find-below
   :canvas-find-closest :canvas-find-enclosed :canvas-find-overlapping
   :canvas-find-withtag :canvas-focus :canvas-gettags :canvas-icursor
   :canvas-imove :canvas-index :canvas-insert :canvas-image-cget
   :canvas-image-configure :canvas-image-names :canvas-itemcget
   :canvas-itemconfig :canvas-lower :canvas-move :canvas-moveto
   :canvas-postscript :canvas-raise :canvas-rchars :canvas-scale
   :canvas-scan-dragto :canvas-scan-mark :canvas-scrolled-coords
   :canvas-select-adjust :canvas-select-clear :canvas-select-from
   :canvas-select-item :canvas-select-to :canvas-type :canvas-xview
   :canvas-xview-moveto :canvas-xview-scroll :canvas-yview
   :canvas-yview-moveto :canvas-yview-scroll :checkbutton
   :choose-color :choose-directory :clipboard-append :clipboard-clear
   :clipboard-get :combobox :combobox-current :configure-window
   :create-command :entry :entry-bbox :entry-delete :entry-get
   :entry-icursor :entry-index :entry-insert :entry-selection-adjust
   :entry-selection-clear :entry-selection-from
   :entry-selection-present :entry-selection-range :entry-selection-to
   :entry-validate :entry-xview :entry-xview-moveto
   :entry-xview-scroll :event-generate :event-key-code
   :event-mouse-position :event-window-path :float-var
   :format-lisp-list :frame :font-configure :font-create :font-delete
   :font-families :font-format :font-names :get-open-file
   :get-save-file :get-tk-themes :grab-release :grab-set :grab-status
   :grid :grid-configure :grid-columnconfigure :grid-forget :grid-info
   :grid-rowconfigure :grid-slaves :image-blank :image-cget
   :image-configure :image-copy :image-create-bitmap
   :image-create-photo :image-data :image-get :image-read
   :image-redither :image-transparency :image-write :integer-var
   :label :labelframe :listbox :listbox-activate :listbox-bbox
   :listbox-curselection :listbox-delete :listbox-get :listbox-index
   :listbox-insert :listbox-nearest :listbox-scan-dragto
   :listbox-scan-mark :listbox-see :listbox-selection-anchor
   :listbox-selection-clear :listbox-selection-includes
   :listbox-selection-set :listbox-size :listbox-xview
   :listbox-xview-moveto :listbox-xview-scroll :listbox-yview
   :listbox-yview-moveto :listbox-yview-scroll :menu :menu-activate
   :menu-add-cascade :menu-add-checkbutton :menu-add-command
   :menu-add-radio :menu-add-separator :menu-entrycget
   :menu-entryconfigure :menu-index :menu-insert-cascade
   :menu-insert-checkbutton :menu-insert-command :menu-insert-radio
   :menu-insert-separator :menu-invoke :menu-popup :menubutton
   :message :message-box :notebook :notebook-add
   :notebook-enable-traversal :notebook-forget :notebook-hide
   :notebook-identify-element :notebook-identify-tab :notebook-index
   :notebook-insert :notebook-select :notebook-tab :notebook-tabs
   :pack :pack-configure :pack-forget :pack-infopla :pack-slaves
   :panedwindow :panedwindow-add :panedwindow-forget
   :panedwindow-identify-element :panedwindow-identify-sash
   :panedwindow-insert :pandewindow-pane :panedwindow-panes
   :panedwindow-sashpos :place :place-configure :place-forget
   :place-info :place-slaves :progressbar :progressbar-start
   :progressbar-step :progressbar-stop :radiobutton :scale :scrollbar
   :scrollbar-connect :scrollbar-delta :scrollbar-fraction :separator
   :set-tk-theme :sizegrip :spinbox :string-var :text :text-bbox
   :text-count-chars :text-count-displaychars
   :text-count-displayindices :text-count-displaylines
   :text-count-indices :text-count-lines :text-count-xpixels
   :text-count-ypixels :text-delete :text-dlineinfo
   :text-edit-modified :text-edit-redo :text-edit-reset
   :text-edit-separator :text-edit-undo :text-get
   :text-get-displaychars :text-index :text-insert :text-image-cget
   :text-image-configure :text-image-create :text-image-names
   :text-mark-gravity :text-mark-names :text-mark-next
   :text-mark-previous :text-mark-set :text-mark-unset :text-replace
   :text-search :text-see :text-tag-add :text-tag-bind :text-tag-cget
   :text-tag-config :text-tag-delete :text-tag-lower :text-tag-names
   :text-tag-raise :text-window-create :text-window-cget
   :text-window-configure :text-window-names :text-xview
   :text-xview-moveto :text-xview-scroll :text-yview
   :text-yview-moveto :text-yview-scroll
   #+darwin :tk-mac-about-panel #+darwin :tk-mac-show-help
   #+darwin :tk-mac-show-preferences
   #+darwin :tk-mac-quit :tk-version :toplevel :trace-var :treeview
   :treeview-bbox :treeview-children :treeview-column-anchor
   :treeview-column-id :treeview-column-minwidth
   :treeview-column-stretch :treeview-column-width :treeview-delete
   :treeview-detach :treeview-exists :treeview-focus :treeview-get
   :treeview-heading-anchor :treeview-heading-command
   :treeview-heading-image :treeview-heading-text
   :treeview-identify-column :treeview-identify-element
   :treeview-identify-item :treeview-identify-region :treeview-insert
   :treeview-item-id :treeview-item-text :treeview-item-values
   :treeview-move :treeview-next :treeview-parent :treeview-prev
   :treeview-see :treeview-selection :treeview-selection-add
   :treeview-selection-remove :treeview-selection-set
   :treeview-selection-toggle :treeview-set :treeview-tag-bind
   :treeview-tag-configure :treeview-tag-has :treeview-tag-names
   :treeview-tag-add :treeview-tag-remove :var-value :window-children
   :window-cget :window-configure :window-deiconify :window-destroy
   :window-exists :window-focus :window-from-path :window-geometry
   :window-height :window-iconify :window-iconphoto :window-identify
   :window-identify-element :window-lower :window-maxsize
   :window-minsize :window-path :window-pointerx :window-pointerxy
   :window-pointery :window-protocol :window-raise :window-resizable
   :window-reqheight :window-reqwidth :window-rootx :window-rooty
   :window-screenheight :window-screenwidth :window-state
   :window-title :window-toplevel :window-width
   :window-winfo-geometry :window-withdraw :window-transient
   :window-wait :window-vrootheight :window-vrootwidth :window-vrootx
   :window-vrooty :with-tk-root))
(in-package #:tk-user)

;;; Tk interactions:  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar tk::*registered-init-procs* nil)

(defun tk::init-session ()
  "Setup a tcl/tk session."
  (let ((interp (tk::tcl-create-interp)))
    (when (/= (tk::tcl-init interp) tk::+tcl-ok+)
      (error (format nil "TCL init error: ~a"
		     (tk::tcl-get-string-result interp))))
    (when (/= (tk::tk-init interp) tk::+tcl-ok+)
      (error (format nil "TK init error: ~a"
		     (tk::tcl-get-string-result interp))))
    (let ((main (tk::tk-main-window interp)))
      (when (tk::null-pointer-p main)
        (error (format nil "TK no main window: ~a"
		       (tk::tcl-get-string-result interp))))
      (tk::tk-geometry-request main 200 200))
    (tk::tcl-create-command interp
			    "call_lisp"
			    (tk::callback tk::call-lisp)
			    (tk::null-pointer) (tk::null-pointer))
    (dolist (fn tk::*registered-init-procs*)
      (funcall fn interp))
    interp))

(defun tk::register-init-procs (&rest functions-of-optional-interp)
  "Register functions [lambda (&optional interp) ...] to call on Tcl/Tk init"
  (setf tk::*registered-init-procs*
	(append tk::*registered-init-procs*
		(set-difference functions-of-optional-interp
				tk::*registered-init-procs*))))

(defun tk:window-height (w)
  "Returns the height of the window W."
  (parse-integer (tk::get-response "winfo height ~a" (window-path w))))

;; (defun window-parent (w)
;;   (gethash (tk::get-response "winfo parent ~A" (window-path w))
;; 	   tk::*window-table*))

(defun window-winfo-id (w)
  (parse-integer (subseq (tk::get-response "winfo id ~a" (window-path w)) 2)
		 :radix 16))		 

(defun window-winfo-viewable (w)
  (> (parse-integer (tk::get-response "winfo viewable ~a" (window-path w))) 0))

(defun window-winfo-ismapped (w)
  (> (parse-integer (tk::get-response "winfo ismapped ~a" (window-path w))) 0))

(defun window-wait-visibility (w)
  (tk::get-response "tkwait visibility ~a" (window-path w)))

(defun text-insert (txt ind str &rest tags)
  "Inserts STR at indes IND in editable text in the window TXT."
  (tk::send-command "~a insert ~a ~s~{~^ ~s~}" (window-path txt) ind str tags))

(defun grid-remove (w)
  "Like grid-forget, except positioning is remembered so (grid w) works"
  (tk::send-command (format nil "grid remove ~a" (window-path w))))

(defun tk:bind-event (win ev fun)
  "Binds the event EV in window W with function FUN.

Function FUN accepts one argument EVT and is called when the event EV
happens in the window W. FUN can be NIL, in which case it removes the
binding for EV."
  (let ((id (string-downcase (format nil "~a.~e" (window-path win) ev))))
    (if fun
        (progn
          (tk::send-command "bind ~a ~a \{call_lisp ~a %x %y %X %Y %A %W\}"
                        (window-path win) ev id)
          (setf (gethash id tk::*event-table*) fun))
        (progn
          (tk::send-command "bind ~a ~a {}" (window-path win) ev)
          (remhash id tk::*event-table*)))))

(defun window-parent (w)
  "Returns the parent of the window W."
  (let ((parent (gethash (tk::get-response "winfo parent ~a" (window-path w))
			 tk::*window-table*)))
    (if (null parent) w parent)))

(defun tk:window-toplevel (w)
  (let ((top (gethash (tk::get-response "winfo toplevel ~a" (window-path w))
		      tk::*window-table*)))
    (if (null top) w top)))

(defun window-root (w)
  (let ((parent (window-parent w)))
    (cond ((equal w parent) w)
	  (t (window-root parent)))))

(defun window-toplevelp (w) (equal w (window-toplevel w)))

(defun window-rootp     (w) (equal w (window-root w)))

(defun wgeom (&key (w 320) (h 240) (x 100) (y 200))
  "Format a string for setting Tk window-geometry by WxH+[-]X+[-]Y"
  (format nil "~Ax~A+~A+~A" (max 0 w) (max 0 h) x y))

(defun parse-wgeom (str)
  "Parse a string of window-geometry from wgeom or (String (W...g... ...)) fmt"
  (do* ((acc   nil)
	(rest  (coerce str 'list) (cdr rest))
	(more? (not (null rest))  (not (null rest)))
	(next# "")
	(nextc (car rest)         (when more? (car rest))))
       ((not more?)
	(progn (push next# acc)
	       (apply #'values (mapcar #'parse-integer
				       (remove-if #'(lambda (x) (= (length x) 0))
						  (reverse acc))))))
    (cond ((member nextc '(#\x #\space #\+ #\-))
	   (progn (push next# acc)
		  (if (member nextc '(#\x #\space))
		      (setf next# "")
		      (setf next# (string nextc)))))
	  ((member nextc '(#\( #\))) nil)
	  (t (setf next# (concatenate 'string next# (string nextc)))))))

(defun title-bar-offset (root)
  (multiple-value-bind (size geox geoy)
      (apply #'values (window-geometry root))
    (declare (ignore size))
    (list (- (window-rootx root) geox)
	  (- (window-rooty root) geoy))))

(defun style-configure (qualified-name &rest attribute-plist)
  (let ((suffix ""))
    (maplist #'(lambda (plist)
		 (when (cdr plist)
		   (setf suffix
			 (concatenate 'string
			   suffix
			   (format nil " -~A {~A}"
				   (tk::key-to-string    (car  plist))
				   (tk::option-to-string (cadr plist)))))))
	     attribute-plist)
    (simple-tk::send-command 
     (format nil "ttk::style configure ~A~A" qualified-name suffix))))

(defun style-lookup (qualified-name option)
  (simple-tk::get-response
   (format nil "ttk::style lookup ~A ~A"
	   qualified-name
	   (tk::key-to-string option))))

(defun font-actual (name &key option displayof)
  (let ((resp (tk::get-response
	       (format nil "font actual ~A ~A ~A"
		       name
		       (if (not (null displayof))
			   (format nil "-displayof ~A"
				   (window-path displayof))
			   "")
		       (if (not (null option))
			   (tk::key-to-string option)
			   "")))))
    (when (null option) (setf resp (tk::parse-tcl-string resp)))
    resp))

(defun font-measure (name text &key displayof)
  (tk::get-response (format nil "font measure ~A ~A ~A"
			    name
			    (if (not (null displayof))
				(format nil "-displayof ~A"
					(window-path displayof))
				"")
			    text)))

(defun font-exists (name)
  (member name (font-names) :test #'string=))

(defun font-deformat (fstr)
  "Performs the reverse of FONT-FORMAT, retrieving an APPLY-able arglist"
  (let ((parts
	 (do* ((lst (coerce fstr 'list))
	       (parts nil)
	       (buffer "")
	       (reading nil)
	       (i 0 (+ i 1)))
	      ((= i (length lst)) (reverse parts))
	   (let ((next (elt lst i)))
	     (if reading
		 (if (eql next #\})
		     (progn
		       (setf reading nil)
		       (push buffer parts)
		       (setf buffer ""))
		     (setf buffer (format nil "~A~C" buffer next)))
		 (when (eql next #\{) (setf reading t))))))
	bold italic underline overstrike)
    (dolist (part (split-sequence:split-sequence #\space (elt parts 2)))
      (cond ((string= part "bold") (setf bold t))
	    ((string= part "italic") (setf italic t))
	    ((string= part "underline") (setf underline t))
	    ((string= part "overstrike") (setf overstrike t))))
    (let ((font-name (car parts))
	  (font-size (parse-integer (cadr parts))))
    (values (list (car parts) :size (parse-integer (cadr parts))
		  :bold bold :italic italic
		  :underline underline :overstrike overstrike)
	    font-name font-size bold italic underline overstrike))))

(defun proc-exists (name)
  (string= name (tk::get-response (format nil "info procs ~A" name))))

(let ((window-delete-fns (make-hash-table :test #'equal)))
  (defun (setf window-delete-fn) (fn w)
    (window-protocol w "WM_DELETE_WINDOW" fn)
    (setf (gethash w window-delete-fns) fn))
  
  (defun window-delete-fn (w)
    (gethash w window-delete-fns)))

(let ((window-configure-fns (make-hash-table :test #'equal)))
  (defun (setf window-configure-fn) (fn w)
    (bind-event w "<Configure>" fn)
    (setf (gethash w window-configure-fns) fn))

  (defun window-configure-fn (w)
    (gethash w window-configure-fns)))

(defun update ()
  (simple-tk::send-command "update"))

(defun update-idletasks ()
  (simple-tk::send-command "update idletasks"))

(defun window-attributes (w &rest option-plist)
  (simple-tk::send-command
   (format nil "wm attributes ~a~a"
	   (window-path w)
	   (let ((suffix ""))
	     (maplist #'(lambda (plist)
			  (when (and (not (null plist))
				     (= (mod (length plist) 2) 0))
			    (setf suffix (format nil "~a -~a ~a"
						 suffix
						 (tk::key-to-string
						  (car plist))
						 (tk::option-to-string
						  (cadr plist))))))
		      option-plist)
	     suffix))))

(defun window-overrideredirect (w bool)
  (simple-tk::send-command (format nil "wm overrideredirect ~a ~a"
				   (window-path w)
				   (tk::option-to-string bool))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Textconsole widget:  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun textconsole (parent &key handler font (height 6) (init "nil"))
  (let* ((frame (frame     :parent parent))
	 (txt   (text      :parent frame :height height))
	 (sb    (scrollbar :parent frame :orient "vertical")))
    (scrollbar-connect txt sb)
    (grid txt :row 0 :column 0 :sticky "enws")
    (grid sb  :row 0 :column 1 :sticky "ns")
    (let* ((string-var (string-var))
	   (entry      (entry :parent frame :textvariable string-var)))
      (grid entry :row 1 :column 0 :columnspan 2 :sticky "ew")
      (grid-rowconfigure frame 0 :weight 1)
      (grid-rowconfigure frame 1 :weight 0)
      (grid-columnconfigure frame 0 :weight 1)
      (grid-columnconfigure frame 1 :weight 0)
      (when (not (null font))
	(text-tag-config txt "defaultFont" :font font))
      (labels ((ins (str)
		 (if (null font)
		     (text-insert txt "insert" str)
		     (text-insert txt "insert" str "defaultFont")))
	       (ev-handler (&optional ev)
		 (ins (format nil "> ~A~%" (var-value string-var)))
		 (ins (format nil "~A"
			      (funcall handler
				       ev
				       string-var
				       entry
				       txt
				       sb
				       font
				       frame)))
		 (setf (text-yview txt) "insert")
		 (ins (format nil "~%"))
		 (setf (var-value string-var) "")))
	(bind-event entry "<Return>" #'ev-handler)
	(when init (tk:after 10 #'(lambda ()
				    (setf (var-value string-var) init)
				    (funcall #'ev-handler))))))
    frame))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Button panel widget creation/support:  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct button-desc
  "Information used in constructing tk:button widgets"
  (label-text   ""           :type string)
  (procedure    #'(lambda () nil) :type function)
  (textvariable nil          :type (or boolean simple-tk::tcl-var)))

(defun button-desc-from-list (desc-as-list)
  "Make button-desc of '(<name> <function> [<string-var>|t|nil]) DESC-AS-LIST"
  (when (= (length desc-as-list) 1)
    (setf (cdr desc-as-list)        (cons #'(lambda () nil) nil)))
  (when (= (length desc-as-list) 2)
    (setf (cdr (last desc-as-list)) (cons nil          nil)))
  (multiple-value-bind (label-text procedure is-variable)
      (apply #'values desc-as-list)
    (make-button-desc :label-text   label-text
		      :procedure    procedure
		      :textvariable (cond ((eql (type-of is-variable)
						'simple-tk::tcl-var)
					   (prog1 is-variable
					     (setf (var-value is-variable)
						   label-text)))
					  ((eql is-variable t)
					   (string-var label-text))
					  (t nil)))))

(defun button-from-button-desc (parent desc &optional (always-return-var t))
  "Instantiate a button under PARENT from the given button-DESC"
  (with-slots (label-text procedure textvariable) desc
      (let* ((return-textvariable nil)
	     (button-args
	      (append (list :parent  parent
 			    :command procedure) 
 		      (cond ((null textvariable) (list :text label-text))
			    ((eql (type-of textvariable) 'simple-tk::tcl-var)
			     (progn (when always-return-var
				      (setf return-textvariable textvariable))
				    (list :textvariable textvariable)))
			    (t (progn (setf return-textvariable (string-var))
				      (list :textvariable
					    return-textvariable)))))))
	(values (apply #'button button-args)
		return-textvariable))))

(defun buttons-from-list (parent lst)
  "Create buttons under PARENT from a LST of lists describing button-descs"
  (let* ((vars    nil)
	 (buttons
	  (mapcar #'(lambda (x)
		      (multiple-value-bind (btn var)
			  (button-from-button-desc parent x)
			(push var vars)
			btn))
		  (mapcar #'(lambda (x) (button-desc-from-list x))
			  lst))))
    (values buttons (reverse vars))))

(defmacro button-panel (definition-table
			&key parent root-as frame-as init
			  (orient "vertical"))
  "Create a button-panel widget or app [& run] from desc DEFINITION-TABLE"
  (let* ((_frame (if frame-as frame-as (gensym)))
	 (_buttons (gensym))
	 (_vars    (gensym))
	 (pack-expr
	  (cond ((string= (string-downcase (string orient)) "vertical")
		 `(multiple-value-bind (,_buttons ,_vars)
		      (buttons-from-list ,_frame ,definition-table)
		    (map nil #'(lambda (x) (pack x :expand t :fill "both"))
			 ,_buttons)
		    (values ,_frame ,_vars)))
		((string= (string-downcase (string orient)) "horizontal")
		 (let ((_i (gensym)))
		   `(multiple-value-bind (,_buttons ,_vars)
			(buttons-from-list ,_frame ,definition-table)
		      (do ((,_i 0 (+ ,_i 1)))
			  ((= ,_i (length ,_buttons)) nil)
			(grid (elt ,_buttons ,_i)
			      :row 0 :column ,_i :sticky "enws")
			(grid-columnconfigure ,_frame ,_i :weight 1))
		      (grid-rowconfigure ,_frame 0 :weight 1)
		      (values ,_frame ,_vars)))))))
    (if parent
	`(let ((,_frame (frame :parent ,parent)))
	   ,pack-expr)
	(let ((_root  (if root-as  root-as  (gensym))))
	  `(with-main-window (,_root ,_frame) (,@init)
	     ,pack-expr)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; UI Macros:  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-toplevel-or-root ((root-or-toplevel-name &key rootarg title)
				 &rest body)
  "Either use a new toplevel from an existing root, or a new root directly"
  (let ((_root (gensym)))
    `(if (not (null ,rootarg))
	 (let* ((,_root ,rootarg)
		(,root-or-toplevel-name (toplevel :parent ,_root)))
	   (setf (window-title ,root-or-toplevel-name) ,title)
	   ,@body)
	 (with-tk-root (,root-or-toplevel-name :title ,title) ,@body))))

(defmacro with-main-window ((root-as frame-as)
			    (&key (title "")
				  (width   nil) (height   nil)
				  (min-width 0) (min-height 0))
			    &rest body)
  "Evaluate body in the context of a newly-instantiated main Tk window/frame"
  `(with-tk-root (,root-as)
     (setf (window-title ,root-as) ,title)
     ,(when (not (and (null width) (null height)))
	`(setf (window-geometry ,root-as) (wgeom :w ,width :h ,height)))
     ,(when (or (> min-width 0) (> min-height 0))
	`(setf (window-minsize ,root-as)
	       (list ,min-width ,min-height)))
     (let ((,frame-as (frame :parent ,root-as)))
       (pack ,frame-as :expand t :fill "both")
       ,@body)))

(defmacro with-button-panel (parent
			     (panel string-vars &key (orient :vertical))
                             definition-table
			     &rest body)
  "Evaluate body after instantiating button-PANEL and binding its STRING-VARS"
  (let ((_vars  (gensym))
	(_panel (gensym)))
    `(let ((,string-vars nil)
	   (,panel nil))
       (multiple-value-bind (,_panel ,_vars)
	   (button-panel ,definition-table
			 :parent   ,parent
			 :orient   ,orient)
	 (setf ,string-vars ,_vars)
	 (setf ,panel ,_panel)
	 ,@body))))

(defmacro with-mfd (parent (outer-frame-as inner-frame-as &optional panels-as)
		    (&key
		     (expand-buttons? nil)
		     panel-defs-right
		     panel-defs-top
		     panel-defs-left
		     panel-defs-bottom)
		    &rest body)
  "Multi-Function-Display OUTER-FRAME exposing INNER-FRAME-AS & button PANELS"
  (when (null panels-as) (setf panels-as (gensym)))
  (let ((_panel-right              (gensym))
	(_panel-top                (gensym))
	(_panel-left               (gensym))
	(_panel-bottom             (gensym))
	(_inner-frame-row-index    (gensym))
	(_inner-frame-column-index (gensym))
	(_number-of-rows           (gensym))
	(_number-of-columns        (gensym)))
    (macrolet ((plc (name defs ornt)               ;; "Panel Let Clause"
		 `(when (not (null ,defs))
		    `((,,name (button-panel ,,defs
					    :parent ,outer-frame-as
					    :orient ,,ornt)))))
	       (cg (defs pnl row col stky
			 &optional (aftpnls nil))  ;; "Conditionally Grid"
		 `(when (not (null ,defs))
		    `(progn
		       (if (and ,,aftpnls (not (null ,panels-as)))
			   (setf (cdr (last ,panels-as))
				 (cons ,,pnl nil))
			   (push ,,pnl ,panels-as))
		       (apply #'grid (append
				      (list ,,pnl :row ,,row :column ,,col)
				      ,(when expand-buttons?
					 `(list :sticky ,,stky))))))))
      `(let* (,@(append `((,outer-frame-as (frame :parent ,parent)))
			`((,inner-frame-as (frame :parent ,outer-frame-as)))
			`((,panels-as nil))
			(plc _panel-right  panel-defs-right  :vertical)
			(plc _panel-top    panel-defs-top    :horizontal)
			(plc _panel-left   panel-defs-left   :vertical)
			(plc _panel-bottom panel-defs-bottom :horizontal)
			`((,_inner-frame-row-index
			   ,(if (null panel-defs-top) 0 1)))
			`((,_inner-frame-column-index
			   ,(if (null panel-defs-left) 0 1)))
			`((,_number-of-rows
			   (+ 1 ,_inner-frame-row-index
			      ,(if (null panel-defs-bottom) 0 1))))
			`((,_number-of-columns
			   (+ 1 ,_inner-frame-column-index
			      ,(if (null panel-defs-right) 0 1))))))
	 ,(let ((ignorable (append (when (null panel-defs-right)
				     (list `,_number-of-columns))
				   (when (null panel-defs-bottom)
				     (list `,_number-of-rows)))))
	       `(declare (ignore ,@ignorable)))
	 ,(cg panel-defs-top  _panel-top  0 _inner-frame-column-index "enws")
	 ,(cg panel-defs-left _panel-left _inner-frame-row-index    0 "enws" t)
	 (grid ,inner-frame-as
	       :row    ,_inner-frame-row-index
	       :column ,_inner-frame-column-index
	       :sticky "enws")
	 ,(cg panel-defs-right _panel-right
	      _inner-frame-row-index `(- ,_number-of-columns 1) "enws")
	 ,(cg panel-defs-bottom _panel-bottom
	      `(- ,_number-of-rows 1) _inner-frame-column-index "enws" t)
	 ,@body))))

(defmacro with-scrollable-frame (parent (outer-frame-name inner-frame-name
				 &key canvas-name
				      canvas-arglist
				      (vscroll? t)
				      vscroll-name
				      hscroll?
				      hscroll-name)
				 &rest body)
  (let ((_outer-frame outer-frame-name)
	(_inner-frame inner-frame-name)
	(_canvas      (if (not (null canvas-name))  canvas-name  (gensym)))
	(_vscroll     (if (not (null vscroll-name)) vscroll-name (gensym)))
	(_hscroll     (if (not (null hscroll-name)) hscroll-name (gensym))))
    (when (not (getf canvas-arglist :highlightthickness))
      (setf canvas-arglist
	    (append canvas-arglist (list :highlightthickness 0))))
    `(let* ((,_outer-frame (frame :parent ,parent))
	    ,@(when hscroll?
		    `((,_hscroll (scrollbar :parent ,_outer-frame
					    :orient "horizontal"))))
	    ,@(when vscroll?
		    `((,_vscroll (scrollbar :parent ,_outer-frame
					    :orient "vertical"))))
	    (,_canvas      (canvas :parent ,_outer-frame ,@canvas-arglist))
	    (,_inner-frame (frame :parent ,_canvas)))
       (grid ,_canvas :row 0 :column 0 :sticky "enws")
       (grid-columnconfigure ,_outer-frame 0 :weight 1)
       (grid-rowconfigure    ,_outer-frame 0 :weight 1)
       ,@(when vscroll? `((grid ,_vscroll :row 0 :column 1 :sticky "ns")))
       ,@(when hscroll? `((grid ,_hscroll :row 1 :column 0 :sticky "ew")))
       (canvas-create-window ,_canvas (list 0 0)
			     :window ,_inner-frame
			     :anchor "nw")
       (setf (window-configure-fn ,_canvas)
	     #'(lambda (&optional ev)
		 (declare (ignore ev))
		 (window-configure ,_canvas
				   :scrollregion (canvas-bbox ,_canvas
							      "all")
				   ,@(when (not hscroll?)
					   `(:width (window-reqwidth
						     ,_inner-frame))))))
       ,@(when vscroll? `((scrollbar-connect ,_canvas ,_vscroll)
			  (grid-columnconfigure ,_outer-frame 0 :weight 1)))
       ,@(when hscroll? `((scrollbar-connect ,_canvas ,_hscroll)
			  (grid-rowconfigure    ,_outer-frame 0 :weight 1)))
       ,@body)))

(defmacro with-debug-window (parent (toplevel-as) (&rest expressions)
			      &rest body)
  (let ((_mfd       (gensym))
	(_main      (gensym))
	(_panels    (gensym))
	(_frame     (gensym))
	(_scrollbar (gensym))
	(_tv        (gensym))
	(_line-ids  (gensym))
	(_update-fn (gensym)))
    `(let* ((,toplevel-as (toplevel  :parent ,parent))
	    (,_tv         nil)
	    (,_line-ids   nil))
       (labels ((,_update-fn ()
		  (treeview-delete ,_tv ,_line-ids)
		  (setf ,_line-ids
			(mapcar #'(lambda (k v)
				    (treeview-insert ,_tv "" 0
						     :values (list k v)))
				(list ,@(mapcar
					 #'(lambda (name)
					     `(format nil "~A"
						      (quote ,name)))
					 (reverse expressions)))
				(list ,@(mapcar
					 #'(lambda (name)
					     `(format nil "~A" ,name))
					 (reverse expressions)))))))
	 (with-mfd ,toplevel-as (,_mfd ,_main ,_panels)
	   (:expand-buttons? t
	    :panel-defs-bottom
	    (list
	     (list "Update" #',_update-fn)
	     (list "Done" #'(lambda () (funcall (window-delete-fn ,toplevel-as))))))
	   (let* ((,_frame      (frame     :parent ,_main))
		  (,_scrollbar  (scrollbar
				 :parent ,_frame :orient "vertical")))
	     (setf ,_tv (treeview :parent  ,_frame
				  :columns '("colExpr" "colResult")
				  :show    "headings"))
	     (scrollbar-connect ,_tv ,_scrollbar)
	     (setf (treeview-heading-text ,_tv "colExpr")   "Expression")
	     (setf (treeview-heading-text ,_tv "colResult") "Result")
	     (setf (window-delete-fn ,toplevel-as)
		   #'(lambda ()
		       (window-destroy ,_tv)
		       (window-destroy ,toplevel-as)))
	     (grid ,_tv        :row 0 :column 0 :sticky "enws")
	     (grid ,_scrollbar :row 0 :column 1 :sticky "ns")
	     (grid-columnconfigure ,_frame 0 :weight 1)
	     (grid-rowconfigure    ,_frame 0 :weight 1)
	     (pack ,_frame :expand t :fill "both")
	     (pack ,_mfd   :expand t :fill "both")
	     (grid-columnconfigure ,_mfd 0 :weight 1)
	     (grid-rowconfigure    ,_mfd 0 :weight 1)
	     (after 25 #'(lambda () (,_update-fn)))
	     ,@body))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Examples:  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun button-panel-applet-example ()
  "Demonstrate button-panel as a running app [which blocks interpreter]"
  (button-panel (mapcar #'(lambda (x) (list x #'(lambda () (window-destroy tk-root))))
			'("This" "is" "an" "example" "button" "panel"))
		:init    (:width 100 :height 600)
		:root-as tk-root
		:orient  :vertical))

(defun button-panel-widget-example ()
  "Demonstrate button-panel as a widget inside a Tk frame"
  (with-main-window (root frame) (:width 600 :height 100)
    (multiple-value-bind (panel vars)
	(button-panel
	 (mapcar #'(lambda (x) (list x #'(lambda () (window-destroy root)))) 
		 '("This" "is" "an" "example" "button" "panel"))
	 :parent frame :orient :horizontal)
      (declare (ignore vars))
      (pack panel :expand t :fill "both"))))

(defun button-panel-advanced-example ()
  "Demonstrate a button-panel which dynamically reassigns its labels"
  (with-main-window (root frame) (:width 240 :height 600)
    (let ((sentence-list '("This" "is" "an" "example" "sentence." "Goodbye!"))
	  (range         '(0      1    2    3         4           5))
	  (svars         nil))
      (multiple-value-bind (panel vars)
	  (button-panel
	   (mapcar #'(lambda (x i)
		       (list x
			     #'(lambda ()
				 (when (string= (var-value (elt svars i))
						"Goodbye!")
				   (window-destroy root))
				 (setf sentence-list
				       (append (cdr sentence-list)
					       (list (car sentence-list))))
				 (mapcar #'(lambda (w v)
					     (setf (var-value v) w))
					 sentence-list
					 svars))
			     t))
		   sentence-list
		   range)
	   :parent frame :orient :vertical)
	(pack panel :expand t :fill "both")
	(setf svars vars)
	(format t "~A~%~A~%" sentence-list svars)))))

(defmacro button-panel-macro-widget-example ()
  "Demonstrate with-button-panel macro while dynamically reassigning labels"
  (with-main-window (root frame) (:width 240 :height 600)
    (let ((sentence-list '("This" "is" "an" "example" "sentence." "Goodbye!"))
	  (range         '(0      1    2    3         4           5)))
      (with-button-panel frame (panel svars :orient :vertical)
	(mapcar #'(lambda (x i)
		      (list x
			    #'(lambda ()
				(when (string= (var-value (elt svars i))
					       "Goodbye!")
				  (window-destroy root))
				(setf sentence-list
				      (append (cdr sentence-list)
					      (list (car sentence-list))))
				(mapcar #'(lambda (w v)
					    (setf (var-value v) w))
					sentence-list
					svars))
			    t))
		  sentence-list
		  range)
	(pack panel :expand t :fill "both")))))

(defun treeview-frame (nb)
  "Example treeview borrowed from the cl-simple-tk demo.lisp example file"
  (let* ((tw (tk:treeview :parent nb :columns (list "one" "two"))))
    (setf (tk:treeview-column-width tw "one") 100)
    (setf (tk:treeview-column-width tw "two") 100)
    (setf (tk:treeview-heading-text tw "one") "Column A")
    (setf (tk:treeview-heading-text tw "two") "Column B")
    (let ((id (tk:treeview-insert tw
				  "" 0 :text "Line 1" :values (list "1" "2"))))
      (tk:treeview-insert tw id 0 :text "Line 2" :values (list "11" "22"))
      (tk:treeview-selection-set tw id))
    (tk:bind-event tw "<<TreeviewSelect>>"
                   (lambda (ev)
                     (declare (ignore ev))
                     (print (tk:treeview-selection tw))))
    tw))

(defun mfd-example ()
  "Demonstrate the with-mfd macro and effects on variable closures"
  (with-main-window (root frame) ()
   (let ((toplevels (make-hash-table :test #'equal)))
    (style-configure "TButton"  :font "Tahoma 8")
    (with-mfd frame (mfd main-elt panels)
      (:panel-defs-top    (mapcar #'list '("Okay" "A" "top" "set" "of"
					   "buttons"))
       :panel-defs-left   (mapcar #'list '("lefty" "is" "a" "terrible" "list"
					   "name" "but" "it's" "all" "I"
					   "got"))
       :panel-defs-right  (let ((svar (string-var)))
			    (list
			     (list "test"
				   #'(lambda ()
				       (let* ((tl (toplevel :parent root)))
					 (setf (gethash tl toplevels) tl)
					 
					 (with-mfd tl (mfd2 elt2 panels2)
					   (:panel-defs-left
					    (list
					     (list "OK"
						   #'(lambda ()
						       (funcall
							(window-delete-fn
							 tl))))))
					   (let ((tv (treeview-frame elt2)))
					     (setf (window-delete-fn tl)
						   #'(lambda ()
						       (remhash tl toplevels)
						       (window-destroy tv)
						       (window-destroy tl)))
					     (pack tv
						   :expand t :fill "both")
					     (pack mfd2
						   :expand t :fill "both"))))))
			     (list "righty"
				   #'(lambda ()
				       (with-debug-window root (dbg) ;;  dbg-sv)
					  (root frame mfd main-elt
					   ":: :: ::"
					   (elt panels 0)
					   (elt panels 1)
					   (elt panels 2)
					   (elt panels 3)
					   ":: :: ::"
					   svar
					   (var-value svar)
					   ":: :: ::"
					   (window-geometry root)
					   (window-rootx  root)
					   (window-rooty  root)
					   (window-width  root)
					   (window-height root)
					   (title-bar-offset root)
					   ":: :: ::"
					   toplevels
					   ":: :: ::"
					   (window-width  main-elt)
					   (window-height main-elt)
					   ":: :: ::"
					   (window-width  (elt panels 0))
					   (window-height (elt panels 0))
					   ":: :: ::"
					   (window-width  (elt panels 1))
					   (window-height (elt panels 1))
					   ":: :: ::"
					   (window-width  (elt panels 2))
					   (window-height (elt panels 2))
					   ":: :: ::"
					   (window-width  (elt panels 3))
					   (window-height (elt panels 3)))
					  (setf (var-value svar) "OK")))
				   svar)))
       :panel-defs-bottom (list (list "goodbye"
				      #'(lambda () (window-destroy root)))))    
      (pack (treeview-frame main-elt) :expand t :fill "both")
      (pack mfd :expand t :fill "both")
      (window-wait-visibility root)
      (style-configure "Treeview" :rowheight 
		       (- (cadr (title-bar-offset root))
			  (car
			   (title-bar-offset root))))
      (window-wait-visibility main-elt)
      (setf (window-geometry root)
	    (wgeom :w (+ (window-width  main-elt)
			 (window-width  (elt panels 0))
			 (window-width  (elt panels 2)))
		   :h (+ (window-height main-elt)
			 (window-height (elt panels 1))
			 (window-height (elt panels 3)))
		   :x (window-rootx root)
		   :y (window-rooty root)))))))

(defun make-exe-mfd-example (filename)
  "On SBCL, call save-lisp-and-die to create a windows GUI executable"
  (sb-ext:save-lisp-and-die filename
			    :toplevel         #'mfd-example
			    :executable       t
			    :application-type :gui))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					      
