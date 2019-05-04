(in-package :glt)

(defun hex2int (str)
  (/ (parse-integer (coerce str 'string) :radix 16)
     255.0))

(defvar *color1*)
(defvar *color2*)
(defvar *color3*)
(defvar *color4*)
(defvar *color5*)
(defvar *color6*)

(defvar *constant-cycle?* nil)
(defvar *rotate?* t)

(defvar *gl-canvas*)

(defun rotate-vars (&rest seq)
  (let ((tmp (var-value (elt seq 0))))
    (map nil #'(lambda (a b) (setf (var-value a) (var-value b)))
	 (subseq seq 0 (- (length seq) 1))
	 (subseq seq 1))
    (setf (var-value (car (last seq))) tmp)))

(let ((rot 0)
      (mut (make-instance 'sb-thread:mutex)))
  (defun draw ()
    "Draw a simple cube"
    (sb-thread:with-mutex (mut :wait-p nil)
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (gl:rotate (if *rotate?* (incf rot 0.5) rot) 1.0 1.0 0.5)
      (gl:begin :quads)
      (labels ((rectangle (c vs)
		 (let ((c (coerce (string-trim '(#\#) c) 'list)))
		   (gl:color (hex2int (subseq c 0 2))
			     (hex2int (subseq c 2 4))
			     (hex2int (subseq c 4 6)))
		   (mapcar #'(lambda (v) (apply #'gl:vertex v)) vs))))
	(let ((bigalpha '(-1 -1  1))
	      (bigbeta '( 1 -1  1))
	      (biggamma '( 1 -1 -1))
	      (bigdelta '(-1 -1 -1))
	      (bigepsilon '(-1  1  1))
	      (bigzeta '( 1  1  1))
	      (bigeta '( 1  1 -1))
	      (bigtheta '(-1  1 -1)))
	  (rectangle (var-value *color1*) (list bigbeta bigalpha bigdelta biggamma))
	  (rectangle (var-value *color2*) (list bigalpha bigepsilon bigtheta bigdelta))
	  (rectangle (var-value *color3*) (list bigalpha bigbeta bigzeta bigepsilon))
	  (rectangle (var-value *color4*) (list biggamma bigeta bigzeta bigbeta))
	  (rectangle (var-value *color5*) (list biggamma bigdelta bigtheta bigeta))
	  (rectangle (var-value *color6*) (list bigepsilon bigzeta bigeta bigtheta)))
	(when *constant-cycle?*
	  (rotate-vars *color1* *color2* *color3* *color4* *color5* *color6*))
	(gl:end)
	(gl:flush)
	(glt:window-swap-buffers *gl-canvas*)))
    (tk:after 8 #'draw)))

(defmacro init-string-vars (&rest names)
  `(setf ,@(apply #'append (mapcar #'(lambda (name) `(,name (string-var))) names))))

(defmacro with-string-vars ((&rest names) &rest body)
  `(let (,@(mapcar #'(lambda (name) `(,name (string-var))) names))
     ,@body))

(defun setup-gl-rendering-options ()
	  (gl::enable      :blend)
	  (gl::enable      :depth-test)
	  (gl::blend-func  :src-alpha :one-minus-src-alpha)
	  (gl::clear-color 1.0 1.0 1.0 0.0)
	  (gl::clear-depth 1.0)
	  (gl::cull-face   :back)
	  (gl::enable      :cull-face)
	  (gl::matrix-mode :projection))  

(defun test-glt ()
  (with-main-window (root win) (:width 1280 :height 1024)
    (init-string-vars *color1* *color2* *color3* *color4* *color5* *color6*)
    (with-string-vars (alpha beta gamma delta epsilon)
      (labels ((rotate-colors ()
		 (rotate-vars
		  *color1* *color2* *color3* *color4* *color5* *color6*))
	       (rotate-words () (rotate-vars alpha beta gamma delta epsilon)))
	(with-mfd win (mfd center-frame)
	  (:expand-buttons? t
	   :panel-defs-top (list (list "This"    #'rotate-words alpha)
			         (list "is"      #'rotate-words beta)
				 (list "just"    #'rotate-words gamma)
				 (list "an"      #'rotate-words delta)
				 (list "example" #'rotate-words epsilon))
	   :panel-defs-bottom '(("OK"))
	   :panel-defs-left (list
			     (list "Auto"  #'(lambda ()
					       (setf *constant-cycle?*
						     (not *constant-cycle?*))))
			     (list "Rotate" #'(lambda ()
						(setf *rotate?*
						      (not *rotate?*))))
			     (list "Close" #'(lambda () (window-destroy root))))
	   :panel-defs-right (list (list "#0000ff" #'rotate-colors *color1*)
				   (list "#00ff00" #'rotate-colors *color2*)
				   (list "#00ffff" #'rotate-colors *color3*)
				   (list "#ff0000" #'rotate-colors *color4*)
				   (list "#ff00ff" #'rotate-colors *color5*)
				   (list "#ffff00" #'rotate-colors *color6*)))
	  ;;(let ((canv (canvas :parent center-frame)))
	  (setf *gl-canvas* (canvas :parent center-frame))
	  (pack *gl-canvas* :expand t :fill "both")
	  (glt:window-bind-gl        *gl-canvas*)
	  (glt:window-gl-set-current *gl-canvas*)
	  (setup-gl-rendering-options)
	  
	  (grid mfd :row 0 :column 0 :columnspan 2 :sticky "enws")
	  (font-create :name   "Courier-New"
		       :family "Courier New" :size 15
		       :weight "bold")
	  (let ((console
		 (tk-user::textconsole win
		      :init    "nil"
		      :font    "Courier-New"
		      :height  20
		      :handler #'(lambda (ev svar entry txt sb font console)
				   (declare (ignore ev))
				   (eval
				    `(progn
				       (in-package ,:glt)
				       (let ((root ,root)
					     (win          ,win)
					     (mfd          ,mfd)
					     (center-frame ,center-frame)
					     (alpha            ,alpha)
					     (beta            ,beta)
					     (gamma            ,gamma)
					     (delta            ,delta)
					     (epsilon            ,epsilon)
					     (string-var   ,svar)
					     (entry        ,entry)
					     (txt          ,txt)
					     (sb           ,sb)
					     (font         ,font)
					     (console      ,console))
					 (declare (ignorable root win mfd
							     center-frame
							     alpha beta gamma delta epsilon
							     entry txt sb
							     string-var
							     font console))
					 ,@(list (read-from-string
						  (var-value svar))))))))))
	    (grid console :row 1 :column 0 :columnspan 2 :sticky "enws"))
	  (grid-rowconfigure    win 0 :weight 5)
	  (grid-rowconfigure    win 1 :weight 1)
	  (grid-columnconfigure win 0 :weight 1)
	  (grid-columnconfigure mfd 1 :weight 1)
	  (grid-rowconfigure    mfd 0 :weight 1)
	  (grid-rowconfigure    mfd 1 :weight 8)
	  (grid-rowconfigure    mfd 2 :weight 1)
	  (tk:after 8 #'draw))))))

(defun install ()
  (save-lisp-and-die "run.exe"
		     :toplevel #'glt::test-glt
		     :executable t
		     :application-type :gui))

(defun install-console ()
  (save-lisp-and-die "runConsole.exe"
		     :toplevel #'glt::test-glt
		     :executable t))
		     
(export '(test-glt))
