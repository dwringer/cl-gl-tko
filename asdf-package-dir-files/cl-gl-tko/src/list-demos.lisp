(in-package :tk-user)

(let ((load-truename (demopanels:get-truename-macro)))
  (defun list-demos ()
    (with-main-window (root frame) (:title "Common Lisp Tcl/Tk Demos"
				    :height 400 :width 600)
      (with-button-panel frame (pnl svars :orient :vertical)
	  (mapcar #'(lambda (pair)
		      (let ((name (car pair))
			    (func (cdr pair)))
			(list name
			      #'(lambda ()
				  (funcall func
					   :root root)))))
		  (let (acc)
		    (maphash #'(lambda (k v) (push (cons k v) acc))
			     demopanels::*demo-windows*)
		    (reverse acc)))
	(pack pnl :expand t :fill "both" :side "left")
	(demopanels:msg-panel frame
	      (list "These are demos adapted from the Tcl demo files"
		    "included with Python and the Python adaptations"
		    "thereof that may be found at"
		    "https://pyinmyeye.blogspot.com."
		    (format nil "~%~%Each may be loaded")
		    "individually, and the code can be inspected with"
		    "a standardized interface."))
	(demopanels:see-dismiss-panel frame :root root :path load-truename)))))
