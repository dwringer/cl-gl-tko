(in-package :tk-user)

(demopanels:define-demo-window checkbutton-demo (f :title "Checkbutton demo")
  (safety vars b0 vb0 vb1 vb2 vb3)
  ((show-vars ()
     (window-configure vb0
       :text (format nil "~8A ~8A" "safety:" (var-value safety)))
     (window-configure vb1
       :text (format nil "~8A ~8A" "wipers:" (var-value (elt vars 0))))
     (window-configure vb2
       :text (format nil "~8A ~8A" "brakes:" (var-value (elt vars 1))))
     (window-configure vb3
       :text (format nil "~8A ~8A" "sober:"  (var-value (elt vars 2)))))
   (safety-changed ()
     (cond ((string= (var-value safety) "none")
	    (dolist (var vars)
	      (setf (var-value var) 0)))
	   ((string= (var-value safety) "all")
	    (let ((values nil))
	      (dolist (var vars)
		(setf values (append values (list (var-value var)))))
	      (if (and (member 1 values) (member 0 values))
		  (progn (setf (var-value safety) "partial")
			 (setf (window-state  b0) "alternate"))
		  (progn (when (member 1 values)
			   (dolist (var vars)
			     (setf (var-value var) 0)))
			 (setf (var-value safety) "none")
			 (setf (window-state  b0) "!selected"))))))
     (show-vars))
   (value-changed ()
     (let ((values nil))
       (dolist (var vars)
	 (setf values (append values (list (var-value var)))))
       (if (not (member 0 values))
	   (setf (var-value safety) "all")
	   (if (not (member 1 values))
	       (progn (setf (var-value safety) "none")
		      (setf (window-state  b0) "!selected"))
	       (progn (setf (var-value safety) "partial")
		      (setf (window-state  b0) "alternate")))))
     (show-vars))
   (create-button-panel (parent)
     (let ((left (labelframe :parent parent :text "Checkbuttons")))
       (setf safety (string-var "none"))
       (let ((wipers (integer-var 0))
	     (brakes (integer-var 0))
	     (sober  (integer-var 0)))
	 (setf vars (list wipers brakes sober))
	 (setf b0   (checkbutton :parent   left
				 :text     "Safety Check"
				 :variable safety
				 :onvalue  "all"
				 :offvalue "none"
				 :command  #'safety-changed))
	 (let ((b1 (checkbutton :parent   left
				:text     "Wipers OK"
				:variable wipers
				:command  #'value-changed))
	       (b2 (checkbutton :parent   left
				:text     "Brakes OK"
				:variable brakes
				:command  #'value-changed))
	       (b3 (checkbutton :parent   left
				:text     "Driver Sober"
				:variable sober
				:command  #'value-changed)))
	   (pack b0 :side "top" :pady 2 :anchor "w")
	   (dolist (b (list b1 b2 b3))
	     (pack b :side "top" :pady 2 :padx 15 :anchor "w"))))
       left))
   (create-var-panel (parent)
     (let ((right (labelframe :parent parent
			      :text   "Checkbutton Control Variables")))
       (setf vb0 (label :parent right :font (list "Courier" 10)))
       (setf vb1 (label :parent right :font (list "Courier" 10)))
       (setf vb2 (label :parent right :font (list "Courier" 10)))
       (setf vb3 (label :parent right :font (list "Courier" 10)))
       (pack vb0 :anchor "nw" :pady 3)
       (pack vb1 :anchor "nw" :pady 3)
       (pack vb2 :anchor "nw" :pady 3)
       (pack vb3 :anchor "nw" :pady 3)
       (show-vars)
       right))
   (create-demo-panel (parent)
     (let ((demo-panel (frame :parent parent)))
       (pack demo-panel :side "top" :expand t :fill "both")
       (let ((left  (create-button-panel demo-panel))
	     (right (create-var-panel    demo-panel)))
	 (pack left  :side "left" :expand t :fill "both" :padx 10)
	 (pack right :side "left" :expand t :fill "both" :padx 10)))))
  ("Four checkbuttons are displayed below.  If you click"
   "on a button, it will toggle the button's selection"
   "state and set a control variable to a value"
   "indicating the state of the checkbutton."
   (format nil "~%~%The first button follows the state of")
   "the other three. If only some of the three are checked,"
   "the first button will display the 'alternate'"
   "(tri-state) mode.")
  (create-demo-panel f))
