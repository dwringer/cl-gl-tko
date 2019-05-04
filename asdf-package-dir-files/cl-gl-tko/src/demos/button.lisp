(in-package :tk-user)

(demopanels:define-demo-window button-demo (f :title "Button demo")
  ()
  ((color-refresh (widget frame)
     (let ((color  (window-cget widget :-text))
	   (stylew (window-cget widget :-style))
	   (stylef (window-cget frame  :-style)))
       (mapcar #'(lambda (style) (style-configure style :background color))
	       (list stylew stylef))))
   (create-demo-panel (parent)
     (let ((demo-panel (frame :parent parent :style "Demo.TFrame")))
       (pack demo-panel :side "top" :expand t :fill "both")
       (let ((colors  '("Peach Puff" "Light Blue" "Sea Green" "Yellow"))
	     (buttons nil))
	 (dolist (color colors)
	   (let ((b (button :parent demo-panel
			    :width  10
			    :style  "Demo.TButton")))
	     (setf buttons (append buttons (list b)))
	     (window-configure b :text color)
	     (bind-command b #'(lambda (&aux (w b)) (color-refresh w demo-panel)))
	     (pack b :side "top" :expand t :pady 2)))))))
  ("If you click on any of the four buttons below, the"
   "background of the button area will change to the"
   "color indicated in the button.  You can press <Tab>"
   "to move among the buttons, then press <Space> to"
   "invoke the current button.")
  (create-demo-panel f))
