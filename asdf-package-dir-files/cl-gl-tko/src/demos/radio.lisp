(in-package :tk-user)

(let (size_0 weight_0)
  (demopanels:define-demo-window radiobutton-demo (f :title "Radiobutton demo")
    (size color align vb0 imglabel-width-min imglabel-width-max)
    ((create-points-panel (parent)
       (let* ((lframe (labelframe :parent parent
				  :text   "Point Size"
				  :style  "Points.TLabelframe"
				  :width  100)))
	 (let ((fontstr (style-lookup "Points.TLabelframe.Label" :-font)))
	   (let ((family (font-actual fontstr :option :-family))
		 (weight (font-actual fontstr :option :-weight)))
	     (when (null size_0)
	       (setf size_0 (font-actual fontstr :option :-size)))
	     (when (null weight_0)
	       (setf weight_0 weight))
	     (let ((points   (list 9 11 13 15 21))
		   (rbuttons nil))
	       (setf size (integer-var size_0))
	       (if (font-exists "pointsFont")
		   (font-configure "pointsFont" :size size_0 :weight weight_0)
		   (font-create :name  "pointsFont" :size   size_0
				:family family	    :weight weight))
	       (style-configure "Points.TLabelframe.Label" :font "pointsFont")
	       (dolist (p points)
		 (setf rbuttons
		       (append rbuttons
			       (list
				(radiobutton
				 :parent   lframe
				 :text     (format nil "Point size ~A" p)
				 :variable size
				 :value    p
				 :command
				 #'(lambda (&optional (v p))
				     (points-changed family v weight)))))))
	       (dolist (b rbuttons)
		 (pack b :side "top" :pady 2 :padx 2 :anchor "w" :fill "x")))))
	 lframe))
     (show-vars ()
       (window-configure vb0
	 :text (format nil "size: ~2a~ccolor: ~a~calign: ~7a"
		       (var-value size)  #\tab
		       (var-value color) #\tab
		       (var-value align))))
     (points-changed (family sz weight)
       (let ((fontstr (style-lookup "Points.TLabelframe.Label" :-font)))
	 (font-configure fontstr :family family :size sz :weight weight))
       (show-vars))
     (create-color-panel (parent)
       (let* ((lbl    (label      :parent     parent
			          :text       "Color"
			          :foreground "blue"
			          :style      "Color.TLabelframe.Label"))
	      (lframe (labelframe :parent      parent
				  :labelwidget lbl)))
	 (let ((colors (list "Red" "Green" "Blue" "Yellow" "Orange" "Purple")))
	   (setf color (string-var "Blue"))
	   (let ((rbuttons nil))
	     (dolist (c colors)
	       (setf rbuttons
		     (append rbuttons
			     (list (radiobutton
				    :parent   lframe
				    :text     c
				    :variable color
				    :value    c
				    :command  #'(lambda (&optional (c c))
						  (color-changed lbl c)))))))
	     (dolist (b rbuttons)
	       (pack b :side "top" :pady 2 :padx 2 :anchor "w" :fill "x"))))
	 lframe))
     (color-changed (lbl color)
       (window-configure lbl :foreground color)
       (show-vars))
     (create-image-panel (parent)
       (when (not (font-exists "imageLabelFont"))
	 (font-create :name   "imageLabelFont"
		      :family "Helv"
		      :size   10
		      :weight "bold"
		      :slant  "italic"))
       (let* ((lframe (labelframe :parent parent
				  :text   "Alignment"))
	      (image  (image-create-photo :file   "images/dialog_question.png"
					  :format "png"))
	      (lbl    (label :parent     lframe
			     :compound   "left"
			     :text       "Label"
			     :image      image
			     :font       "imageLabelFont"
			     :foreground "blue")))
	 (grid lbl :row 1 :column 1 :padx 5 :pady 5)
	 (let* ((char-width (parse-integer
			     (font-measure "imageLabelFont" "_")))
		(maxwidth   (floor (window-reqwidth lbl) char-width)))
	   (window-configure lbl :compound "top")
	   (let* ((minwidth (floor (window-reqwidth lbl) char-width)))
	     (window-configure lbl :compound "left" :width (- minwidth 1))
	     (setf imglabel-width-min minwidth)
	     (setf imglabel-width-max maxwidth)))
	 (let ((loc (list "Top" "Left" "Bottom" "Right")))
	   (setf align (string-var "left"))
	   (let ((rbuttons nil))
	     (dolist (l loc)
	       (setf rbuttons
		     (append
		      rbuttons
		      (list (radiobutton
			     :parent   lframe
			     :text     l
			     :variable align
			     :value    (string-downcase l)
			     :width    7
			     :command
			     #'(lambda (&optional (l l))
				 (align-changed lbl (string-downcase l))))))))
	     (grid (elt rbuttons 0) :row 0 :column 1 :sticky "n")
	     (grid (elt rbuttons 1) :row 1 :column 0 :sticky "w")
	     (grid (elt rbuttons 2) :row 2 :column 1 :sticky "s")
	     (grid (elt rbuttons 3) :row 1 :column 2 :sticky "e")
	     (grid-rowconfigure lframe 1 :weight 1)))
	 lframe))
     (align-changed (lbl value)
       (window-configure lbl :compound value)
       (window-configure lbl :width    (if (member value (list "left" "right")
					      :test #'string=)
					   (- imglabel-width-min 1)
					   imglabel-width-max))
       (show-vars))
     (create-var-panel (parent)
       (let ((right (labelframe :parent parent
				:text   "Radiobutton Control Variables")))
	 (setf vb0 (label :parent right
			  :font   (font-format "Courier" :size 10)))
	 (pack vb0 :side "left" :anchor "nw" :pady 3 :padx 15)
	 (show-vars)
	 right))
     (create-demo-panel (parent)
       (let ((demo-panel (frame :parent parent)))
	 (pack demo-panel :side "top" :expand t :fill "both")
	 (let ((points (create-points-panel demo-panel))
	       (color  (create-color-panel  demo-panel))
	       (align  (create-image-panel  demo-panel))
	       (vars   (create-var-panel    demo-panel)))
	   (pack vars :side "bottom" :padx 10 :pady 10 :fill "x"
		      :expand t :anchor "sw")
	   (pack points :side "left" :padx 10 :pady 10 :fill "both")
	   (pack color  :side "left" :padx 10 :pady 10 :fill "both")
	   (pack align  :side "left" :padx 10 :pady 10 :fill "both"
		        :expand t)))))
    ("Three groups of radiobuttons are displayed below."
     "If you click on a button then the button will become"
     "selected exclusively among all the buttons in its"
     "group.  A control variable is associated with each"
     "group to indicate which of the group's buttons is"
     "selected."
     (format nil "~%~%Selecting a Point Size or Color")
     "changes the associated Labelframe text."
     (format nil "~%~%Selecting an Alignment repositions")
     "the graphic with respect to the label i.e. 'Bottom'"
     "puts the graphic 'below' the label.")
    (create-demo-panel f)))
