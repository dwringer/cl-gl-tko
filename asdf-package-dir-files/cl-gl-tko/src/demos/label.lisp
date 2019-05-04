(in-package :tk-user)

(demopanels::define-demo-window label-demo (f :title "Label demo")
  ()
  ((create-demo-panel (parent)
     (let ((demo-panel (frame :parent parent)))
       (pack demo-panel :side "top" :expand t :fill "both")
       (let ((left  (frame :parent demo-panel))
	     (right (frame :parent demo-panel)))
	 (pack left  :side "left" :expand t :fill "both" :padx 10 :pady 10)
	 (pack right :side "left" :expand t :fill "both" :padx 10 :pady 10)
	 (let ((l1 (label :parent left
			  :text   "First label"))
	       (l2 (label :parent left
			  :text   "Second label, raised"
			  :relief "raised"))
	       (l3 (label :parent left
			  :text   "Third label, sunken"
			  :relief "sunken")))
	   (pack l1 :side "top" :anchor "w" :expand t :pady 2 :ipadx 2)
	   (pack l2 :side "top" :anchor "w" :expand t :pady 2 :ipadx 2)
	   (pack l3 :side "top" :anchor "w" :expand t :pady 2 :ipadx 2)
	   (let ((l4      (label :parent right
				 :relief "sunken"
				 :image  (image-create-photo
					  :file   "images/ouster.png"
					  :format "png")
				 :borderwidth 2))
		 (caption (label :parent right
				 :text   "Tcl/Tk Proprietor")))
	     (pack l4      :side "top")
	     (pack caption :side "top")))))))
  ("Five labels are displayed below: three textual ones"
   "on the left, and a bitmap label and a text label on"
   "the right. Labels are pretty boring because you can't"
   "do anything with them.")
  (create-demo-panel f))
