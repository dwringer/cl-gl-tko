(in-package :tk-user)

(demopanels:define-demo-window puzzle-demo (f :title "15 Puzzle Demo")
  (xypos)
  ((puzzle-switch (button)
     (let* ((num (parse-integer (window-cget button :-text)))
	    (sx  (elt (gethash "space" xypos) 0))
	    (sy  (elt (gethash "space" xypos) 1))
	    (x   (elt (gethash num     xypos) 0))
	    (y   (elt (gethash num     xypos) 1)))
       (when (or (and (<=  (- sy .01) y (+ sy .01))
		      (<=  (- sx .26) x (+ sx .26)))
		 (and (<=  (- sx .01) x (+ sx .01))
		      (<=  (- sy .26) y (+ sy .26))))
	 (let ((swap-tmp-space
		(copy-list (gethash "space" xypos))))
	   (setf (gethash "space" xypos)
		 (copy-list (gethash num xypos)))
	   (setf (gethash num xypos)
		 (copy-list swap-tmp-space)))
	 (place button
		:relx (elt (gethash num xypos) 0)
		:rely (elt (gethash num xypos) 1)))))
   (create-demo-panel (parent)
     (let* ((bg-color   "gray80")
	    (demo-panel (frame :parent      parent
			       :borderwidth 2
			       :relief      "sunken"
			       :style       "Puzzle.TFrame"
			       :width       120
			       :height      120)))
       (pack demo-panel :side "top" :pady 1 :padx 1)
       (setf xypos (make-hash-table :test 'equal))
       (setf (gethash "space" xypos) (list 0.75 0.75))
       (let ((order (list 3 1 6 2 5 7 15 13 4 11 8 9 14 10 12)))
	 (dotimes (i 15)
	   (let ((num (elt order i)))
	     (setf (gethash num xypos) (list (* (mod   i 4) 0.25)
					     (* (floor i 4) 0.25)))
	     (let ((button (button :parent demo-panel
				   :text   (format nil "~A" num)
				   :style  "Puzzle.TButton")))
	       (bind-command button #'(lambda (&aux (b button)) (puzzle-switch b)))
	       (place button
		      :relx      (elt (gethash num xypos) 0)
		      :rely      (elt (gethash num xypos) 1)
		      :relwidth  0.25
		      :relheight 0.25)))))
       (style-configure "Puzzle.TFrame"  :background bg-color)
       (style-configure "Puzzle.TButton" :background bg-color))))
  ("A 15-puzzle appears below as a collection of buttons."
   "Click on any of the pieces next to the space and that"
   "piece will slide over the space."
   (format nil "~%~%Continue this until the pieces are")
   "arranged in numerical order from upper-left to"
   "lower-right.")
  (create-demo-panel f))
