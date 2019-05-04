(in-package :tk-user)

(demopanels:define-demo-window unicode-label-demo
    (f :title "Label with Unicode Demo")
  ()
  ((create-demo-panel (parent)
     (let ((demo-panel (frame :parent parent)))
       (pack demo-panel :expand t :fill "both" :padx 25)
       (let ((row 0))
	 (mapcar
	  #'(lambda (assignment)
	      (let ((k (car assignment))
		    (v (cdr assignment)))
		(grid (label :parent demo-panel
			     :text   k
			     :anchor "nw")
		      :row row :column 0 :sticky "ew" :pady 0)
		(grid (label :parent demo-panel
			     :text   v
			     :anchor "nw"
			     :font   '("Helvetica" 12 "italic"))
		      :row row :column 1 :sticky "ew" :pady 0))
	      (incf row))
	  (sort (list (cons "Arabic"
			    (format nil "~{~C~} ~{~C~}"
				    '(#\ARABIC_LETTER_ALEF
				      #\ARABIC_LETTER_LAM
				      #\ARABIC_LETTER_KAF
				      #\ARABIC_LETTER_LAM
				      #\ARABIC_LETTER_MEEM
				      #\ARABIC_LETTER_TEH_MARBUTA)
				    '(#\ARABIC_LETTER_ALEF
				      #\ARABIC_LETTER_LAM
				      #\ARABIC_LETTER_AIN
				      #\ARABIC_LETTER_REH
				      #\ARABIC_LETTER_BEH
				      #\ARABIC_LETTER_YEH
				      #\ARABIC_LETTER_TEH_MARBUTA)))
		      (cons "Trad. Chinese"
			    (format nil "~{~C~}"
				    '(#\U4E2D #\U570B #\U7684
				      #\U6F22 #\U5B57)))
		      (cons "Simp. Chinese"
			    (format nil "~{~C~}"
				    '(#\U6C49 #\U8BED)))
		      (cons "Greek"
			    (format nil "~{~C~} ~{~C~}"
				    '(#\GREEK_CAPITAL_LETTER_EPSILON
				      #\GREEK_SMALL_LETTER_LAMDA
				      #\GREEK_SMALL_LETTER_LAMDA
				      #\GREEK_SMALL_LETTER_ETA
				      #\GREEK_SMALL_LETTER_NU
				      #\GREEK_SMALL_LETTER_IOTA
				      #\GREEK_SMALL_LETTER_KAPPA
				      #\GREEK_SMALL_LETTER_ETA_WITH_TONOS)
				    '(#\GREEK_SMALL_LETTER_GAMMA
				      #\GREEK_SMALL_LETTER_LAMDA
				      #\GREEK_SMALL_LETTER_OMEGA_WITH_TONOS
				      #\GREEK_SMALL_LETTER_SIGMA
				      #\GREEK_SMALL_LETTER_SIGMA
				      #\GREEK_SMALL_LETTER_ALPHA)))
		      (cons "Hebrew"
			    (format nil "~{~C~}"
				    '(#\HEBREW_LETTER_KAF
				      #\HEBREW_LETTER_TAV
				      #\HEBREW_LETTER_BET
				      #\HEBREW_LETTER_AYIN
				      #\HEBREW_LETTER_BET
				      #\HEBREW_LETTER_RESH
				      #\HEBREW_LETTER_YOD
				      #\HEBREW_LETTER_TAV)))
		      (cons "Japanese"
			    (format nil "~{~C~} ~{~C~}"
				    '(#\U65E5 #\U672C #\U8A9E
				      #\HIRAGANA_LETTER_NO
				      #\HIRAGANA_LETTER_HI
				      #\HIRAGANA_LETTER_RA
				      #\HIRAGANA_LETTER_GA
				      #\HIRAGANA_LETTER_NA)
				    '(#\U6F22 #\U5B57
				      #\HIRAGANA_LETTER_TO
				      #\KATAKANA_LETTER_KA
				      #\KATAKANA_LETTER_TA
				      #\KATAKANA_LETTER_KA
				      #\KATAKANA_LETTER_NA)))
		      (cons "Korean"
			    (format nil "~{~C~} ~{~C~}"
				    '(#\HANGUL_SYLLABLE_DAE
				      #\HANGUL_SYLLABLE_HAN
				      #\HANGUL_SYLLABLE_MIN
				      #\HANGUL_SYLLABLE_GUG
				      #\HANGUL_SYLLABLE_YI)
				    '(#\HANGUL_SYLLABLE_HAN
				      #\HANGUL_SYLLABLE_GEUL)))
		      (cons "Russian"
			    (format nil "~{~C~} ~{~C~}"
				    '(#\CYRILLIC_CAPITAL_LETTER_ER
				      #\CYRILLIC_SMALL_LETTER_U
				      #\CYRILLIC_SMALL_LETTER_ES
				      #\CYRILLIC_SMALL_LETTER_ES
				      #\CYRILLIC_SMALL_LETTER_KA
				      #\CYRILLIC_SMALL_LETTER_I
				      #\CYRILLIC_SMALL_LETTER_SHORT_I)
				    '(#\CYRILLIC_SMALL_LETTER_YA
				      #\CYRILLIC_SMALL_LETTER_ZE
				      #\CYRILLIC_SMALL_LETTER_YERU
				      #\CYRILLIC_SMALL_LETTER_KA))))
	   #'(lambda (a b) (string< (car a) (car b)))))
	 (grid-columnconfigure demo-panel 1 :weight 1)))))
  ("This is a sample of Tk's support for languages that use"
   "non-Western character sets.  However, what you will actually"
   "see below depends largely on the character sets you have"
   "installed; and what you see for characters that are not"
   "present varies greatly between platforms as well.  The strings"
   "are assembled from character literals ( #\\... ) to"
   "spell them with readable names wherever possible.")
  (create-demo-panel f))
