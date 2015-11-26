;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LispFurc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright 2015, Michal "phoe" Herda.
;;;;
;;;; The whole project is licensed under GPLv3.
;;;;

(in-package #:lispfurc)

;;;;=========================================================
;;;; COLOR CODE HELPER FUNCTIONS

(let ((color-table-1 ;; Used for fur and markings.
       (list
	#\# "Cat Gray"   #\$ "Arctic"      #\% "Black"    #\& "Mule Tan"
	#\' "Dust"       #\( "Chocolate"   #\) "Lavender" #\* "Wolf Gray"
	#\+ "Fox Orange" #\, "Horse Brown" #\- "Tawny"    #\. "Mocha"
	#\/ "Ruddy"      #\0 "Cream"       #\1 "Yellow"   #\2 "Grass Green"
	#\3 "Burgundy"   #\4 "Pinewood"    #\5 "Sea Blue" #\6 "Violet"
	#\7 "Red"        #\8 "White"       #\9 "Navy"     #\: "Cloudy Gray"
	#\; "Royal Blue"))
      
      (color-table-2 ;; Used for hair.
       (list
	#\# "Blood Red"      #\$ "Courage Red"   #\% "Merry Red"       #\& "Ember Orange"
	#\' "Hearty Brown"   #\( "Burnt Orange"  #\) "Busy Orange"     #\* "August Orange"
	#\+ "Noble Brown"    #\, "Mahogany"      #\- "Clever Maize"    #\. "Pure Gold"
	#\/ "Moon Yellow"    #\0 "Sun Yellow"    #\1 "Tan"             #\2 "Woodland Green"
	#\3 "Friendly Green" #\4 "Lucky Green"   #\5 "Travelers Green" #\6 "Aquacyan"
	#\7 "Deepsea Blue"   #\8 "Proud Blue"    #\9 "Lightsky Blue"   #\: "Syndira Blue"
	#\; "Straight Blue"  #\< "Royal Purple"  #\= "Lonely Orchid"   #\> "Spiritual Purple"
	#\? "Mad Green"      #\@ "Royal Blue"    #\A "Passion"         #\B "Bliss Red"
	#\C "Blush Pink"     #\D "Twilight Sigh" #\E "Dreamer Blue"    #\F "Shadow Gray"
	#\G "Neutral Gray"   #\H "Tragic Gray"   #\I "Winter Gray"     #\J "Indigo"
	#\K "Black"          #\L "Cloudy Gray"   #\M "White"           #\N "Pinewood"
	#\O "Green Yellow"))
      
      (color-table-3 ;; Used for eyes.
       (list
	#\# "Mahogany"     #\$ "Tiger Orange" #\% "Moon Yellow" #\& "Warm Gold"
	#\' "Pond Green"   #\( "Light Blue"   #\) "Sea Blue"    #\* "Storm Gray"
	#\+ "Dust"         #\, "China Blue"   #\- "Medium Blue" #\. "Dark Blue"
	#\/ "Hazel"        #\0 "Blue Green"   #\1 "Royal Blue"  #\2 "Light Brown"
	#\3 "Medium Brown" #\4 "Dark Brown"   #\5 "Black"       #\6 "Albino Pink"
	#\7 "Mink Mauve"   #\8 "Chocolate"    #\9 "Silver"      #\: "Amethyst"
	#\; "Ember Orange" #\< "Sunshine"     #\= "Mad Green"   #\> "Gloomy Grey"
	#\? "Minty Green"  #\@ "Navy Blue"))
      
      (color-table-4 ;; Used for badge.
       (list
	#\# "Blood Red"      #\$ "Courage Red"   #\% "Merry Red"       #\& "Ember Orange"
	#\' "Hearty Brown"   #\( "Burnt Orange"  #\) "Busy Orange"     #\* "August Orange"
	#\+ "Noble Brown"    #\, "Mahogany"      #\- "Clever Maize"    #\. "Pure Gold"
	#\/ "Moon Yellow"    #\0 "Sun Yellow"    #\1 "Tan"             #\2 "Woodland Green"
	#\3 "Friendly Green" #\4 "Lucky Green"   #\5 "Travelers Green" #\6 "Aquacyan"
	#\7 "Deepsea Blue"   #\8 "Proud Blue"    #\9 "Lightsky Blue"   #\: "Syndira Blue"
	#\; "Straight Blue"  #\< "Royal Purple"  #\= "Lonely Orchid"   #\> "Spiritual Purple"
	#\? "Mad Green"      #\@ "Royal Blue"    #\A "Passion"         #\B "Bliss Red"
	#\C "Blush Pink"     #\D "Twilight Sigh" #\E "Dreamer Blue"    #\F "Shadow Gray"
	#\G "Neutral Gray"   #\H "Tragic Gray"   #\I "Winter Gray"     #\J "Indigo"))
      
      (color-table-5 ;; Used for vest, bracers, cape, boots and trousers.
       (list
	#\# "Red"          #\$ "Horse Brown"     #\% "Gray Brown"     #\& "Fox Orange"
	#\' "Cream Brown"  #\( "Yellow"          #\) "Pea Green"      #\* "Grass Green"
	#\+ "Avocado"      #\, "Sea Blue"        #\- "Blue Gray"      #\. "Wolf Gray"
	#\/ "Arctic"       #\0 "Violet Blue"     #\1 "Spring Green"   #\2 "Burgundy"
	#\3 "Green Yellow" #\4 "Light Chocolate" #\5 "Dark Chocolate" #\6 "Light Navy"
	#\7 "Dark Navy"    #\8 "Light Dust"      #\9 "Dark Dust"      #\: "Gray"
	#\; "Black"        #\< "Light Mule"      #\= "Dark Mule"      #\> "Violet"
	#\? "Royal Blue"   #\@ "White")))

  (labels
      ((pick-color-table (keyword)
	 (case keyword
	   ((:fur :markings)                        color-table-1)
	   (:hair                                   color-table-2)
	   (:eyes                                   color-table-3)
	   (:badge                                  color-table-4)
	   ((:vest :bracers :cape :boots :trousers) color-table-5)
	   (otherwise (error "Keyword not matching any of:
:FUR :MARKINGS :HAIR :EYES :BADGE :VEST :BRACERS :CAPE :BOOTS :TROUSERS")))))
    
    (defun get-color-char (keyword string)
      "This, given a keyword designating a color-code slot and a color name, returns a proper
character for the color code."
      (check-type string string)
      (check-type keyword keyword)
      (string=-getf-key (pick-color-table keyword) string))

    (defun get-color-string (keyword char)
      "This, given a keyword designating a color-code slot and a character for the color code,
returns a proper color name."
      (check-type char character)
      (check-type keyword keyword)
      (getf (pick-color-table keyword) char))))
