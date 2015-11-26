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
	0  "Cat Gray"   1  "Arctic"      2  "Black"    3  "Mule Tan"
	4  "Dust"       5  "Chocolate"   6  "Lavender" 7  "Wolf Gray"
	8  "Fox Orange" 9  "Horse Brown" 10 "Tawny"    11 "Mocha"
	12 "Ruddy"      13 "Cream"       14 "Yellow"   15 "Grass Green"
	16 "Burgundy"   17 "Pinewood"    18 "Sea Blue" 19 "Violet"
	20 "Red"        21 "White"       22 "Navy"     23 "Cloudy Gray"
	24 "Royal Blue"))
      
      (color-table-2 ;; Used for hair.
       (list
	0  "Blood Red"      1  "Courage Red"   2  "Merry Red"       3  "Ember Orange"
	4  "Hearty Brown"   5  "Burnt Orange"  6  "Busy Orange"     7  "August Orange"
	8  "Noble Brown"    9  "Mahogany"      10 "Clever Maize"    11 "Pure Gold"
	12 "Moon Yellow"    13 "Sun Yellow"    14 "Tan"             15 "Woodland Green"
	16 "Friendly Green" 17 "Lucky Green"   18 "Travelers Green" 19 "Aquacyan"
	20 "Deepsea Blue"   21 "Proud Blue"    22 "Lightsky Blue"   23 "Syndira Blue"
	24 "Straight Blue"  25 "Royal Purple"  26 "Lonely Orchid"   27 "Spiritual Purple"
	28 "Mad Green"      29 "Royal Blue"    30 "Passion"         31 "Bliss Red"
	32 "Blush Pink"     33 "Twilight Sigh" 34 "Dreamer Blue"    35 "Shadow Gray"
	36 "Neutral Gray"   37 "Tragic Gray"   38 "Winter Gray"     39 "Indigo"
	40 "Black"          41 "Cloudy Gray"   42 "White"           43 "Pinewood"
	44 "Green Yellow"))
      
      (color-table-3 ;; Used for eyes.
       (list
	0  "Mahogany"     1  "Tiger Orange" 2  "Moon Yellow" 3  "Warm Gold"
	4  "Pond Green"   5  "Light Blue"   6  "Sea Blue"    7  "Storm Gray"
	8  "Dust"         9  "China Blue"   10 "Medium Blue" 11 "Dark Blue"
	12 "Hazel"        13 "Blue Green"   14 "Royal Blue"  15 "Light Brown"
	16 "Medium Brown" 17 "Dark Brown"   18 "Black"       19 "Albino Pink"
	20 "Mink Mauve"   21 "Chocolate"    22 "Silver"      23 "Amethyst"
	24 "Ember Orange" 25 "Sunshine"     26 "Mad Green"   27 "Gloomy Grey"
	28 "Minty Green"  29 "Navy Blue"))
      
      (color-table-4 ;; Used for badge.
       (list
	0  "Blood Red"      1  "Courage Red"   2  "Merry Red"       3  "Ember Orange"
	4  "Hearty Brown"   5  "Burnt Orange"  6  "Busy Orange"     7  "August Orange"
	8  "Noble Brown"    9  "Mahogany"      10 "Clever Maize"    11 "Pure Gold"
	12 "Moon Yellow"    13 "Sun Yellow"    14 "Tan"             15 "Woodland Green"
	16 "Friendly Green" 17 "Lucky Green"   18 "Travelers Green" 19 "Aquacyan"
	20 "Deepsea Blue"   21 "Proud Blue"    22 "Lightsky Blue"   23 "Syndira Blue"
	24 "Straight Blue"  25 "Royal Purple"  26 "Lonely Orchid"   27 "Spiritual Purple"
	28 "Mad Green"      29 "Royal Blue"    30 "Passion"         31 "Bliss Red"
	32 "Blush Pink"     33 "Twilight Sigh" 34 "Dreamer Blue"    35 "Shadow Gray"
	36 "Neutral Gray"   37 "Tragic Gray"   38 "Winter Gray"     39 "Indigo"))
      
      (color-table-5 ;; Used for vest, bracers, cape, boots and trousers.
       (list
	0  "Red"          1  "Horse Brown"     2  "Gray Brown"     3  "Fox Orange"
	4  "Cream Brown"  5  "Yellow"          6  "Pea Green"      7  "Grass Green"
	8  "Avocado"      9  "Sea Blue"        10 "Blue Gray"      11 "Wolf Gray"
	12 "Arctic"       13 "Violet Blue"     14 "Spring Green"   15 "Burgundy"
	16 "Green Yellow" 17 "Light Chocolate" 18 "Dark Chocolate" 19 "Light Navy"
	20 "Dark Navy"    21 "Light Dust"      22 "Dark Dust"      23 "Gray"
	24 "Black"        25 "Light Mule"      26 "Dark Mule"      27 "Violet"
	28 "Royal Blue"   29 "White")))

  (flet
      ((color-table (keyword)
	 (case keyword
	   ((:fur :markings)                        color-table-1)
	   (:hair                                   color-table-2)
	   (:eyes                                   color-table-3)
	   (:badge                                  color-table-4)
	   ((:vest :bracers :cape :boots :trousers) color-table-5)
	   (otherwise (error "Keyword not matching any of:
:FUR :MARKINGS :HAIR :EYES :BADGE :VEST :BRACERS :CAPE :BOOTS :TROUSERS")))))
    
    (defun color-char (keyword string)
      "This, given a keyword designating a color-code slot and a color name, returns a proper
character for the color code."
      (check-type string string)
      (check-type keyword keyword)
      (string=-getf-key (color-table keyword) string))

    (defun color-string (keyword char)
      "This, given a keyword designating a color-code slot and a character for the color code,
returns a proper color name."
      (check-type char character)
      (check-type keyword keyword)
      (getf (color-table keyword) char))))
