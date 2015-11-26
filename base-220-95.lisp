;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LispFurc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright 2015, Michal "phoe" Herda.
;;;;
;;;; The whole project is licensed under GPLv3.
;;;;

(in-package #:lispfurc)

;;;;=========================================================
;;;; FURCADIA BASE-220 AND BASE-95 CONVERSION

(labels
    ((cut-last (string)
       (if (equal "" string)
	   (values "" nil)
	   (values (subseq string 0 (1- (length string)))
		   (aref string (1- (length string))))))
     (rec (num base off)
       (if (eq num 0)
	   nil
	   (append (rec (floor num base) base off)
		   (list (code-char (+ off (nth-value 1 (floor num base)))))))))
  
  (defun from-220 (string)
    "This converts a number from Furcaida's base-220 to integer."
    (if (equal "" string)
	0
	(+ (- (char-int (nth-value 1 (cut-last string))) 35)
	   (* 220 (from-220 (nth-value 0 (cut-last string)))))))

  (defun to-220 (num)
    "This converts a number from integer to Furcaida's base-220."
    (if (null (rec num 220 35))
	"#"
	(concatenate 'string (rec num 220 35))))
  
  (defun from-95 (string)
    "This converts a number from Furcaida's base-95 to integer."
    (if (equal "" string)
	0
	(+ (- (char-int (nth-value 1 (cut-last string))) 32)
	   (* 95 (from-95 (nth-value 0 (cut-last string)))))))

  (defun to-95 (num)
    "This converts a number from integer to Furcaida's base-95."
    (if (null (rec num 95 32))
	"#"
	(concatenate 'string (rec num 95 32)))))
