;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LispFurc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright 2015, Michal "phoe" Herda.
;;;;
;;;; The whole project is licensed under GPLv3.
;;;;

(in-package #:lispfurc)

;;;;=========================================================
;;;; (NESTED) ALIST HELPER FUNCTIONS

(defun value (key alist)
  "Given an alist and a key, returns a respective value or NIL if it's not found."
  (cdr (assoc key alist)))

(defun key (value alist)
  "Given an alist and a value, returns a respective key or NIL if it's not found."
  (car (rassoc value alist)))

(defun alist-get-diff (alist mdf &optional (test #'eq))
  "This function takes two alists and returns the second alist while omitting the keys for
which the two alists have equal value."
  (iter (for (key . value) in mdf)
	(unless (funcall test (value key alist) (value key mdf))
	  (collect (cons key (value key mdf))))))

(defun alist-apply-diff (alist mdf)
  "This function takes two alists and returns an alist with the same keys as the first and
values that:

* come from the second alist if they exist in it under respective keys,
* come from the first alist otherwise.

No new keys are added to the alist, even if they exist in the second one."
  (iter (for (key . value) in alist)
	(if (assoc key mdf)
	    (collect (cons key (value key mdf)))
	    (collect (cons key value)))))

(defun alist-set-value (alist value keylist &optional (test #'eq))
  "This function takes a set of nested alists as first argument, a value, a list of valid 
keys in order of nestedness, and returns the same nested alist with the value inserted
at the location pointed by the keylist.

This function will create the necessary alist structure if it doesn't exist in the original
alist. If used with value equal to NIL, this can be used solely to create nested alist
structure.

This function may (and will) change the order of alist elements and remove duplicates."
  (if (null keylist)
      value
      (append (iter (for (key . value) in alist)
		    (unless (funcall test key (car keylist))
		      (collect (cons key value))))
	      (cons (cons (car keylist)
			  (alist-set-value (value (car keylist) alist)
					   (cdr keylist) value))
		    nil))))

(defun alist-get-value (alist keylist)
  "This function takes a set of nested alists as first argument, a list of valid keys in
order of nestedness, and returns the value at the end of the keylist.

Returns NIL if the keylist does not match the alist structure."
  (if (null keylist)
      alist
      (alist-get-value (value (car keylist) alist)
		       (cdr keylist))))

(defun alist-remove (alist keyword &optional (keylist nil) (test #'eq))
  "This function, given an alist and a keyword, returns a new alist with the cons holding
a keyword in its car removed.

Optionally, if given a third argument being a list of valid keys in order of nestedness,
this function will return an alist with the cons removed at a given depth.

This function may (and will) change the order of alist elements and remove duplicates."
  (if (null keylist)
      (iter (for (key . value) in alist)
	    (unless (funcall test key keyword)
	      (collect (cons key value))))
      (append (iter (for (key . value) in alist)
		    (unless (funcall test key (car keylist))
		      (collect (cons key value))))
	      (cons (cons (car keylist)
			  (alist-remove (value (car keylist) alist)
					keyword (cdr keylist)))
		    nil))))

(defmacro alist-gen (&rest args)
  "This generates an alist based on its arguments list.
If any key has a default value, it gets evaluated and set as the value within the alist.
Otherwise, that key's its value is set to NIL."
  (flet ((conser (arg)
	   (if (consp arg)
	       (cons (first arg) (second arg))
	       (cons arg nil))))
    (list 'quote (mapcar #'conser args))))

;;;;=========================================================
;;;; PLIST HELPER FUNCTIONS

(defun string=-getf (plist indicator)
  "This is a version of getf utilizing string= for comparison. Given a plist and a key, returns
a value."
  (loop for key in plist by #'cddr
     for value in (rest plist) by #'cddr
     when (string= key indicator)
     return value))

(defun string=-getf-key (plist indicator)
  "This is a version of getf utilizing string= for comparison. Given a plist and a value,
returns a key."
  (loop for key in plist by #'cddr
     for value in (rest plist) by #'cddr
     when (string= value indicator)
     return key))
