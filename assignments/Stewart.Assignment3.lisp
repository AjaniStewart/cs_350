					; Name: Ajani Stewart
					; Date: February 20, 2019
					; Assignment 3


(defun all-crackersp (list)
  (cond
    ((endp list) t)
    ((not (equal (first list) 'cracker)) nil)
    (t (all-crackersp (rest list)))))
(defun some-crackersp (list)
  (cond
    ((endp list) nil)
    ((equal (first list) 'cracker) t)
    (t (some-crackersp (rest list)))))
(defun shopping-forp (shopping-list item)
  (cond
    ((endp shopping-list) nil)
    ((equal (first (rest (first shopping-list))) item) t)
    (t (shopping-forp (rest shopping-list) item))))
(defun homogenyp (list item)
  (cond
    ((endp list) t)
    ((not (equal (eval (first list)) (eval item))) nil)
    (t (homogenyp (rest list) (eval item)))))
(defun non-decreasingp (list)
  (cond
    ((endp (rest list)) t)
    ((< (first (rest list)) (first list)) nil)
    (t (non-decreasingp (rest list)))))
(defun list-counter (list)
  (cond
    ((endp list) 0)
    ((listp (first list)) (+ 1 (list-counter (rest list))))
    (t (list-counter (rest list)))))
(defun long-list-counter (list threshold)
  (cond
    ((endp list) 0)
    ((and
      (listp (first list))
      (> (length (first list)) threshold))
     (+ 1 (long-list-counter (rest list) threshold)))
    (t (long-list-counter (rest list) threshold))))
(defun deep-long-list-counter (list threshold)
  (cond
    ((endp list) 0)
    ((and
      (listp (first list))
      (> (length (first list)) threshold))
     (+ 1
	(deep-long-list-counter (first list) threshold)
	(deep-long-list-counter (rest list) threshold)))
    (t (deep-long-list-counter (rest list) threshold))))
(defun eradicate (list item)
  (cond
    ((endp list) nil)
    ((not (equal (first list) item))
     (cons (first list) (eradicate (rest list) item)))
    (t (eradicate (rest list) item))))
(defun lall-crackersp (list)
  (loop for item in list
     always (equal item 'cracker)))
(defun lsome-crackersp (list)
  (loop for item in list
     thereis (equal item 'cracker)))
(defun lshopping-forp (shopping-list target)
  (loop for item in shopping-list
     thereis (equal (first (rest item)) target)))
(defun lhomogenyp (list expr)
  (loop for item in list
     always (equal (eval item) (eval expr))))
(defun lnon-decreasingp (list)
  (loop for item in list
     and prev = (first list) then item
     never (> prev item)))
(defun llist-counter (list)
  (loop for item in list
     when (listp item)
     count it))
(defun llong-list-counter (list threshold)
  (loop for item in list
     when (and (listp item) (> (length item) threshold))
     count it))
(defun ldeep-long-list-counter (list threshold))
(defun leradicate (list expr)
  (loop for item in list
     unless (equal item expr)
       collect item))
