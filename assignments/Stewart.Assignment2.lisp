;; Name: Ajani Stewart
;; Date: February 7, 2019
;; Assignment 2

(defun plus-3 (number)
  (+ 3 number))

(defun circle-area (radius)
  (* (expt radius 2) 3.14))

(defun parallelepiped (length width height)
  (* length width height))

(defun longishp (list)
  (> (length list) 5))

(defun solongp (list threshold)
  ( > (length list) threshold))

(defun dieting (list)
  (loop for item in list
       sum (* (first item) (third item))))

(defun eating (list)
  (loop for item in list
     collect (second item)))

(defun remove-third (list)
  (loop for item in list
     for i from 0 to (- (length list) 1)
     unless (= i 2)
     collect item))

(defun replace-second (list replacement)
  (loop for item in list
     for i from 0 to (- (length list) 1)
     if (= i 1)
     collect replacement
     else collect item))

(defun hungry (list)
  (loop for item in list
       collect (reverse (cons (* (first item) (third item)) (reverse item)))))

(defun backwardsp (list)
  (equal list (reverse list)))

(defun language (words)
  (loop for word in words
	when (or (equal word 'one) (equal word 'two) (equal word 'three)) collect 'english
        else  when (or (equal word 'un) (equal word 'deux) (equal word 'trois)) collect 'french
	else when (or (equal word 'uno) (equal word 'dos) (equal word 'tres)) collect 'spanish
	else when (or (equal word 'ekahi) (equal word 'elua) (equal word 'ekolu)) collect 'hawaiian
	else collect 'unknown))

(defun ranger (numbers expr)
  (if (not (numberp expr)) "INVALID SECOND ARGUMENT"
      (loop
	 with max-below = most-negative-fixnum
	 with min-above = most-positive-fixnum
	 for number in numbers
	 maximize number into g-max
	 minimize number into g-min
	 when (and (> number max-below) (<= number expr)) do (setf max-below number)
	 else when (and (> min-above number) (>= number expr)) do (setf min-above number)
	 finally
	   (return
	     (cond
	       ((or (> expr g-max) (< expr g-min)) "NO RANGE")
	       ((or (= expr max-below) (= expr min-above)) t)
	       (t (float (/ (+ max-below min-above) 2))))))))

(defun palindromep (string)
  (loop
     for letter across string
     and r-letter across (reverse string)
     when (and (alphanumericp letter) (alphanumericp r-letter) (not(string-equal letter r-letter))) return nil
     finally (return t)))
       
