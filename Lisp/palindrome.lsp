(defun palindrome (x)
  (equal x (reverse x))  ;check if x = (reverse x)
)


(defvar a) ;define a variable
(defun rd () ;input a number to variable a
    (setq a (read))
    (print (palindrome a))
)
(rd)
