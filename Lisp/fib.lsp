(defun fib1 (n)
    (if (< n 2)
        n
        (+ (fib1 (- n 1)) (fib1 (- n 2)))  ;等待底層的function回傳值,較佔用stack空間
    )
)

(defun fib2 (n)
    (defun fib_cal (n a b)
        (if (> n 0) ;當 n > 0 , 計算下一項之和 && n -= 1
            (fib_cal (- n 1) b (+ a b)) ;將 前兩項一起帶入下一次function call ,不佔用stack空間
            a)
    )
    (fib_cal n 0 1) ;tail call
)

(defvar a) ;define a variable
(defun rd () ;input a number to variable a
    (setq a (read))
    (trace fib1)
    (trace fib2)
    (print (fib1 a))
    (print (fib2 a))
)
(rd)

