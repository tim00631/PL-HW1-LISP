;;; file prime.lsp


(defun prime (n)
    (if (< n 2)
        (return-from prime nil) ; n<2 is not a prime
        (do ((i 2 (+ i 1))) ; 除數i從 2 increment
            ((if (> i 1) ; if 除數i > 1
                (if (< i n) ; if 除數 i < n
                    (if (= (mod n i) 0) ; if (n 對 i 取餘數 == 0) 意思就是整除 代表不是質數 
                        (return-from prime nil)  ; return NULL
                    ) 
                (return-from prime t)   ;i >= n break 代表找不到因數 故為質數 
                )
            ))
        )
    )
)
(defvar a) ;define a variable
(defun rd () ;input a number to variable a
    (setq a (read))
    (print (prime a))
)
(rd)


