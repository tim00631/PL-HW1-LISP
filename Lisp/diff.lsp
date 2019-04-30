(defun rd_file_to_list (filename)
    ;; 將路徑filename的檔案內容一行一行讀至listA裡
    ;; 第一個元素為nil
    ;; for example:
    ;; file1.txt
    ;; cat
    ;; dog
    ;; return (nil, "cat" , "dog")
    (let
        (
            (fin (open filename :if-does-not-exist nil)) ;;read a filename
            (listA nil)
        )
        (when fin
            (loop for line = (read-line fin nil) ;; read line one by one from two files
                while line
                    do (push line listA) ;;push to listA
            )
            (setq listA (reverse listA))
            (push nil listA)
            (close fin)
            (return-from rd_file_to_list listA)
        )
    )
)

(defun LCS (list1 list2)
    ;; LCS algorithm, 找出list1, list2 loggest common sequence
    ;; 並且使用dp紀錄lcs長度, path紀錄方向
    (let 
        (
            (list1_length (- (length list1) 1)) (list2_length (- (length list2) 1))
            (dp nil) ;; dp: a dynamic programming array for the length of LCS
            (path nil) ;; path: an array for record direction
        )
        (setq dp (make-array (list (+ list1_length 1) (+ list2_length 1)) :initial-element 0))
        (setq path (make-array (list (+ list1_length 1) (+ list2_length 1)) :initial-element 0))
        (loop for i from 1 to list1_length
            do (loop for j from 1 to list2_length
                do (cond
                    ((string= (nth i list1) (nth j list2)) ;; 如果兩個list 的第i,j個字相同, dp[i][j] = dp[i-1][j-1]+1 左上
                        (setf (aref dp i j) 
                            (+ (aref dp (- i 1) (- j 1)) 1) 
                        )
                        (setf (aref path i j) 0) ;; path[i][j]設成0
                    )
                    ((< (aref dp (- i 1) j) (aref dp i (- j 1))) ;; 如果左 > 上, dp[i][j] = dp[i][j-1]
                        (setf (aref dp i j)
                            (aref dp i (- j 1))
                        )
                        (setf (aref path i j) 1)    ;; path[i][j]設成1 (走左邊)
                    )
                    ((>= (aref dp (- i 1) j) (aref dp i (- j 1))) ;; 如果左 <= 上,dp[i][j] = dp[i-1][j]
                        (setf (aref dp i j)
                            (aref dp (- i 1) j)
                        )
                        (setf (aref path i j) 2) ;; path[i][j]設成2 (走上面)
                    )
                )
            )
        )
        (let
            (
                (lcslength (aref dp list1_length list2_length))
                (i list1_length) (j list2_length)
                (l1pos nil) (l2pos nil)
            )
            (loop while (> lcslength 0)
                ;; 開始儲存LCS字串的i,j位置
                do (cond
                    ((= (aref path i j) 0) ;;如果方向是左上 代表是LCS，故push它們的位置至l1pos,l2pos
                        (push i l1pos)
                        (push j l2pos)
                        (decf lcslength)
                        (decf i)
                        (decf j)
                    )
                    ((= (aref path i j) 1) ;;如果方向是左邊 往左走
                        (decf j)
                    )
                    ((= (aref path i j) 2) ;;如果方向是上面 往上走
                        (decf i)
                    )
                )
            )
            (return-from lcs (values l1pos l2pos))
        )
    )
)

(defun diff (list1 list2 l1pos l2pos)
    ;; 藉由l1pos, l2pos, 印出diff function (l1pos,l2pos是用來記錄lcs在兩個檔案中的位置)
    (let
        (
            (list1_length (- (length list1) 1)) (list2_length (- (length list2) 1))
            (lcs_curr_len 0) (i 1) (j 1)
        )
        (loop while (<= i list1_length)
            do (cond
                ((< lcs_curr_len (length l1pos)) 
                    (cond
                        ((= i (nth lcs_curr_len l1pos)) ;; 如果file1現在的位置 = lcs 答案的位置
                            (loop while (< j (nth lcs_curr_len l2pos)) ;; 換file2, 如果file2 現在的位置 < lcs答案的位置
                                do (format t "~C[32m+~a~%~C[00m" ;;印出 + ....
                                    #\ESC (nth j list2) #\ESC)
                                do (incf j) ;; 繼續檢查file2 直到 = lcs答案的位置，印出lcs
                            )
                            (format t " ~a~%" (nth i list1)) ;;承上行,印出lcs
                            (incf lcs_curr_len);; lcs current length ++
                            (incf j) ;; j++
                        )
                        ((/= i (nth lcs_curr_len l1pos)) ;;如果file1 現在的位置 != lcs答案的位置 (一定是比較小)
                            (format t "~C[31m-~a~%~C[00m" ;;印出 - ....
                            #\ESC (nth i list1) #\ESC)
                        )
                    )
                )
                ((>= lcs_curr_len (length l1pos)) ;;lcs印完之後就結束 將剩下的東西印出來
                    (return)
                )
            )
            do (incf i)
        )
        (loop while (<= i list1_length) ;; 將list1 剩下行 印出 - ....
            do (format t "~C[31m-~a~%~C[00m" #\ESC (nth i list1) #\ESC)
            do (incf i)
        )
        (loop while (<= j list2_length) ;; 將list1 剩下行 印出 + ....
            do (format t "~C[32m+~a~%~C[00m" #\ESC (nth j list2) #\ESC)
            do (incf j)
        )
    )
)

(let
    (
        (list1 (rd_file_to_list "./file1.txt"))
        (list2 (rd_file_to_list "./file2.txt"))
    )
    (multiple-value-bind (l1pos l2pos) (lcs list1 list2)
        (diff list1 list2 l1pos l2pos)
    )
)
