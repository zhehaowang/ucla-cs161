; Homework 4
; Zhehao Wang, 404380075, zhehao@cs.ucla.edu


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))



(defun in-assign (assign value)
  (cond
    ((null assign) nil)
    ((= (car assign) value) t)
    ((= (+ (car assign) value) 0) nil)
    (t (in-assign (cdr assign) value))
  )
)

(defun check-clause (assign clause)
  (if (null clause)
    nil
    (if (in-assign assign (car clause))
      t
      (check-clause assign (cdr clause))
    )
  )
)

(defun check-clauses (assign delta)
  (if (null delta)
    t
    (and (check-clause assign (car delta)) (check-clauses assign (cdr delta)))
  )
)

(defun copy-n-reverse-last (array length)
  (cond
    ; don't expect this to happen
    ((null array) nil)
    ; don't epect this to happen
    ((<= length 0) nil)
    ((= length 1) (list (- 0 (car array))))
    (t (cons (car array) (copy-n-reverse-last (cdr array) (- length 1))))
  )
)

(defun reverse-list (array)
  (if (null array)
    nil
    (append (reverse-list (cdr array)) (list (car array))))
)

(defun find-backtrack-point (array)
  (cond
    ((null array) 0)
    ; 0 meaning we've found the next backtrack point
    ((> (car array) 0) 0)
    ; let and if statements are added to handle the overrided semantic of the return value of this function
    (t (+ 1 (find-backtrack-point (cdr array))))
  )
)

(defun dfs (n current delta)
  (let ((depth (length current)))
    (if (equal depth n) 
      (if (check-clauses current delta)
        current
        (let ((idx (find-backtrack-point (reverse current))))
          (if (= idx depth)
            nil
            (dfs n (copy-n-reverse-last current (- depth idx)) delta)
          )
        )
      )
      (dfs n (append current (list (+ depth 1))) delta)
    )
  )
)

(defun sat? (n delta) 
  (dfs n '() delta)
)

; (load "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/hw4.lsp")
; (solve-cnf "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/cnfs/sat/cnf_10.cnf")