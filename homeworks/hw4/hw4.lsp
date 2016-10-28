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


(defun print-cnf (filename)
  (let ((cnf (parse-cnf filename))) (second cnf))
)

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

(defun element-abs (value)
  (if (< value 0)
    (- 0 value)
    value
  )
)

(defun array-abs (array)
  (cond
    ((null array) nil)
    (t (cons (element-abs (car array)) (array-abs (cdr array))))
  )
)

(defun copy-and-reverse-at-idx (array idx)
  (cond
    ((null array) nil)
    ((= idx 1) (cons (- 0 (car array)) (array-abs (cdr array))))
    (t (cons (car array) (copy-and-reverse-at-idx (cdr array) (- idx 1))))
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

(defun dfs (n sequence seqlen delta)
  (if (equal seqlen n) 
    (if (check-clauses sequence delta)
      sequence
      (let ((idx (find-backtrack-point (reverse sequence))))
        (if (= idx n)
          nil
          (dfs n (copy-and-reverse-at-idx sequence (- n idx)) (- n idx) delta)
        )
      )
    )
    ; without forward checking
    (dfs n sequence (+ seqlen 1) delta)

    ; debugging this forward checking logic
    ; (let ((reduced (reduce-top sequence seqlen delta)))
    ;   (if (null reduced)
    ;     ; forward checking tells us this definitely won't work, so we backtrack early
    ;     (let ((idx (find-backtrack-point (reverse sequence))))
    ;       (if (= idx n)
    ;         nil
    ;         (dfs n (copy-and-reverse-at-idx sequence (- n idx)) (- n idx) delta)
    ;       )
    ;     )
    ;     ; this could work, so we proceed; unfortunately without a data structure to hold the past clauses, we cannot call dfs with 'reduced' instead of 'delta'
    ;     (dfs n sequence (+ seqlen 1) delta)
    ;   )
    ; )
  )
)

(defun default-sequence (n)
  (if (= n 0)
    nil
    (append (default-sequence (- n 1)) (list n))
  )
)

(defun variable-ordering-sequence (n delta)
  (extract-first (insertion-sort (preprocess-delta (generate-counts n) delta)))
)

(defun sat? (n delta) 
  (dfs n (variable-ordering-sequence n delta) 0 delta)
)
; (dfs 3 (default-sequence 3) 0 '((1 2) (-1 2) (-2)))

; variable ordering attempt

(defun insertion-sort (my-list)
  (if (null my-list)
    '()
    (insert-in-place (car my-list) (insertion-sort (cdr my-list)))))

(defun insert-in-place (e my-list)
  (if (null my-list)
    (cons e '())
    (if (> (second e) (second (car my-list)))
      (cons e my-list)
      (cons (car my-list) (insert-in-place e (cdr my-list))))))

(defun extract-first (my-list)
  (cond
    ((null my-list) nil)
    (t (cons (first (car my-list)) (extract-first (cdr my-list))))
  )
)

(defun increment-count (counts value)
  (cond 
    ((null counts) counts)
    ((= value 0) (cons (list (first (car counts)) (+ (second (car counts)) 1)) (cdr counts)))
    (t (cons (car counts) (increment-count (cdr counts) (- value 1))))
  )
)

(defun preprocess-clause (counts clause)
  (cond
    ((null clause) counts)
    (t (let ((x (increment-count counts (element-abs (car clause)))))
      (preprocess-clause x (cdr clause)))
    )
  )
)

(defun preprocess-delta (counts delta)
  (cond
    ((null delta) counts)
    (t (let ((x (preprocess-clause counts (car delta))))
      (preprocess-delta x (cdr delta)))
    )
  )
)

(defun generate-counts (n)
  (cond
    ((= n 0) nil)
    (t (append (generate-counts (- n 1)) (list (list n 0))))
  )
)

; logic deduction attempt

(defun find-first-single-literal (delta)
  (cond
    ((null delta) nil)
    (t (if (= 1 (length (car delta))) 
      (car delta)
      (find-first-single-literal (cdr delta))))
  )
)

(defun remove-first-single-literal (delta)
  (cond
    ((null delta) nil)
    (t (if (= 1 (length (car delta))) 
      (cdr delta)
      (cons (car delta) (remove-first-single-literal (cdr delta)))))
  )
)

(defun modus-ponens (single-literal literals)
  (if (null literals)
    nil
    (cond
      ((= single-literal (car literals)) t)
      ((= single-literal (- 0 (car literals))) (modus-ponens single-literal (cdr literals)))
      (t (cons (car literals) (modus-ponens single-literal (cdr literals))))
    )
  )
)

(defun reduce-helper (sequence seqlen delta result)
  (if (= seqlen 0)
    ; done with literals we have so far
    delta
    ; apply modus ponen
    (if (null delta)
      (reduce-helper (cdr sequence) (- seqlen 1) result '())
      (let ((res (modus-ponens (car sequence) (car delta))))
        (cond 
          ((null res) nil)
          ((numberp res) (reduce-helper sequence seqlen (cdr delta) (append result (list res))))
          (t (reduce-helper sequence seqlen (cdr delta) result))
        )
      )
    )
))

(defun my-reduce (sequence seqlen delta)
  (let ((res (reduce-helper sequence seqlen delta '())))
    (if (null res)
      nil
      (if (equal res delta)
        ; reached fixed point, we are done
        res
        ; can probably still reduce
        (my-reduce sequence seqlen res)
      )
    )
  )
)

(defun result-reduce (literal delta)
  (let* ((res (my-reduce literal 1 delta)) (start (find-first-single-literal res)))
    (if (null start)
      res
      (result-reduce start (remove-first-single-literal res))
    )
  )
)

(defun reduce-top (sequence seqlen delta)
  (let* ((res (my-reduce sequence seqlen delta)) (start (find-first-single-literal res)))
    (if (null start)
      res
      (result-reduce start res)
    )
  )
)

; (reduce-helper '(1) 1 '((-1 3)) '())
; (my-reduce '(1 2 3) 2 '((-1 3) (-3 2) (-1 -2 -3)))
; (reduce-top '(1 2 3) 2 '((-1 3) (-3 2) (-1 -2 -3)))

; (load "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/hw4.lsp")
; (solve-cnf "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/cnfs/sat/cnf_10.cnf")
; (solve-cnf "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/cnfs/unsat/cnf_20.cnf")


