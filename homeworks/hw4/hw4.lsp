; Homework 4
; Zhehao Wang, 404380075, zhehao@cs.ucla.edu

; A backtracking DFS SAT solver with variable ordering and forward checking with modus-ponens
; On a Macbook Pro running Clozure CL, this solves sat_50 in matter of seconds, and takes a few minutes to solve unsat_42
; haven't tried with new test cases

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; hw4 code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; helper function for printing cnf
(defun print-cnf (filename)
  (let ((cnf (parse-cnf filename))) (second cnf))
)

; check if value or its complement is covered by an assignment (list of vars)
(defun in-assign (assign value)
  (cond
    ((null assign) nil)
    ((= (car assign) value) t)
    ((= (+ (car assign) value) 0) nil)
    (t (in-assign (cdr assign) value))
  )
)

; check if a clause if satisfied by an assignment (list of vars)
(defun check-clause (assign clause)
  (if (null clause)
    nil
    (if (in-assign assign (car clause))
      t
      (check-clause assign (cdr clause))
    )
  )
)

; check all clauses if satisfied by an assignment
(defun check-clauses (assign delta)
  (if (null delta)
    t
    (and (check-clause assign (car delta)) (check-clauses assign (cdr delta)))
  )
)

; absolute function
(defun element-abs (value)
  (if (< value 0)
    (- 0 value)
    value
  )
)

; take the absolute of all elements of an array
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

(defun copy-n (array n)
  (cond
    ((null array) nil)
    ((= n 0) nil)
    (t (cons (car array) (copy-n (cdr array) (- n 1))))
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
  (let* 
    ((clauses (get-last delta)) 
     (reduced (my-reduce sequence seqlen clauses)))

      (if (null reduced)
        ; forward checking tells us this definitely won't work, so we backtrack early
        (let ((idx (find-backtrack-point (reverse (copy-n sequence seqlen)))))
          (if (= idx seqlen)
            ; we can't backtrack any more, return nil
            nil
            ; we can backtrack, thus we pop the useless reduced clauses and backtrack
            (dfs n (copy-and-reverse-at-idx sequence (- seqlen idx)) (- seqlen idx) (pop-n delta idx))
          )
        )
        ; forward checking thinks this could work, so we proceed;
        (if (equal seqlen n) 
          (if (check-clauses sequence reduced)
            ; the current sequence works, we return
            sequence
            ; the current sequence does not work, we backtrack; ideally this shouldn't be executed
            (let ((idx (find-backtrack-point (reverse sequence))))
              (if (= idx n)
                nil
                (dfs n (copy-and-reverse-at-idx sequence (- n idx)) (- n idx) (pop-n delta idx))
              )
            )
          )
          (dfs n sequence (+ seqlen 1) (append delta (list reduced)))
        )
      )
))

; default sequence is (1 2 3 4...)
(defun default-sequence (n)
  (if (= n 0)
    nil
    (append (default-sequence (- n 1)) (list n))
  )
)

; this orders the variables according to how many times it appears in all clauses
(defun variable-ordering-sequence (n delta)
  (extract-first (insertion-sort (preprocess-delta (generate-counts n) delta)))
)

; top-level function for sat
(defun sat? (n delta) 
  (dfs n (variable-ordering-sequence n delta) 0 (list delta))
)

; (dfs 3 (default-sequence 3) 0 '((1 2) (-1 2) (-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; variable ordering related functions
; this orders the variable by the number of occurrences in all clauses
; the intuition's that the more a literal appears, the more likely its (early) assignment will be constraining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; insertion sort
(defun insertion-sort (my-list)
  (if (null my-list)
    '()
    (insert-in-place (car my-list) (insertion-sort (cdr my-list)))))

; insertion sort helper
(defun insert-in-place (e my-list)
  (if (null my-list)
    (cons e '())
    (if (> (second e) (second (car my-list)))
      (cons e my-list)
      (cons (car my-list) (insert-in-place e (cdr my-list))))))


; extract the first element from a list of lists; similar with Python dict.keys()
(defun extract-first (my-list)
  (cond
    ((null my-list) nil)
    (t (cons (first (car my-list)) (extract-first (cdr my-list))))
  )
)

; increment the count given a certain key
; count is a list of [(key, count)], where key is the element
(defun increment-count (counts value)
  (cond 
    ((null counts) counts)
    ((= value 1) (cons (list (first (car counts)) (+ (second (car counts)) 1)) (cdr counts)))
    (t (cons (car counts) (increment-count (cdr counts) (- value 1))))
  )
)

; count the occurrences of all literals in one clause, returns the [(key, count)] structure
(defun preprocess-clause (counts clause)
  (cond
    ((null clause) counts)
    (t (let ((x (increment-count counts (element-abs (car clause)))))
      (preprocess-clause x (cdr clause)))
    )
  )
)

; count the occurrences of all literals of all terms, returns the [(key, count)] structure
(defun preprocess-delta (counts delta)
  (cond
    ((null delta) counts)
    (t (let ((x (preprocess-clause counts (car delta))))
      (preprocess-delta x (cdr delta)))
    )
  )
)

; generate the [(1 0)(2 0)...] structure
(defun generate-counts (n)
  (cond
    ((= n 0) nil)
    (t (append (generate-counts (- n 1)) (list (list n 0))))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; stack
; pop and get element from a stack implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-pop (my-list)
  (cond
    ((null my-list) nil)
    ((null (cdr my-list)) nil)
    (t (cons (car my-list) (my-pop (cdr my-list))))
  )
)

(defun pop-n (my-list n)
  (cond
    ((= n 0) my-list)
    (t (pop-n (my-pop my-list) (- n 1)))
  )
)

(defun get-last (my-list)
  (cond
    ((null my-list) nil)
    ((null (cdr my-list)) (car my-list))
    (t (get-last (cdr my-list)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; deduction using modus-ponens logic
; this only tries the assigned variables with the whole set of clauses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; returns the first single-literal clause in delta
(defun find-first-single-literal (delta)
  (cond
    ((null delta) nil)
    (t (if (= 1 (length (car delta))) 
      (car delta)
      (find-first-single-literal (cdr delta))))
  )
)

; remove the first single-literal clause in delta
(defun remove-first-single-literal (delta)
  (cond
    ((null delta) nil)
    (t (if (= 1 (length (car delta))) 
      (cdr delta)
      (cons (car delta) (remove-first-single-literal (cdr delta)))))
  )
)

; apply a single modus-ponens
(defun modus-ponens (single-literal literals)
  (if (null literals)
    nil
    (cond
      ; ((= single-literal (car literals)) t)
      ((= single-literal (- 0 (car literals))) (modus-ponens single-literal (cdr literals)))
      (t (cons (car literals) (modus-ponens single-literal (cdr literals))))
    )
  )
)

; helper function for reducing all clauses given first seqlen in sequence literals (assignments) 
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
          (t (reduce-helper sequence seqlen (cdr delta) (append result (list res))))
          ; (t (reduce-helper sequence seqlen (cdr delta) result))
        )
      )
    )
))

; top-level function for reducing all clauses given first seqlen in sequence literals (assignments) 
; tells that we reach the end by checking if we end up on a fixed point after applying reduce-helper
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; provided code: CNF file parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defun split-line (line)
;   (if (equal line :eof)
;       :eof
;       (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

; (defun read-cnf (filename)
;   (with-open-file (in filename)
;     (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
;       if (equal 'p (first line)) collect (third line)      ; var count
;       if (integerp (first line)) collect (butlast line)))) ; clause

; (defun parse-cnf (filename)
;   (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; ; Following is a helper function that combines parse-cnf and sat?
; (defun solve-cnf (filename)
;   (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

;;;;;;;;;;;;;;;;;;;;;;;;
; simple test cases and unused code
;;;;;;;;;;;;;;;;;;;;;;;;

; this does further reduction 
; (applies modus-ponens until unapplicable; cannot differentiate nil or t for now so commented)

; (defun result-reduce (literal delta)
;   (let* ((res (my-reduce literal 1 delta)) (start (find-first-single-literal res)))
;     (if (null start)
;       res
;       (result-reduce start (remove-first-single-literal res))
;     )
;   )
; )

; (defun reduce-top (sequence seqlen delta)
;   (let* ((res (my-reduce sequence seqlen delta)) (start (find-first-single-literal res)))
;     (if (null start)
;       res
;       (result-reduce start res)
;     )
;   )
; )

; (reduce-helper '(1) 1 '((-1 3)) '())
; (my-reduce '(1 2 3) 2 '((-1 3) (-3 2) (-1 -2 -3)))
; (reduce-top '(1 2 3) 2 '((-1 3) (-3 2) (-1 -2 -3)))

; (load "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/hw4.lsp")
; (solve-cnf "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/cnfs/sat/cnf_10.cnf")
; (solve-cnf "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/cnfs/unsat/cnf_20.cnf")


; (load "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/hw4.lsp")
; (print-cnf "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/cnfs/unsat/cnf_12.cnf")
; (solve-cnf "/Users/zhehaowang/projects/ucla-cs-2015/cs-161/homeworks/hw4/cnfs/unsat/cnf_12.cnf")

; unsat_12
; (preprocess-delta (generate-counts 10) '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (-1 -4) (-1 -7) (-1 -10) (-4 -7) (-4 -10) (-7 -10) (-2 -5) (-2 -8) (-2 -11) (-5 -8) (-5 -11) (-8 -11) (-3 -6) (-3 -9) (-3 -12) (-6 -9) (-6 -12) (-9 -12)))
; (variable-ordering-sequence 10 '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (-1 -4) (-1 -7) (-1 -10) (-4 -7) (-4 -10) (-7 -10) (-2 -5) (-2 -8) (-2 -11) (-5 -8) (-5 -11) (-8 -11) (-3 -6) (-3 -9) (-3 -12) (-6 -9) (-6 -12) (-9 -12)))

; (dfs 10 '(10 9 8 7 6 5 4 3 2 1) 0 '(((1 2 3) (4 5 6) (7 8 9) (10 11 12) (-1 -4) (-1 -7) (-1 -10) (-4 -7) (-4 -10) (-7 -10) (-2 -5) (-2 -8) (-2 -11) (-5 -8) (-5 -11) (-8 -11) (-3 -6) (-3 -9) (-3 -12) (-6 -9) (-6 -12) (-9 -12))))
; (my-reduce '(10 9 8 7 6 5 4 3 2 1) 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12) (-1 -4) (-1 -7) (-1 -10) (-4 -7) (-4 -10) (-7 -10) (-2 -5) (-2 -8) (-2 -11) (-5 -8) (-5 -11) (-8 -11) (-3 -6) (-3 -9) (-3 -12) (-6 -9) (-6 -12) (-9 -12)))
; (dfs 3 '(3 2 1) 0 '(((-1) (1) (3))))

; sat_10
; (preprocess-delta (generate-counts 10) '((1 2 -7) (1 6 9) (-8 -9 -10) (-4 -8 -9) (-1 2 3) (3 -7 -10) (-1 3 -5) (-6 9 10) (-5 7 8) (-4 5 -10) (-2 4 9) (3 7 9) (3 4 6) (-1 7 -8) (-1 -2 3) (6 9 10) (-1 -9 10) (-1 8 -10) (2 -7 -8) (-4 -7 -10)))
; (variable-ordering-sequence 10 '((1 2 -7) (1 6 9) (-8 -9 -10) (-4 -8 -9) (-1 2 3) (3 -7 -10) (-1 3 -5) (-6 9 10) (-5 7 8) (-4 5 -10) (-2 4 9) (3 7 9) (3 4 6) (-1 7 -8) (-1 -2 3) (6 9 10) (-1 -9 10) (-1 8 -10) (2 -7 -8) (-4 -7 -10)))
