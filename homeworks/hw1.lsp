; Problem 1

(defun TREE-CONTAINS (N TREE)
  (cond
    ; base case: tree has only root, if root == N, T else nil
    ((atom TREE) (if (= N TREE) T NIL))
    ; induction case: check (= > <) root, return true, go to left subtree, or right subtree correspondingly
    ((= N (second TREE)) T)
    ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
    ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))
  )
)

; Test cases
; (TREE-CONTAINS 3 3)
; (TREE-CONTAINS 4 '(1 2 4))
; (TREE-CONTAINS 3 '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))
; (TREE-CONTAINS 7 '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))

; Problem 2

(defun TREE-MAX (TREE)
  (cond
    ; base case: tree has only root, return it
    ((atom TREE) TREE)
    ; induction case: recursion on the right subtree
    (T (TREE-MAX (third TREE)))
  )
)

; (TREE-MAX '((1 2 3) 7 8))

; Problem 3

(defun TREE-ORDER (TREE)
  (cond
    ; base case: tree has only root, return it
    ((atom TREE) (list TREE))
    ; induction case: recursion on left and right subtree, piece them together with cons and append
    (T (append (TREE-ORDER (first TREE)) (cons (second TREE) (TREE-ORDER (third TREE)))))
  )
)

; (TREE-ORDER 3)
; (TREE-ORDER '((1 2 3) 7 8))
; (TREE-ORDER '((1 2 3) 5 (6 8 (9 10 (11 12 13)))))

; Problem 4
; spec: what if start + len > len(l)? Say (SUB-LIST '(a b c d) 3 3) => (d) is expected?

(defun SUB-LIST (L START LEN)
  (cond
    ; len = 0 case
    ((= LEN 0) NIL)
    ; L = nil case
    ((null L) NIL)
    ; start = 0 case: can start now
    ((= START 0) (cons (car L) (SUB-LIST (cdr L) 0 (- LEN 1))))
    ; start > 0 case: can't start yet, seek till start
    ((> START 0) (SUB-LIST (cdr L) (- START 1) LEN))
  )
)

; (SUB-LIST '(a b c d) 0 3)
; (SUB-LIST '(a b c d) 3 1)
; (SUB-LIST '(a b c d) 2 0)
; (SUB-LIST '(a b c d) 6 0)
; (SUB-LIST '(a b c d) 3 3)

; Problem 5
; just to make sure (SPLIT-LIST '(1)) => (() (1))? and (SPLIT-LIST ()) => (() ())?

(defun SPLIT-LIST (L)
  (let ((size (length L)))
    (if (evenp size) 
      (list (SUB-LIST L 0 (/ size 2)) (SUB-LIST L (/ size 2) (/ size 2)))
      (list (SUB-LIST L 0 (/ (- size 1) 2)) (SUB-LIST L (/ (- size 1) 2) (/ (+ size 1) 2)))
  ))
)

; (SPLIT-LIST '(a b c d))
; (SPLIT-LIST '(a b c d e))
; (SPLIT-LIST '(a b c d e f))
; (SPLIT-LIST '(a))
; (SPLIT-LIST NIL)

; Problem 6

(defun BTREE-HEIGHT (TREE)
  (cond
    ((atom TREE) 0)
    (t (let ((left (BTREE-HEIGHT (first TREE))) (right (BTREE-HEIGHT (second TREE))))
      (if (> left right) (+ left 1) (+ right 1)))
    )
  )
)

; (BTREE-HEIGHT 1)
; (BTREE-HEIGHT '(1 2))
; (BTREE-HEIGHT '(1 (2 3)))
; (BTREE-HEIGHT '((1 2) (3 4)))
; (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7))))
; (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8))))

; Problem 7

(defun LIST2BTREE (LEAVES)
  (let ((l (length LEAVES)))
    (cond 
      ((= l 0) NIL)
      ((= l 1) (car LEAVES))
      ((= l 2) LEAVES)
      (t (let ((res (SPLIT-LIST LEAVES)))
        (list (LIST2BTREE (first res)) (LIST2BTREE (second res)))
      ))
    )
  )
)

; (LIST2BTREE '())
; (LIST2BTREE '(1))
; (LIST2BTREE '(1 2))
; (LIST2BTREE '(1 2 3))
; (LIST2BTREE '(1 2 3 4))
; (LIST2BTREE '(1 2 3 4 5 6 7))
; (LIST2BTREE '(1 2 3 4 5 6 7 8))

; Problem 8

(defun BTREE2LIST (TREE)
  (if (null TREE) NIL
    (if (listp TREE)
      (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE)))
      (list TREE)
    )
  )
)

; (BTREE2LIST NIL)
; (BTREE2LIST 1) 
; (BTREE2LIST '(1 2)) 
; (BTREE2LIST '(1 (2 3))) 
; (BTREE2LIST '((1 2) (3 4))) 
; (BTREE2LIST '((1 (2 3)) ((4 5) (6 7)))) 
; (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8))))

; Problem 9; TODO: more test cases

(defun IS-SAME (E1 E2)
  (if (null E1) 
    (if (null E2) T NIL)
    (if (atom E1)
      (if (null E2) 
        NIL
        (if (atom E2)
          (if (= E1 E2) T NIL)
          NIL)
      )
      (IS-SAME (car E1) (car E2))
    )
  )
)

; (IS-SAME NIL NIL)
; (IS-SAME NIL '(1 2))
; (IS-SAME 1 NIL) : 1 is not an expr, so this shouldn't be used to test
; (IS-SAME '(1 2) NIL)
; (IS-SAME '(1 2) '((1) 2))
; (IS-SAME '(1 (2 (4))) '(2 1 (4)))
; (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8))
; (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8))