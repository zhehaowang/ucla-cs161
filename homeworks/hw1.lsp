; TODO: for tree problems, check for usage of atom vs numberp; also, NULL check for problems

; Problem 1

; TREE-CONTAINS checks if number N appears in ordered-tree TREE
; param N number
; param TREE list (ordered-tree)
; return T or NIL
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

; TREE-MAX returns the maximum number from given ordered-tree
; param TREE list (ordered-tree)
; return number
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

; TREE-ORDER returns the in-order traversal of given ordered-tree
; param TREE list (ordered-tree)
; return list
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
; spec: what if start + len > len(l)? Say, (SUB-LIST '(a b c d) 3 3) => (d) is expected?

; SUB-LIST returns x elements starting from index i in list l, where x, i and l are given.
; param L list
; param START number
; param LEN number
; return list
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
; Make sure (SPLIT-LIST '(1)) => (() (1))? and (SPLIT-LIST ()) => (() ())?

; SPLIT-LIST returns the list L split into two halves, first half has the same amount of elements as, or one less element than second half.
; param L list (simple-list)
; return list (a list of two lists)
(defun SPLIT-LIST (L)
  (let ((size (length L)))
    (if (evenp size) 
      ; call sub-list function for split-list, differentiate len(l) even/odd case
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

; BTREE-HEIGHT returns the height of the given B-TREE
; param TREE list (binary tree)
; return number
(defun BTREE-HEIGHT (TREE)
  (cond
    ; base case: tree has only root, height 0
    ((atom TREE) 0)
    ; induction case: get the height of right / left tree, return the larger one + 1
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

; LIST2BTREE returns a binary tree converted from given list
; param TREE list (simple list)
; return list (binary tree)
(defun LIST2BTREE (LEAVES)
  (let ((l (length LEAVES)))
    (cond 
      ; base cases
      ((= l 0) NIL)
      ((= l 1) (car LEAVES))
      ((= l 2) LEAVES)
      ; call split-list in recursive case to 
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

; BTREE2LIST returns a list converted from given binary tree
; param TREE list (binary tree)
; return list (simple list)
(defun BTREE2LIST (TREE)
  (if (null TREE) NIL
    (if (listp TREE)
      ; bracket removal by recursively looking at two subtrees
      (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE)))
      ; if it's a single element return as list so that append does not complain
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

; IS-SAME checks if two params are the same without calling equals
; param E1 list or atom
; param E2 list of atom
; return T or NIL
(defun IS-SAME (E1 E2)
  (if (null E1) 
    ; if both are NIL, return T
    (if (null E2) T NIL)
    (if (atom E1)
      (if (null E2) 
        ; if E1 is atom, and E2 is NIL, return NIL
        NIL
        ; if E1 is atom and E2 is atom, compare the two with =
        (if (atom E2)
          (if (= E1 E2) T NIL)
          NIL)
      )
      ; if E1's a non-empty list, compare its first element with E2's first element
      ; E2 could be NIL here, but won't affect the result of the test
      (if (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2)) NIL)
    )
  )
)

; (IS-SAME NIL NIL)
; (IS-SAME NIL '(1 2))
; (IS-SAME 1 NIL) ; 1 is not an expr, so this shouldn't be used to test
; (IS-SAME '(1) NIL)
; (IS-SAME '(1 2) NIL)
; (IS-SAME '(1 2) '(1 3))
; (IS-SAME '(1 2) '((1) 2))
; (IS-SAME '(1 (2 (4))) '(2 1 (4)))
; (IS-SAME '((1 2 3) 8 8) '((1 2 3) 7 8))
; (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8))