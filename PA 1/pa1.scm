(define sqr
    (lambda(x) (* x x)
    )
)

(define (double x) 
    (* 2 x)
)

(define (dist x1 y1 x2 y2)
    (sqrt 
        (+(sqr (- x2 x1)) (sqr (- y2 y1)))
    )
)

(define pointa '(1 2))
(define pointb '(3 5))

(define (x-coord point)
    (car point))

(define (y-coord point)
    (car (cdr point)))


(define (dist_pt p1 p2)
    (sqrt 
    (+
        (sqr (- (x-coord p2) (x-coord p1))) (sqr (- (y-coord p2) (y-coord p1)))
    )
    ) 
)

; define find_tf: 
; (find_tf x lst) => finds if x is in the list lst.  it returns #t or #f

(define (find_tf x lst)
    (cond
        ; base case: empty list -> false
        [(null? lst) #f]
        ; top == x => FOUND!
        [(equal? (car lst) x) #t]
        [else (find_tf x (cdr lst))]    
    )
)

; define find_posit: 
; (find_posit x lst) => returns index of x is in the list lst.  it returns -1 if not in list

; Base 0 indexing
(define (find_posit x lst)
    (
        let recur((lst lst) (idx 0))
        (if (null? lst)
            -1
            ; recursive case
            (if (equal? x (car lst))
                ; FOUND 
                idx 
                ; else? increments index
                (recur (cdr lst) (+ idx 1))
            )
        )
    )
)

; define count_all:
; (count_all lst) => returns the number of elements in the list lst
(define (count_all lst)
    (
        let recur((lst lst) (idx 0))
        ; base: reach end of arr / empty arr
        (if (null? lst)
            idx

            (recur (cdr lst) (+ idx 1))
        )
    )
)

; define count_twos
; (conut_twos lst) => returns the number of 2s in the list lst
(define (count_twos lst)
(
    let recur((lst lst) (two_counts 0))
    ; base: reach end of arr / empty arr
    (if (null? lst)
        two_counts
        
        ; top elem == 2
        (if (eq? (car lst) 2)
                (recur (cdr lst) (+ two_counts 1))
            ; else: dont increment count
            (recur (cdr lst) two_counts)
        )
    )
)
)


(display (sqr 5))
(sqr 4)
(newline)
(display (double 2))
(newline)
(display (dist 1 2 3 5))
(newline)

; (define pointa '(1 2))
; (define pointb '(3 5))

(display (dist_pt pointa pointb))
(newline)
(display (dist_pt '(1 2) '(3 5)))
(newline)

; (display (find_tf 5 (list 1 2 3 4)))
; (newline)
; (display (find_posit 5 (list 1 2 3 4 5)))
; (newline)
; (display (count_all (list 1 2 3 4 5)))
; (newline)
; (display (count_twos (list 2 2 2 4 5 2)))
; (newline)