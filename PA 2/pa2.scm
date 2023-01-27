; Scheme Programming Assignment #2

; define sqr-list
; (sqr-list lst) => squares each element of the list

; helper func sqr
(define sqr
    (lambda(x) (* x x)
    )
)

(define (sqr-list lst)
    (let iter ((lst lst) (res '() ))
        (if (null? lst)
            (reverse res)
            
            ; else: square the TOP elem + recursively runs the func on remaining items
            (
                iter (cdr lst) (cons (sqr (car lst)) res)
            )
        )
    )
)

; (Not for submission) Jim said no high-order functions
(define sqr-list-1
    (lambda (lst)
        (map sqr lst)
    )
)


; define place: 
; (place x lst) => inserts x into ordered list lst

; place may be too hard; I'll get you started with one possible answer (it uses a helper function):
; (define place
;   (lambda (x lst)
;     (if (null? lst)
;       (list x)
;     ;else
;       (place2 x (car lst) (cdr lst)))))  

; (define place2
;   (lambda (x carlst cdrlst)
;     ; <= top elem of list -> places X at front of list
;     (if (<= x carlst)
;         (cons x (cons carlst cdrlst))

        
;     )
;   )
; )

(define (place x lst)
    (cond
        ; dealing w cases adding x to the end of list: e.g (place 7 '(1 2 3 5))
        ((null? lst) (cons x lst) )
        ; <= top elem of list -> places X at front
        ((<= x (car lst)) (cons x lst))
        ; else -> keep recur on remaining items
        (else 
            (cons (car lst) (place x (cdr lst)))
        )
    )
)

; (change x) => if x is negative it returns x^2, if 0 it returns 0, if positive it returns x + 1.
(define (change x)
    (cond
        ; negative -> x^2
        ((negative? x) (* x x))
        ; positive -> x + 1
        ((positive? x) (+ x 1))
        ; other type of elem/ elem == 0 -> return x
        (else (x))
    )
)

; (change-list lst) applies change to each element in the list
(define (change-list lst)
    cons ( (change-list cdr(lst)))
)

; (closest-point point lst) => returns the point in the list of points that 
; is closest to the input point.

; helper functions from pa1 + sqr function from above
(define (x-coord point)
    (car point)
)

(define (y-coord point)
    (cadr point)
)

(define (dist_pt p1 p2)
    (sqrt 
        (+
            (sqr (- (x-coord p2) (x-coord p1))) (sqr (- (y-coord p2) (y-coord p1)))
        )
    ) 
)

(define (closest-point point lst)
    ; an arbitrary large num
    (let min-dist ((lst lst) (cur-min-dist 1000000000 ) (res (car lst)) (cur-dist (dist_pt point (car lst))))
        (if (null? lst)
            res
            
            ; found new MIN DISTANCE
            (if (= (min cur-min-dist cur-dist) cur-dist)
                (min-dist (cdr lst) cur-dist (car lst) (dist_pt point (car lst)))

                    ; same MIN DISTANCE as prev iteration
                    (min-dist (cdr lst) cur-min-dist res (dist_pt point (car lst))) 
            )
        )
    )
)

; define add-list:
; (add-list lst1 lst2) => adds each element of the 2 lists
; example:
; (add-list '(1 2 3) '(2 1 4)) => (3 3 7)
(define (add-list lst1 lst2)
    ; both ls1 and lst2 should have the same len
    (if (not (= (length lst1) (length lst2))) "Invalid params" 
        (let iter ((lst1 lst1) (lst2 lst2) (res '() ))
            (if (or (null? lst1) (null? lst2))
                (reverse res)
                
                ; else: square the TOP elem + recursively runs the func on remaining items
                (
                    iter (cdr lst1) (cdr lst2) (cons (+ (car lst1) (car lst2)) res)
                )
            )
        )
    )
)

; define delete-lists:
; (delete-lists lst) => deletes the sub-lists from this list.
; example:
; (delete-lists '(1 2 (3 4) (5 (6 7)) 8 (9)))
; returns:
; (1 2 8)
(define (delete-lists lst)
    (cond
        ((null? lst) lst)
        ; list detected -> skips the list
        ((list? (car lst))  (delete-lists (cdr lst)))
        ; not list? add current top elem back to the list
        (else (cons (car lst) (delete-lists (cdr lst))))
    )
)

; define flatten:
; (flatten lst) => removes sub-list structure but keeps the list elements
; (flatten '(1 2 (3 (4 5) 6))) returns (1 2 3 4 5 6)
(define (flatten lst)
    (cond
        ((null? lst) lst)
        ; list detected -> recursively unpack that list -> eventually merge w main list
        ((list? (car lst)) (append (flatten (car lst)) (cdr lst)))
        ; not list? add current top elem back to the list
        (else (cons (car lst) (flatten (cdr lst))))
    )    
)

; Run 20 commands from Chapter 6 that you think would be particularly
; advantageous.  Make sure you experiment with vectors and their conversions
; to and from lists.


; (display (sqr-list (list 1 2 3 4)))
; (newline)

; (display (sqr-list-1 (list 1 2 3 4)))
; (newline)

; (display (change -2))
; (newline)

; (display(add-list '(1 2 3 4) '(1 2 3 4)))
; (newline)

; (display(place 7 '(1 3 4 5 6)))
; (newline)

; (display(flatten '(1 2 (3 (4 5) 6))))
; (newline)

(display(delete-lists '(1 2 (3 4) (5 (6 7)) 8 (9))))
(newline)

(display(closest-point '(0 28) '((26 63) (23 63) (22 63)) ))
(newline)
