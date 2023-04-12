#lang racket
(provide total-rect-area)



; total-rect-area: returns the total integer area covered by any (one or more) rectangles in the given list 

(define (total-rect-area rect-list)
	(rect-union (make-rect-lst rect-list))


)



;(rect x0 y0 x1 y1)
; (cit xmid i-mid-left i-mid-right left-subtree right-subtree)

;xmid: median of set of interval end points
;i-mid: set of interval where interval-start <= xmid and interval-end >= xmid
;i-mid-left: set of i-mid sort by interval-start/x0
;i-mid-right : set of i-mid sort by interval-end/x1
;i-left: set of all interval where interval-end < xmid
;i-right: set of all interval where interal start >xmid 

(define (rect x0 y0 x1 y1)
            (if (< x0 x1)
                    (begin
                            (let ([temp '(rect x0 x1)]) temp)
                            (if (< y0 y1)
                                    (let ([temp (cons 'rect (cons x0 (cons y0  (cons x1 ( cons y1 null) ))))]) temp)

                                    (let ([temp (cons 'rect (cons x0 (cons y1  (cons x1 ( cons y0 null) ))))]) temp )


                            )
                    )

                    (begin
                            (let ([temp '(rect x1 x0)]) temp)
                            (if (< y0 y1)

      (let ([temp (cons 'rect (cons x1 (cons y0  (cons x0 ( cons y1 null) ))))]) temp )

      (let ([temp (cons 'rect (cons x1 (cons y1  (cons x0 ( cons y0 null) ))))]) temp )

                             )
                     )

            )


     )


; Predicate defining a rectangle
(define (rect? r)
  (match r
         [`(rect ,x0 ,y0 ,x1 ,y1)
          (and (andmap integer? `(,x0 ,x1 ,y0 ,y1))
               (<= x0 x1)
               (<= y0 y1))]
         [else #f]))



; find rect area
(define (rect-area rect)


	(let ([ area
		(*  
			(-  (car(cdr (cdr (cdr rect)))) (car(cdr rect)) )
			(- (car(cdr (cdr (cdr (cdr rect))))) (car(cdr (cdr rect))))
		)
		
		])
		area
	)
	
  )




(define (find-length-list lst)

	(if (empty? lst)
		0
		(add1 (find-length-list (cdr lst)))
	)


)



(define (find-median lst-length)

	(if (eq? 0 (modulo lst-length 2)  )
		(sub1 (/ lst-length 2))
		(sub1 (ceiling(/ lst-length 2)))
	)
)

; need extra test for null!!!!!!
;find xmid which is median of x1 of a rect list sorted by x1
(define (find-x-mid sort-rect-list)
	(if (null? sort-rect-list)
		0
		(fourth
			(list-ref sort-rect-list
				(find-median (find-length-list sort-rect-list))
			)
		)
	)

)



; need extra test for null!!!!!!
;find imid which is a set of rect where x0<=xmid and x1>= xmid
(define (find-i-mid sort-rect-list xmid)
	(if (null? sort-rect-list )
		empty
		(for/list ([i sort-rect-list]
			#:when (and (<= (second i) xmid) (>= (fourth i) xmid)) )
			i
		)
	)
)

;set of i-mid which sort by x0
(define (find-i-mid-left imid)
	(sort imid < #:key second )

)

;set of i-mid which sort by x1
(define (find-i-mid-right imid)
	(sort imid < #:key fourth)

)


;set of ileft which x1 <xmid
;i-left: set of all interval where interval-end/x1 < xmid
;rect-lst sort by fourth before pass in and add break statement
(define (find-i-left sort-rect-lst xmid)

	(define (i-left sort-rect-lst xmid)
		(for/list ([i sort-rect-lst]
			#:when (< (fourth i) xmid))
			i
		)
	)
	(sort (i-left sort-rect-lst xmid) < #:key fourth)

)



;set of ileft which x0 >xmid
;i-left: set of all interval where interval-end/x0 > xmid
;rect-lst sort by fourth before pass in and add break statement
;need to sort or not????
(define (find-i-right sort-rect-lst xmid)

	(define (i-right sort-rect-lst xmid)
		(for/list ([i sort-rect-lst]
			#:when (> (second i) xmid))
			i
		)
	)
	(sort (i-right sort-rect-lst xmid) < #:key fourth)

)


;cit tree format '(cit xmid i-mid-left i-mid-right left-sub-tree right-sub-tree)
;build center interval tree
;time complexity O(nlogn) (in theory)
(define (build-cit rect-lst)

	(define xmid (find-x-mid (sort rect-lst < #:key fourth)) )
	(define imid (find-i-mid rect-lst xmid) )
	(if (null? rect-lst)
		`(cit 0 ,empty ,empty ,empty ,empty)
		`(cit ,xmid
			,(find-i-mid-left imid)
		 	,(find-i-mid-right imid)
		 	,(build-cit (find-i-left rect-lst xmid)   )
			,(build-cit (find-i-right rect-lst xmid)  )
		  )


	)
)

;check if a rect x interval contain query-point-x qx, return true if yes, false otherwise
(define (check-contain-qx qx rectx)
	(cond 
		[(null? rectx) -99]
		[(and (>= qx  (second rectx)) (< qx (fourth rectx)) ) #t ]
		
		[else -99]
	)	

)

;walk along i-mid-left or i-mid-right report all rect that include query-point-x qx
;break when rect does not contain query-point-x qx

(define (walk-along-imid-left-or-right qx imid-left-or-right)
	(if (null? imid-left-or-right)
		'()
		(for/list ([i imid-left-or-right]
				#:when (and (eq? (check-contain-qx qx i) #t) (> (rect-area i) 0) )
				#:break (eq? (check-contain-qx qx i) -99))
	
				`(,(third i) ,(fifth i))
		)
	
	)


)





;cit tree format '(cit xmid i-mid-left i-mid-right left-sub-tree right-sub-tree)
;return a list of rect that contain the query-point-x qx
(define (find-all-rect-intersect-qx qx cit total-lst )
	(if (and (null? (fifth cit)) (null? (sixth cit)) )
		'()
		(cond
			[(< qx (second cit)) (append  (walk-along-imid-left-or-right qx (third cit))    (find-all-rect-intersect-qx qx (fifth cit) total-lst ) )    ]
			[(> qx (second cit)) (append  (walk-along-imid-left-or-right qx (fourth cit))    (find-all-rect-intersect-qx qx (sixth cit) total-lst ) )    ]
			[(eq? qx (second cit)) (append  (walk-along-imid-left-or-right qx (fourth cit))    (find-all-rect-intersect-qx qx (sixth cit) total-lst ) )    ]
		)



	)

)



; find line union
(define (line-coverage-0 lines-lst)
  (define (acc-total-from lst total from-x)
    (if (null? lst)
        total
        (acc-total-from (cdr lst)
                        (+ total
                           (- (max from-x (second (first lst)))
                              (max from-x (first (first lst)))))
                        (max from-x (second (first lst))))))
  (acc-total-from (sort lines-lst < #:key first) 0 0))


; find max x from rect list 
(define  (rect-list-max-x rect-list)

	(fourth (first (sort rect-list > #:key fourth)) )

)

; find min x from rect list
(define  (rect-list-min-x rect-list)

	(second (first (sort rect-list < #:key second)) )

)






(define (area-total min-x max-x cur-x cit total)


	
	(if (eq?  max-x cur-x)
		(+ total  
												(line-coverage-0 
													(find-all-rect-intersect-qx cur-x cit '() )
												)
												 
										)
										

	
			(area-total min-x max-x (add1 cur-x) cit (+ total  
												(line-coverage-0 
													(find-all-rect-intersect-qx cur-x cit '() )
												)
												 
										)
			)
			
		

	)


)




;find total area from line union
(define (rect-union rect-lst)

		(if (empty? rect-lst)
	
		0
		(area-total  (rect-list-min-x rect-lst) (rect-list-max-x rect-lst)   (rect-list-min-x rect-lst) (build-cit (sort (set->list(list->set rect-lst)) < #:key fourth)) 0)
	)



)


;convert non standard to standard rect format
(define (make-rect-lst rect-lst)

	(for/list ([i rect-lst]
			#:when(and (> (rect-area i) 0)    (and (<= (second i) (fourth i) ) (<= (third i) (fifth i) )  )) )
		i
	)


)
