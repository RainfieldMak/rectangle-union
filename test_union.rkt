#lang racket

(require "rectangle-coverage.rkt")

(define rect-lst '((rect 1 1 1 4)
                   (rect 0 0 0 6)
              	 (rect 6 6 2 6)
                   (rect 1 1 1 1)
			 (rect 1 23 12 56)
                   (rect 2 2 2 2)
                   (rect 999 9 999 9)
                   (rect 0 0 0 9999999999)) )


(total-rect-area rect-lst)