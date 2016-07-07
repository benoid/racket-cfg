#lang racket


;;;;;;;;;;; Earley Item ;;;;;;;;;;;

(define (make-earley-item 
         lhs-non-terminal 
         symbols-read
         symbols-unread
         start-index)
  (attach-tag 
    'EarleyItem
    (list 
      left-nt
      symbols-read
      symbols-unread
      start-index)))

(define (earley-item->lhs-non-term item)
  (first (get-item item)))

(define (earley-item->symbols-read item)
  (second (get-item item)))

(define (earley-item->symbols-unread item)
  (third (get-item item)))

(define (earley-item->start-index item)
  (fourth (get-item item)))

;;;;;;;;;;;;;;   State Set ;;;;;;;;;;;


