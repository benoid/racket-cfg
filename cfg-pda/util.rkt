#lang racket

(provide (all-defined-out))

(define (attach-tag tag item)
  (cons tag item))
(define (get-tag item)
  (car item))
(define (get-item item)
  (cdr item))
