#lang racket
(require "util.rkt")

(provide (all-defined-out))

(define (produce str var rule)
  (cond ((eq? str '()) '())
        ((eq? (car str) var) 
         (produce (append rule (cdr str)) var rule))
        (else
          (cons (car str) (produce (cdr str) var rule)))))


;;;;;;;;;;;;;;; Production Rule ;;;;;;;;;;;;

(define (make-production-rule lhs rhs)
  (attach-tag
    'ProductionRule
    (list lhs rhs)))
(define (production-rule? item)
  (eq? (get-tag item) 'ProductionRule))

(define (production-rule->lhs item)
  (car (get-item item)))

(define (production-rule->rhs item) 
  (cadr (get-item item)))

(define-syntax prod-rule
  (syntax-rules (=>)
    [(prod-rule a => b) (make-production-rule a b)]))


;;;;;;;;;;;;;;; Context Free Grammar ;;;;;;;;;
;
(define (make-cfg non-terms terms rule-list start-var)
  (attach-tag 
    'ContextFreeGrammar
    (list
      non-terms
      terms
      rule-list
      start-var)))

(define (cfg->non-terms cfg)
  (first (get-item cfg)))

(define (cfg->terms cfg)
  (second (get-item cfg)))

(define (cfg->rule-list cfg)
  (third (get-item cfg)))

(define (cfg->start-var cfg)
  (fourth (get-item cfg)))


;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;

(define str1 (list 1 'S 4 5 6))
(define rul (list 2 3))
(define str2 (produce str1 'S rul))

(define rule1 (prod-rule 'S => (list 2 3)))


