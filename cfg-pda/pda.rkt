#lang racket

(require "./util.rkt")

(provide (all-defined-out))

;;;;;;; Transition Rule ;;;;;;;;;;;;

(define (make-transition-rule
          current-state
          next-state
          input-symbol
          pop-symbol
          push-symbol)
  (attach-tag 'TransitionRule
              (list 
                current-state 
                next-state
                input-symbol
                pop-symbol 
                push-symbol)))

(define-syntax trans-rule
  (syntax-rules (:: =>)
    [(trans-rule current-state 
                 :: 
                 input-symbol 
                 :: 
                 pop-symbol 
                 => 
                 push-symbol 
                 :: 
                 next-state)
      (make-transition-rule 
        current-state 
        next-state 
        input-symbol 
        pop-symbol 
        push-symbol)]))

(define (trans-rule->current-state r)
  (first (get-item r)))

(define (trans-rule->next-state r)
  (second (get-item r)))

(define (trans-rule->input-symbol r)
  (third (get-item r)))

(define (trans-rule->pop-symbol r)
  (fourth (get-item r)))

(define (trans-rule->push-symbol r)
  (fifth (get-item r)))

(define (filter-rule-list-by-current-state rule-list state) 
  (filter  (lambda (rule) 
             (eq? (trans-rule->current-state rule)
                  state))
           rule-list))

(define (filter-rule-list-by-next-state rule-list state) 
  (filter  (lambda (rule) 
             (eq? (trans-rule->next-state rule)
                  state))
           rule-list))

(define (filter-rule-list-by-input-symbol rule-list input-symbol) 
  (filter  (lambda (rule) 
             (eq? (trans-rule->input-symbol rule)
                  input-symbol))
           rule-list))

(define (filter-rule-list-by-pop-symbol rule-list pop-symbol) 
  (filter  (lambda (rule) 
             (eq? (trans-rule->pop-symbol rule)
                  pop-symbol))
           rule-list))

(define (filter-rule-list-by-push-symbol rule-list push-symbol) 
  (filter  (lambda (rule) 
             (eq? (trans-rule->push-symbol rule)
                  push-symbol))
           rule-list))

;;;;;;;;; Push Down Automaton ;;;;;;;

(define (build-table-entry rule-list)
  (if (not (pair? rule-list)) 
      '()
      (cons
        (trans-rule->current-state (car rule-list))
        (map (lambda (item) 
               (list (trans-rule->input-symbol item)
                     (trans-rule->pop-symbol item)
                     (trans-rule->push-symbol item)
                     (trans-rule->next-state item))) rule-list))))

(define (table-entry->current-state entry)
  (if (pair? entry)
      (car entry)
      '()))

(define (table-entry->rule-list entry)
  (if (pair? entry)
  (cdr entry)
  '()))

(define (entry-rule->input-symbol rule)
  (first rule))

(define (entry-rule->pop-symbol rule)
  (second rule))

(define (entry-rule->push-symbol rule)
  (third rule))

(define (entry-rule->next-state rule)
  (fourth rule))

;;;;; Match Table Entry ;;;;


(define (get-table-entry
          table
          current-state)
  (filter (lambda (item)
            (eqv? (table-entry->current-state item)
                  current-state)) table))

(define (match-entry-next-state
          table
          current-state
          next-state)
  (filter (lambda (item)
            (eqv? (entry-rule->next-state item)
                  next-state))
          (table-entry->rule-list 
            (car (get-table-entry table current-state)))))

(define (match-entry-input-symbol
          table
          current-state
          sym)
  (filter (lambda (item)
            (eqv? (entry-rule->input-symbol item)
                  sym)) 
          (table-entry->rule-list 
            (car (get-table-entry table current-state)))))

(define (match-entry-pop-symbol
          table
          current-state
          sym)
  (filter (lambda (item)
            (eqv? (entry-rule->pop-symbol item)
                  sym))
          (table-entry->rule-list 
            (car (get-table-entry table current-state)))))

(define (match-entry-push-symbol
          table
          current-state
          sym)
  (filter (lambda (item)
            (eqv? (entry-rule->push-symbol item)
                  sym))
          (table-entry->rule-list 
            (car (get-table-entry table current-state)))))



(define (make-state-table state-set trans-rules)
  (if (or (eq? state-set '()) (eq? trans-rules '()))
      '()
      (cons (build-table-entry
              (filter-rule-list-by-current-state
                trans-rules
                (car state-set)))
            (make-state-table  (cdr state-set) trans-rules))))



(define (make-pda
          state-set       ;; list of states
          input-alphabet  ;; list of input symbols
          stack-alphabet  ;; list of stack symbols
          transition-relations  ;; list of transition relation functions
          stack-init-symbol  ;; the stack init symbol
          accepting-states ;; list of accept states
          stack
          current-state
          )
  (attach-tag 'PushDownAutomaton
              (list 
                state-set
                input-alphabet
                stack-alphabet
                transition-relations
                stack-init-symbol
                accepting-states
                current-state
                stack
                (make-state-table 
                  state-set
                  transition-relations))))
 

(define local-def/Q list)
(define local-def/sigma list)
(define local-def/gamma list)
(define local-def/transitions list)
(define (local-def/q0 x) x)
(define (local-def/q-current x) x)
(define local-def/F list)
(define (local-def/stack-init x) x)
(define local-def/accept-states list)

(define epsilon '())

(define make-stack list)


(define (pda->state-set m)
  (first (get-item m)))

(define (pda->input-alphabet m)
  (second (get-item m)))

(define (pda->stack-alphabet m)
  (third (get-item m)))

(define (pda->rule-list m)
  (fourth (get-item m)))

(define (pda->stack-init-symbol m)
  (fifth(get-item m)))


(define (pda->accepting-states m)
  (sixth (get-item m)))

(define (pda->stack m)
  (seventh (get-item m)))

(define (pda->current-state m)
  (eighth (get-item m)))

(define (pda->stack-push pda item)
  (make-pda
    (pda->state-set pda)
    (pda->input-alphabet pda)
    (pda->stack-alphabet pda)
    (pda->rule-list pda)
    (pda->stack-init-symbol pda)
    (pda->accepting-states pda)
    (if (= item epsilon)
      (pda->stack pda)
      (cons item (pda->stack pda)))))

(define (pda->stack-pop pda item)
  (make-pda
    (pda->state-set pda)
    (pda->input-alphabet pda)
    (pda->stack-alphabet pda)
    (pda->rule-list pda)
    (pda->stack-init-symbol pda)
    (pda->accepting-states pda)
    (cdr (pda->stack pda))))

(define (pda->stack-top pda)
  (car (pda->stack)))

(define (pda->state-table m)
  (ninth (get-item m)))

(define (pda->make-state) 1)

(define (pda->step) 1)

(define (pda->accepts? input) 1)



;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;

(display "Test make-pda")
(newline)

(define test-pda (make-pda 
                  (local-def/Q 'q1 'q2 'q3 'q4) 
                  (local-def/sigma 0 1) 
                  (local-def/gamma '$)
                  (local-def/transitions
                    (trans-rule
                      'q1 :: epsilon :: epsilon => '$ :: 'q2)
                    (trans-rule
                      'q2 :: 0 :: epsilon => 0 :: 'q2)
                    (trans-rule
                      'q2 :: 1 :: 0 => epsilon :: 'q3)
                    (trans-rule
                     'q3 :: 1 :: 0 => epsilon :: 'q3)
                    (trans-rule
                      'q3 :: '$ :: '$ => epsilon :: 'q4))
                  (local-def/stack-init '$)
                  (local-def/accept-states 'q4)
                  (local-def/q0 'q1)
                  '()))
test-pda
(newline)

(display "Test rule-list")
(newline)
(define rule-list (pda->rule-list test-pda))
rule-list
(newline)

(define q2-trans-list
  (filter-rule-list-by-current-state
    (pda->rule-list test-pda)
    (second (pda->state-set test-pda))))

(display "Test pda->state table")
(newline)
(define table(pda->state-table test-pda))
table
(newline)



(display "Test match-entry-input-symbol")
(newline)
(match-entry-input-symbol table 'q2 0)
(newline)


