#lang plai

; uncomment the following line to hide all passing tests
; (print-only-errors)

;; ==========================================================
;;                     EBNF & DEFINE-TYPES
;; ==========================================================


;; DE = Distribution expression
;;
;; <DE> ::= <num>
;;     | {distribution <num>*}   ; <num>* means 0 or more <num> non-terminals
;;     | {uniform <num> <num>}
;;     | {+ <DE> <DE>}
;;     | {- <DE> <DE>}
;;     | {* <DE> <DE>}
;;     | {observe-that <DE> is <COMP> <DE>}
;;     | {with {<id> <DE>} <DE>}
;;     | <id>
;;
;; <COMP> ::= < | <= | == | >= | > | !=
;;
;; Note: <id> cannot be one of these reserved words:
;;       with distribution uniform + - * observe-that is < <= == >= > !=
;;
;; Note: the expression {uniform a b} evaluates to a discrete uniform 
;;       distribution from a to b (including a but EXCLUDING b). If
;;       a = b then the distribution is empty. The case a > b is not 
;;       allowed and must result in an error!
;;
;;       For example, {uniform 3 6} means the same as the range notation
;;       [3, 6) and includes the numbers 3, 4, and 5.

(define-type DE
  [distribution (values (listof number?))]
  [id (name symbol?)]
  [binop (op procedure?) (lhs DE?) (rhs DE?)]
  [compop (comp procedure?) (lhs DE?) (rhs DE?)]
  [with (name symbol?) (named-expr DE?) (body DE?)])


;; ==========================================================
;;                           PARSE
;; ==========================================================

(define RESERVED_SYMBOLS '(with distribution uniform + - * observe-that is < <= == >= > !=))

;; legal-id? : any -> bool
;; returns true if id is legal as an identifier, i.e.,
;; a symbol and not one of the reserved symbols.
(define (legal-id? id)
  (and (symbol? id)
       (not (member id RESERVED_SYMBOLS))))

; Simple tests
(test (legal-id? 'x) true)
(test (legal-id? 'num) true)
(test (legal-id? 'hello22) true)

; Not symbols
(test (legal-id? 2) false)
(test (legal-id? +) false)
(test (legal-id? '(a)) false)

; Reserved words
(test (legal-id? 'with) false)
(test (legal-id? 'distribution) false)
(test (legal-id? 'uniform) false)
(test (legal-id? 'observe-that) false)
(test (legal-id? 'is) false)
(test (legal-id? '+) false)
(test (legal-id? '-) false)
(test (legal-id? '*) false)
(test (legal-id? '>) false)
(test (legal-id? '>=) false)
(test (legal-id? '==) false)
(test (legal-id? '=) true) ; NOT a reserved word, but it kept messing us up; so included as a test!
(test (legal-id? '<=) false)
(test (legal-id? '<) false)
(test (legal-id? '!=) false)


; Note: You can use the given helper function legal-id? in parse to check
;       that an identifier is legal. It is not included in the automated tests,
;       so you are allowed to change it as needed.
;       Feel free (and encouraged) to structure your code by adding more helper
;       functions as needed. All additional top-level functions MUST however be
;       documented and tested on a similar level as the given functions!

;; parse : s-exp -> DE
;; Consumes an s-expression and generates the corresponding DE
(define (parse sexp)
  (id 'todo)) ; TODO

; TODO: write more tests for parse as needed
; Note: for some cases we sort and compare the underlying lists in
;       case your parser outputs them in a different order
(test (parse 'x) (id 'x))
(test (parse '{distribution}) (distribution empty))
(test (parse '1) (distribution '(1)))
(test (parse '{distribution 3}) (distribution '(3)))
(test (sort (distribution-values (parse '{uniform 9 10})) <) '(9))
(test (sort (distribution-values (parse '{uniform 10 10})) <) empty)
(test (sort (distribution-values (parse '{uniform 1 6})) <) '(1 2 3 4 5))
(test (sort (distribution-values (parse '{distribution 10 20 30})) <)
      '(10 20 30))
(test (parse '{+ 1 1})
      (binop + (distribution '(1)) (distribution '(1))))
(test (parse '{* {distribution 1 2} {distribution 3 4}})
      (binop * (distribution '(1 2)) (distribution '(3 4))))
(test (parse '{with {x 1} x})
      (with 'x (distribution '(1)) (id 'x)))
(test (parse '{with {x 1} {with {x 2} x}})
      (with 'x (distribution '(1))
            (with 'x (distribution '(2)) (id 'x))))
(test (parse '{* {- 1 2} {+ 3 4}})
      (binop *
             (binop - (distribution '(1)) (distribution '(2)))
             (binop + (distribution '(3)) (distribution '(4)))))
(test (parse '{observe-that {uniform 1 4} is >= {distribution 1 2 3}})
      (compop >=
             (distribution '(1 2 3)) (distribution '(1 2 3))))
(test (parse '{observe-that {- 1 2} is < {+ 3 4}})
      (compop <
             (binop - (distribution '(1)) (distribution '(2)))
             (binop + (distribution '(3)) (distribution '(4)))))

; These tests check for errors generated explicitly by you via (error ...)
; TODO: write more tests for checking that parse generates errors when needed
(test/exn (parse "I am a string, not a symbol") "")
(test/exn (parse '{with {} 1}) "")
(test/exn (parse '{with {{1 x}} x}) "")
(test/exn (parse '{uniform 1}) "")
(test/exn (parse '{+ 1 2 3}) "")
(test/exn (parse 'with) "")
(test/exn (parse '{a b c}) "")
(test/exn (parse '{uniform 9 7}) "")


;; ==========================================================
;;                           INTERP
;; ==========================================================

;; subst : DE symbol DE -> DE
;; substitutes second argument with third argument in first argument,
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument
(define (subst expr sub-id val)
  expr) ; TODO - you will need to use subst in your interp below

; TODO: The following test is just an example. Write more tests for subst!
(test (subst (id 'x) 'x (distribution'(1))) (distribution '(1)))

;; interp : DE -> (listof Number)
;; Consumes a DE representation of an expression and outputs the list
;; representation of the corresponding distribution
(define (interp expr)
  empty) ; TODO

; TODO: write more tests for interp as needed
; NOTE: our tests sort the output of interp before comparing when needed
(test (sort (interp (distribution '(2 3 1))) <) '(1 2 3))
(test (interp (with 'x (distribution '(1)) (id 'x))) '(1))
(test (interp (binop - (distribution '(8)) (distribution '(3)))) '(5))
(test (interp (binop -
                     (binop * (distribution '(5)) (distribution '(6)))
                     (binop + (distribution '(-10)) (distribution '(2)))))
      '(38))
(test (sort (interp (binop + (distribution '(1 2)) (distribution '(3 4)))) <) '(4 5 5 6))
(test (sort (interp (binop * (distribution '(1 2 3 4 5)) (distribution '(2 4)))) <)
      '(2 4 4 6 8 8 10 12 16 20))
(test (sort (interp (compop <= (distribution '(1 2 3)) (distribution '(1 2 3)))) <) '(1 1 1 2 2 3))
(test (sort (interp (compop < (distribution '(1 2 3)) (distribution '(1 2 3)))) <) '(1 1 2))
(test (sort (interp (compop <= (distribution '(1 2 3)) (distribution '(4)))) <) '(1 2 3))
(test (sort (interp (compop <= (distribution '(4)) (distribution '(1 2 3)))) <) empty)
(test (sort (interp (compop = (distribution '(1 2 3)) (distribution '(3 4 5)))) <) '(3))
(test (interp (with 'x (distribution '(1)) (distribution '(2)))) '(2))
(test (interp (with 'x (binop + (distribution '(1)) (distribution '(2))) (id 'x))) '(3))
(test (interp (with 'x (distribution '(1)) (id 'x))) '(1))
(test (interp (with 'x (distribution '(1))
                    (with 'x (distribution '(2)) (id 'x)))) '(2))
(test (interp (with 'x (distribution '(1))
                    (with 'x (id 'x) (id 'x)))) '(1))
(test/exn (interp (id 'y)) "")
