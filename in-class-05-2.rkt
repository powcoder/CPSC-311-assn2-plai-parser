#lang plai

;;;;;;;;;;;;;;;;;;;;;;;;;;; INITIAL DISCUSSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define INITIAL-DISCUSSION 0) ; section marker

;; So, we want first-class functions. What does that mean for a test like this one?
#;
(test (parse-deffun '{deffun {double x} {+ 1 2}})
      (fundef 'double 'n (parse '{+ n n})))

;; Let's evaluate:
#;
{double {+ 1 2}}



;; What did we use from the fundef? What didn't we? What do we need in our functions?

;; CAUTION: Let's evaluate this example:

#;
{with {f {fun {x} {+ x y}}}
  {with {y 1}
    {f 10}}}



;; Say.. remember the error we got from our broken lazy interpreter?
;;
;; Options?
;;
;; Let's go implement unbound-ids. This is our first (substantial) static analysis.
;; A static analysis is an analysis we run on the program (AST) to determine some
;; property about it WITHOUT running it. (Static analyses are generally guaranteed
;; to terminate.)
;;
;; (IT'S DOWN BELOW IN THE INTERPRETER SECTION! It's a static analysis, but in our
;; implementation we wait until JUST before interpretation starts to call it.)



;; failed-tests : -> (listof plai error reports)
;; Generates a list of the failed tests indicated by the plai language.
;; The plai documentation describes the format for those reports.
;; (Search for "plai" in Racket help, and read the testing framework info.)
;; Note that failed tests are everything but "good" tests: bad, exception,
;; and pred-exception (IIRC).
(define (failed-tests)
  (reverse ; PLAI adds new tests to the front of the list.
           ; So, we reverse.  (Why does it add to the front?
           ; For more efficient execution.  Ask me in person
           ; if you're curious why that is.)
   (filter (lambda (x) (not (symbol=? (first x) 'good))) 
           plai-all-test-results)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LANGUAGE DEFINITION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define LANGUAGE-DEFINITION 0) ; section marker



;; EBNF
;;
;; <FWAE> :: <num>
;;         | {+ <FWAE> <FWAE>}
;;         | {with {<id> <FWAE>} <FWAE>}
;;         | <id>
;;         | {fun ...}                     ; TODO
;;         | {<FWAE> <FWAE>}               ; application
;; <id> can be any symbols except +, with, and fun

;; TODO: Let's rewrite our double test in concrete syntax now!

(define-type FWAE    
  [num (n number?)] 
  [add (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (param-name symbol?) (body FWAE?)] ; DONE! No name included in fun values.
  [app (fun-exp FWAE?) (arg-expr FWAE?)]) ; note that the first field is an expression now, not necessarily a name

;; We'll jump through parsers quickly, as usual.

(define *reserved-symbols* '(+ with fun))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PARSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; valid-identifier? : any -> boolean
;; Determines whether the parameter is valid as an identifier name, i.e.,
;; a symbol that is not reserved.
(define (valid-identifier? sym)
  (and (symbol? sym)
       (not (member sym *reserved-symbols*))))

;; Reserved symbols.
(test (valid-identifier? '+) false)
(test (valid-identifier? 'with) false)
(test (valid-identifier? 'fun) false)

(test (valid-identifier? '-) true)

;; Not a symbol
(test (valid-identifier? '{+ 1 2}) false)
(test (valid-identifier? 3) false)
(test (valid-identifier? "id") false)

;; OK
(test (valid-identifier? 'id) true)
(test (valid-identifier? 'x) true)



;; parse : any -> FWAE
;; Consumes an s-expression (in FWAE's concrete syntax)
;; and generates the corresponding FWAE program.
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ lhs rhs) (add (parse lhs) (parse rhs))]
    [(list 'with (list (? valid-identifier? name) named-exp) body)
     (with name (parse named-exp) (parse body))]
    [(list 'fun (list (? valid-identifier? name)) body)
     (fun name (parse body))]
    [(? valid-identifier?) (id sexp)]
    [(list fun-exp arg-exp) (app (parse fun-exp) (parse arg-exp))]
    [_ (error 'parse "unable to parse ~a" sexp)]))

(test (parse 'x) (id 'x))
(test (parse 'ys) (id 'ys))

;; Numbers
(test (parse '3) (num 3))
(test (parse '0) (num 0))

;; Plain arithmetic.
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{+ 3 4}) (add (num 3) (num 4)))

;; With binding
(test (parse '{with {x 1} x}) (with 'x (num 1) (id 'x)))

(test (parse '{with {x {with {y 2} {+ x y}}} {with {z 3} {+ x z}}})
      (with 'x (with 'y (num 2) (add (id 'x) (id 'y)))
            (with 'z (num 3) (add (id 'x) (id 'z)))))

; fun's
(test (parse '{fun {x} x}) (fun 'x (id 'x)))
(test (parse '{fun {y} 1}) (fun 'y (num 1)))

; application
(test (parse '{a b}) (app (id 'a) (id 'b)))
(test (parse '{{fun {x} x} 1}) (app (fun 'x (id 'x)) (num 1)))


;; Error checking

; non-lists, reserved symbols (e.g., +), strings
(test/exn (parse "hello") "")
(test/exn (parse '+) "")
(test/exn (parse 'fun) "")
(test/exn (parse 'with) "")


; + with fewer or more than 2 arguments
(test/exn (parse '{+}) "")
(test/exn (parse '{+ 1}) "")
(test/exn (parse '{+ 1 2 3}) "")

; ill-structured with
(test/exn (parse '{with}) "")
(test/exn (parse '{with x}) "")
(test/exn (parse '{with x 2 3}) "")
(test/exn (parse '{with {x 1}}) "")
(test/exn (parse '{with {x 1} 2 3}) "")
(test/exn (parse '{with {x 1 2} 3}) "")
(test/exn (parse '{with {+ 1} 2}) "")

; ill-structured fun
(test/exn (parse '{fun}) "")
(test/exn (parse '{fun x}) "")
(test/exn (parse '{fun x 3}) "")
(test/exn (parse '{fun {x 1}}) "")
(test/exn (parse '{fun {x 1} 2}) "")
(test/exn (parse '{fun {x} 2 3}) "")
(test/exn (parse '{fun {+} 2}) "")

; ill-structured app
(test/exn (parse '{f}) "")
(test/exn (parse '{f 1 2}) "")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INTERPRETATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; unbound-ids : FWAE -> (listof symbol)
;; Given an FWAE, produces a list of the identifiers unbound in that expression
(define (unbound-ids expr)
  (type-case FWAE expr
    [num (n) empty]
    [id (name) (list name)]
    [add (lhs rhs) (append (unbound-ids lhs) (unbound-ids rhs))]
    [app (fe ae) (append (unbound-ids fe) (unbound-ids ae))]  ; {f x}
    ; {with {x 1} x}
    [with (name ne body) (append (unbound-ids ne)
                                 (remove* (list name) (unbound-ids body)))]
    ; (remove* list-of-what-to-remove list-to-remove-from) may be handy
    ; {fun {x} {+ x y}}
    [fun (name body) (remove* (list name) (unbound-ids body))]))

(define-type FWAE    
  [num (n number?)] 
  [add (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (param-name symbol?) (body FWAE?)] ; DONE! No name included in fun values.
  [app (fun-exp FWAE?) (arg-expr FWAE?)])

(test (unbound-ids (num 1)) empty)
(test (unbound-ids (id 'x)) (list 'x))
(test (unbound-ids (add (id 'x) (id 'y))) (list 'x 'y))
(test (unbound-ids (app (id 'x) (id 'y))) (list 'x 'y))
(test (unbound-ids (with 'x (id 'x) (id 'y))) (list 'x 'y))
(test (unbound-ids (with 'x (id 'y) (id 'x))) (list 'y))

; TODO: fun tests

;; TODO: let's define a value type!
(define-type Value
  [numV (n number?)]
  ; TODO: other values?
  )

;; interp : FWAE -> Value
;; consumes a FWAE and produces its value (which may be a function!)
(define (interp expr)
  (local [; Check for unbound IDs. Used in the body below.
          (define unbound-list (unbound-ids expr))
          
          ; So we don't accidentally call interp rather than helper :P
          (define (interp expr)
            (error "use helper instead!"))

	  ; Since we have a separate value type, we don't get to
	  ; just assume that the result of a call to our helper is
	  ; a number or a (num ...). Thus, this helper lets us turn
	  ; values back into abstract syntax. That may not be easy
	  ; in the future as our values get richer and richer!
	  ; Fortunately, we'll also leave substitution behind.
	  (define (decompile value)
	    (type-case Value value
	      [numV (n) (num n)]
	      ; TODO: function case
	      )
	  
          (define (helper expr)
            (type-case FWAE expr
              [num (n) (numV n)]
              [add (l r) 'TODO]
              [with (bound-id named-expr bound-body)
                    ; Just like before:
                    ; (1) Evaluate the named expression
                    ; (2) Substitute the result for the bound-id in the body
                    ; (3) Evaluate the resulting body (in tail position)
                    (helper (subst bound-body
                                   bound-id
                                   (decompile (helper named-expr))))]
              [id (v) (error 'interp "free identifier; this should NEVER occur b/c of check for unbound IDs")]
              [fun () 'TODO]
              [app (fun-expr arg-expr)
                   (local [(define fun-val (helper fun-expr))
                           (define arg-val (helper arg-expr))]
                     ; Now what?
                     'TODO)]))]
    ; Let's ensure that we aren't caught out by any unbound IDs.
    (if (empty? unbound-list)
        (helper expr)
        (error 'interp "unbound IDs found in ~a: ~a" expr unbound-list))))


;; subst : FWAE symbol FWAE -> FWAE
;; substitute the val for all FREE instances of id in expr
(define (subst expr sub-id val)
  (type-case FWAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val) 
                    (subst bound-body sub-id val)))]
    [id (n) (if (symbol=? n sub-id) val expr)]
    ; New case!!
    [fun ()
         'TODO]
    [app (fun-expr arg-expr)   (app (subst fun-expr sub-id val)
                                    (subst arg-expr sub-id val))]))

(test (interp (parse '{with {double {fun {x} {+ x x}}} {double 3}})) (num 6))

; Num
(test (subst (num 1) 'x (num 2)) (num 1))

; Check that subst actually uses val. Otherwise, not testing variations
; in val, b/c it's treated as a black box in substitution.
(test (subst (id 'x) 'x (num 1)) (num 1))
(test (subst (id 'x) 'x (fun 'y (id 'y))) (fun 'y (id 'y)))

; ID that doesn't match. (Tested matching already.)
(test (subst (id 'y) 'x (num 2)) (id 'y))

; Add/app
(test (subst (add (id 'x) (id 'x)) 'x (num 2)) (add (num 2) (num 2)))
(test (subst (add (id 'y) (id 'y)) 'x (num 2)) (add (id 'y) (id 'y)))
(test (subst (app (id 'x) (id 'x)) 'x (num 2)) (app (num 2) (num 2)))
(test (subst (app (id 'y) (id 'y)) 'x (num 2)) (app (id 'y) (id 'y)))

; With
(test (subst (with 'x (id 'x) (id 'x)) 'x (num 1)) (with 'x (num 1) (id 'x)))
(test (subst (with 'y (id 'x) (id 'x)) 'x (num 1)) (with 'y (num 1) (num 1)))

; Fun
(test (subst (fun 'x (id 'x)) 'x (num 1)) (fun 'x (id 'x)))
(test (subst (fun 'y (id 'x)) 'x (num 1)) (fun 'y (num 1)))



(test (interp (parse '1)) (numV 1))
(test (interp (parse '{+ 1 2})) (numV 3))

(test (interp (parse '{+ 5 5})) (numV 10))
(test (interp (parse '{with {x {+ 5 5}} {+ x x}})) (numV 20))
(test (interp (parse '{with {x 5} {+ x x}})) (numV 10))
(test (interp (parse '{with {x {+ 5 5}} {with {y {+ x 3}} {+ y y}}})) (numV 26))
(test (interp (parse '{with {x 5} {with {y {+ x 3}} {+ y y}}})) (numV 16))
(test (interp (parse '{with {x 5} {+ x {with {x 3} 10}}})) (numV 15))
(test (interp (parse '{with {x 5} {+ x {with {x 3} x}}})) (numV 8))
(test (interp (parse '{with {x 5} {+ x {with {y 3} x}}})) (numV 10))
(test (interp (parse '{with {x 5} {with {y x} y}})) (numV 5))
(test (interp (parse '{with {x 5} {with {x x} x}})) (numV 5))

(test (interp (parse '{{fun {x} {+ x x}} 1})) (numV 2))

(test (interp (parse '{with {double {fun {x} {+ x x}}} {double 5}})) (numV 10))

(test/exn (interp (parse 'x)) "")
(test/exn (interp (parse '{with {f {fun {x} {+ x y}}}
                                {with {y 1}
                                      {f 2}}})) "")
