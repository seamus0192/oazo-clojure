#lang typed/racket


;; (defn -main []
;;     (println "Hello, World!"))

(struct NumC ([n : Real]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct DefnC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct BoolC ([b : Boolean]) #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct AppC ([name : Symbol] [args : (Listof Symbol)]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(define-type ExprC (U NumC StrC DefnC BoolC IfC AppC IdC))

;; this struct may need to change due to named functions
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([bool : Boolean]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent)
(struct StrV ([str : String]) #:transparent)

(define-type Value (U CloV NumV BoolV PrimV StrV))

(struct Binding ([name : Symbol] [value : Value]) #:transparent)
(struct Env ([bindings : (Listof Binding)]) #:transparent)


;; Operator functions

(define (add [a : Real] [b : Real]) : Real
  (+ a b))

(define (subtract [a : Real] [b : Real]) : Real
  (- a b))

(define (multiply [a : Real] [b : Real]) : Real
  (* a b))

;; division of integers that can't be reduced to an integer yields a ratio,
;; i.e. 22/7 = 22/7, rather than a floating point or truncated value
(define (divide [a : Real] [b : Real]) : (U String Real)
  (if (= b 0)
      (error "Division by zero")
      (let ((result (/ a b)))
        (if (or (not (exact? a)) (not (exact? b)))
            result
            (if (integer? result)
                result
                (string-append (number->string a) "/" (number->string b)))))))


#;(define (interp [expr : ExprC] [env : Env]) : Value
  

  )