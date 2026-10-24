#lang plai-typed

(require "ps2-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;
;; See ps2-ast.rkt and README.md for more information.

(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (valC (numV (s-exp->number s)))]
    [(s-exp-boolean? s) (valC (boolV (s-exp->boolean s)))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (case (s-exp->symbol (first l))
         [(+) (plusC (parse (second l)) (parse (third l)))]
         [(*) (timesC (parse (second l)) (parse (third l)))]
         [(equal?) (equal?C (parse (second l)) (parse (third l)))]
         [(if) (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))]
         [(list) (listC (map parse (rest l)))]
         [(cons) (consC (parse (second l)) (parse (third l)))]
         [(first) (firstC (parse (second l)))]
         [(rest) (restC (parse (second l)))]
         [(let) (letC (map (lambda (binding) (let ([b (s-exp->list binding)]) (pair (s-exp->symbol (first b)) (parse (second b))))) (s-exp->list (second l))) (parse (third l)))]
         [(let*) (let*C (map (lambda (binding) (let ([b (s-exp->list binding)]) (pair (s-exp->symbol (first b)) (parse (second b))))) (s-exp->list (second l))) (parse (third l)))]
         [(unpack) (unpackC (map s-exp->symbol (s-exp->list (second l))) (parse (third l)) (parse (fourth l)))]
         [(natrec) (let ([step (s-exp->list (fourth l))]) (natrecC (parse (second l)) (parse (third l)) (s-exp->symbol (first step)) (s-exp->symbol (second step)) (parse (third step))))]
         [(listrec) (let ([step (s-exp->list (fourth l))]) (listrecC (parse (second l)) (parse (third l)) (s-exp->symbol (first step)) (s-exp->symbol (second step)) (s-exp->symbol (third step)) (parse (fourth step))))]
         ))]
    ))

(define-type Binding ; define binding type
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))  ; useful for shorter code 
(define empty-env empty) 
(define extend-env cons) ; constructor for list of env

(define (lookup (x : symbol) (env : Env)) : Value
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    ))

(define (eval-let* (bindings : (listof (symbol * Expr)))
                  (env : Env)
                  (body : Expr)) : Value
  (cond
    [(empty? bindings) (eval-env env body)]
    [else
     (let ([b (first bindings)])
     (eval-let*
        (rest bindings)
        (extend-env (bind (fst b) (eval-env env (snd b))) env)
        body))]))

(define (eval-env (env : Env) (e : Expr)) : Value
  (type-case Expr e
    [valC (v) v]

    [plusC (e1 e2)
      (numV (+ (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]

    [timesC (e1 e2)
      (numV (* (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]

    [equal?C (e1 e2) (boolV (equal? (eval-env env e1) (eval-env env e2)))]

    [ifC (condition e1 e2)
      (if (boolV-b (eval-env env condition))
      (eval-env env e1)
      (eval-env env e2))]

    [listC (es) (listV (map (lambda (e) (eval-env env e)) es))]

    [firstC (e) (first (listV-vs (eval-env env e)))]

    [restC (e) (listV (rest (listV-vs (eval-env env e))))]

    [consC (e1 e2) (listV (cons (eval-env env e1) (listV-vs (eval-env env e2))))]

    [idC (x) (lookup x env)]

    [letC (bindings body)
        (let ([new-bindings
        (map (lambda (b) (bind (fst b) (eval-env env (snd b)))) bindings)])
    (eval-env (append new-bindings env) body))]

    [let*C (bindings body) (eval-let* bindings env body)]

    [unpackC (vars e1 e2) (let ([values (listV-vs (eval-env env e1))]) (eval-env (append (map2 bind vars values) env) e2))]

    [natrecC (e1 e2 x y e3)
             (let ([n (numV-n (eval-env env e1))])
               (if (= n 0)
                   (eval-env env e2)
                   (let ([previous
                          (eval-env env
                                    (natrecC (valC (numV (- n 1)))
                                             e2 x y e3))])
                     (eval-env
                      (extend-env
                       (bind x (numV (- n 1)))
                       (extend-env (bind y previous) env))
                      e3))))]

    [listrecC (e1 e2 hd tl res e3)
              (let ([vs (listV-vs (eval-env env e1))])
                (if (empty? vs)
                    (eval-env env e2)
                    (let ([previous
                           (eval-env env
                                     (listrecC
                                      (valC (listV (rest vs)))
                                      e2 hd tl res e3))])
                      (eval-env
                       (extend-env
                        (bind hd (first vs))
                        (extend-env
                         (bind tl (listV (rest vs)))
                         (extend-env (bind res previous) env)))
                       e3))))]
    
    ))

(define (eval (e : Expr)) : Value
  (eval-env empty-env e))