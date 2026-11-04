#lang plai-typed
(require "ps3-ast.rkt")

;; TODO: Implement the following two functions.
;;
;; parse should take an s-expression representing a program and return an
;; AST corresponding to the program.
;;
;; eval-base should take an expression e (i.e. an AST) and evaluate e,
;; returning the resulting value.
;;

;; See ps3-ast.rkt and README.md for more information.

;; Note that as in lecture 6, you probably want to implement a version
;; of eval that returns a result that can be an arbitrary value (not just
;; a BaseValue) and also returns a store.  Your eval-base would then be a
;; wrapper around this more general eval that tries to conver the value
;; to a BaseValue, and fails if it cannot be.
;;
;; For grading, the test cases all result in values that can be converted to base values.

(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([forms (s-exp->list s)])
       (cond
         [(empty? forms) (error 'parse "Expected a nonempty expression")]
         [(s-exp-symbol? (first forms))
          (case (s-exp->symbol (first forms))
            [(+) (plusC (parse (second forms)) (parse (third forms)))]
            [(*) (timesC (parse (second forms)) (parse (third forms)))]
            [(pair) (pairC (parse (second forms)) (parse (third forms)))]
            [(fst) (fstC (parse (second forms)))]
            [(snd) (sndC (parse (second forms)))]
            [(equal?) (equal?C (parse (second forms)) (parse (third forms)))]
            [(if) (ifC (parse (second forms)) (parse (third forms))
                       (parse (fourth forms)))]
            [(lambda) (lambdaC (s-exp->symbol (second forms)) (parse (third forms)))]
            [(let) (letC (s-exp->symbol (second forms)) (parse (third forms))
                         (parse (fourth forms)))]
            [(box) (boxC (parse (second forms)))]
            [(unbox) (unboxC (parse (second forms)))]
            [(set-box!) (setboxC (parse (second forms)) (parse (third forms)))]
            [(vector) (vectorC (map parse (rest forms)))]
            [(vector-length) (vector-lengthC (parse (second forms)))]
            [(vector-ref) (vector-refC (parse (second forms)) (parse (third forms)))]
            [(vector-set!) (vector-set!C (parse (second forms)) (parse (third forms))
                                         (parse (fourth forms)))]
            [(vector-make) (vector-makeC (parse (second forms)) (parse (third forms)))]
            [(subvector) (subvectorC (parse (second forms)) (parse (third forms))
                                     (parse (fourth forms)))]
            [(begin) (beginC (map parse (rest forms)))]
            [(transact) (transactC (parse (second forms)))]
            [else (appC (parse (first forms)) (parse (second forms)))])]
         [else (appC (parse (first forms)) (parse (second forms)))]))]
    [else (error 'parse "Unsupported expression")]))

(define-type-alias Location number)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define empty-store empty)
(define override-store cons)

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [pairV (left : Value) (right : Value)]
  [closV (env : Env) (x : symbol) (body : Expr)]
  [boxV (loc : Location)]
  [vectorV (start : Location) (size : number)])

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

(define-type Result
  [res (v : Value) (s : Store)])

(define (fetch (loc : Location) (sto : Store)) : Value
  (cond
    [(empty? sto) (error 'fetch "No location found")]
    [(= loc (cell-location (first sto))) (cell-val (first sto))]
    [else (fetch loc (rest sto))]))

(define (lookup (x : symbol) (env : Env)) : Value
  (cond
    [(empty? env) (error 'lookup "No binding found")]
    [(equal? x (bind-name (first env))) (bind-val (first env))]
    [else (lookup x (rest env))]))

;; Reserve consecutive locations so a subvector can share the same cells.
(define reserve-locations
  (let ([next-location (box 0)])
    (lambda (size)
      (let ([start (unbox next-location)])
        (begin
          (set-box! next-location (+ start size))
          start)))))

(define (element-location (v : Value) (index : number)) : Location
  (if (and (<= 0 index) (< index (vectorV-size v)))
      (+ (vectorV-start v) index)
      (error 'vector "Index out of bounds")))

(define (eval-vector-elements (env : Env) (sto : Store)
                              (es : (listof Expr)) (loc : Location)) : Store
  (if (empty? es)
      sto
      (let ([item (eval-env env sto (first es))])
        (eval-vector-elements env
                              (override-store (cell loc (res-v item)) (res-s item))
                              (rest es)
                              (+ loc 1)))))

(define (fill-vector (sto : Store) (loc : Location) (size : number)
                     (value : Value)) : Store
  (if (= size 0)
      sto
      (fill-vector (override-store (cell loc value) sto)
                   (+ loc 1) (- size 1) value)))

(define (eval-begin (env : Env) (sto : Store) (es : (listof Expr))) : Result
  (cond
    [(empty? es) (error 'begin "Expected at least one expression")]
    [(empty? (rest es)) (eval-env env sto (first es))]
    [else
     (let ([item (eval-env env sto (first es))])
       (eval-begin env (res-s item) (rest es)))]))

(define (to-base (value : Value)) : BaseValue
  (type-case Value value
    [numV (n) (numBV n)]
    [boolV (b) (boolBV b)]
    [pairV (left right) (pairBV (to-base left) (to-base right))]
    [else (error 'eval-base "Expected a base value")]))

(define (eval-env (env : Env) (sto : Store) (e : Expr)) : Result
  (type-case Expr e
    [numC (n) (res (numV n) sto)]
    [boolC (b) (res (boolV b) sto)]
    [lambdaC (x body) (res (closV env x body) sto)]
    [idC (x) (res (lookup x env) sto)]
    [plusC (e1 e2)
     (let* ([left (eval-env env sto e1)]
            [right (eval-env env (res-s left) e2)])
       (res (numV (+ (numV-n (res-v left)) (numV-n (res-v right)))) (res-s right)))]
    [timesC (e1 e2)
     (let* ([left (eval-env env sto e1)]
            [right (eval-env env (res-s left) e2)])
       (res (numV (* (numV-n (res-v left)) (numV-n (res-v right)))) (res-s right)))]
    [pairC (e1 e2)
     (let* ([left (eval-env env sto e1)]
            [right (eval-env env (res-s left) e2)])
       (res (pairV (res-v left) (res-v right)) (res-s right)))]
    [fstC (e)
     (let ([pair (eval-env env sto e)])
       (res (pairV-left (res-v pair)) (res-s pair)))]
    [sndC (e)
     (let ([pair (eval-env env sto e)])
       (res (pairV-right (res-v pair)) (res-s pair)))]
    [equal?C (e1 e2)
     (let* ([left (eval-env env sto e1)]
            [right (eval-env env (res-s left) e2)])
       (res (boolV (equal? (to-base (res-v left)) (to-base (res-v right))))
            (res-s right)))]
    [ifC (guard e1 e2)
     (let ([test (eval-env env sto guard)])
       (eval-env env (res-s test) (if (boolV-b (res-v test)) e1 e2)))]
    [letC (x e1 e2) (eval-env env sto (appC (lambdaC x e2) e1))]
    [appC (e1 e2)
     (let* ([function (eval-env env sto e1)]
            [argument (eval-env env (res-s function) e2)]
            [closure (res-v function)])
       (eval-env (extend-env (bind (closV-x closure) (res-v argument))
                             (closV-env closure))
                 (res-s argument)
                 (closV-body closure)))]
    [boxC (e)
     (let* ([item (eval-env env sto e)]
            [loc (reserve-locations 1)])
       (res (boxV loc) (override-store (cell loc (res-v item)) (res-s item))))]
    [unboxC (e)
     (let ([target (eval-env env sto e)])
       (res (fetch (boxV-loc (res-v target)) (res-s target)) (res-s target)))]
    [setboxC (e1 e2)
     (let* ([target (eval-env env sto e1)]
            [item (eval-env env (res-s target) e2)])
       (res (res-v item)
            (override-store (cell (boxV-loc (res-v target)) (res-v item)) (res-s item))))]
    [vectorC (es)
     (let* ([size (length es)]
            [start (reserve-locations size)])
       (res (vectorV start size) (eval-vector-elements env sto es start)))]
    [vector-lengthC (e)
     (let ([target (eval-env env sto e)])
       (res (numV (vectorV-size (res-v target))) (res-s target)))]
    [vector-refC (e1 e2)
     (let* ([target (eval-env env sto e1)]
            [index (eval-env env (res-s target) e2)]
            [loc (element-location (res-v target) (numV-n (res-v index)))])
       (res (fetch loc (res-s index)) (res-s index)))]
    [vector-set!C (e1 e2 e3)
     (let* ([target (eval-env env sto e1)]
            [index (eval-env env (res-s target) e2)]
            [item (eval-env env (res-s index) e3)]
            [loc (element-location (res-v target) (numV-n (res-v index)))])
       (res (res-v item) (override-store (cell loc (res-v item)) (res-s item))))]
    [vector-makeC (e1 e2)
     (let* ([size-result (eval-env env sto e1)]
            [item (eval-env env (res-s size-result) e2)]
            [size (numV-n (res-v size-result))]
            [start (reserve-locations size)])
       (res (vectorV start size) (fill-vector (res-s item) start size (res-v item))))]
    [subvectorC (e offset len)
     (let* ([target (eval-env env sto e)]
            [offset-result (eval-env env (res-s target) offset)]
            [length-result (eval-env env (res-s offset-result) len)]
            [v (res-v target)]
            [start (numV-n (res-v offset-result))]
            [size (numV-n (res-v length-result))])
       (if (and (<= 0 start) (<= 0 size) (<= (+ start size) (vectorV-size v)))
           (res (vectorV (+ (vectorV-start v) start) size) (res-s length-result))
           (error 'subvector "Range out of bounds")))]
    [beginC (es) (eval-begin env sto es)]
    [transactC (e)
     (let* ([result (eval-env env sto e)]
            [pair (res-v result)])
       (res (pairV-right pair)
            (if (boolV-b (pairV-left pair)) (res-s result) sto)))]))

(define (eval-base (e : Expr)) : BaseValue
  (to-base (res-v (eval-env empty-env empty-store e))))
