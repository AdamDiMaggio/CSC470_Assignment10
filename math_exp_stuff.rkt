;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname math_exp_stuff) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;constructor
(define empty-env
  (lambda () (list 'empty-env)))

;adds name-value to current environment
(define extend-env
  (lambda (var-name var-value env)
    (list 'extend-env var-name var-value env)))

(define extend-env*
  (lambda (lon lov env)
    (cond
      ((null? lon) env)
      (else (extend-env* (cdr lon) (cdr lov) (extend-env (car lon) (car lov) env))))))

;is this env empty?
(define empty-env?
  (lambda (env)
    (eq? (car env) 'empty-env)))

;is this env not empty?
(define extend-env?
  (lambda (env)
    (eq? (car env) 'extend-env)))

;getters
;return variable name
(define get-name
  (lambda (env)
    (car (cdr env))))

;return variable value
(define get-value
  (lambda (env)
    (car (cdr (cdr env)))))
;return next environment
(define get-env
  (lambda (env)
    (car (cdr (cdr (cdr env))))))

;returns value associated  w/ var-name or #f if not found
(define apply-env
  (lambda (var-name env)
    (cond
      ((empty-env? env) #f)                                   ;if empty, resolve to false
      ((eq? var-name (get-name env)) (get-value env)) ;
      (else (apply-env var-name (get-env env))))))

;is a variable name already used?
(define has-binding?
  (lambda (var-name env)
    (not (eq? (apply-env var-name env) #f)))) ;when apply comes back false, nothing is bound and this returns true

;(define env (extend-env 'a 5 (extend-env 'b 7 (empty-env))))
;(extend-env* '(c d e) '(1 2 3) env)


;Grammar constructors
(define var-exp
  (lambda (s)
    (list 'var-exp s)))

(define lambda-exp
  (lambda (s lc-exp)
    (list 'lambda-exp s lc-exp)))

(define app-exp
  (lambda (lambda-exp param-value)
    (list 'app-exp lambda-exp param-value))) ;lambda-exp = operator      param-value=operand

(define lit-exp
  (lambda (n)
    (list 'lit-exp n)))

(define math-exp
  (lambda (operator lc-exp1 lc-exp2)
    (list 'math-exp operator lc-exp1 lc-exp2)))

;grammar getters
(define lc-exp->type ;get type of lambda calculus expression(var, lambda, app)
  (lambda (lc-exp)
    (car lc-exp)))
;***************************

;lit-exp
(define lit-exp->value
  (lambda (lit-exp)
    (cadr lit-exp)))
;***************************

;math-exp
(define math-exp->operator
  (lambda (math-exp)
    (cadr math-exp)))

(define math-exp->lc-exp1
  (lambda (math-exp)
    (caddr math-exp)))

(define math-exp->lc-exp2
  (lambda (math-exp)
    (cadddr math-exp)))

;***************************

;var-exp
(define var-exp->var-name 
  (lambda (var-exp)
    (cadr var-exp)))
;***************************
;lambda-exp
(define lambda-exp->parameter-name
  (lambda (lambda-exp)
    (cadr lambda-exp)))

(define lambda-exp->body
  (lambda (lambda-exp)
    (caddr lambda-exp)))
;***************************
;app-exp
(define app-exp->lambda-exp
  (lambda (app-exp)
    (cadr app-exp)))

(define app-exp->parameter-input
  (lambda (app-exp)
    (caddr app-exp)))


;Grammar predicates
(define var-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'var-exp)))

(define lambda-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'lambda-exp)))

(define app-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'app-exp)))

(define lit-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'lit-exp)))

(define math-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'math-exp)))

;Codex code stuff here!!!!
;(get-value 'a)

;(func 'gets x 'does y)          [y is also codex, needs to be parsed]

;(run (func 'gets x 'does y) 'with parameter)

;(literal 5)

;(domath x +,-,*,/ y)

;Codex getters
(define literal-exp->value
  (lambda (literal-exp)
    (cadr literal-exp)))

(define domath-exp->operator
  (lambda (domath-exp)
    (caddr domath-exp)))

(define domath-exp->lc-exp1
  (lambda (domath-exp)
    (cadr domath-exp)))

(define domath-exp->lc-exp2
  (lambda (domath-exp)
    (cadddr domath-exp)))

(define get-value-exp->value
  (lambda (get-value-exp)
    (cadr get-value-exp)))

(define func-exp->value
  (lambda (func-exp)
    (caddr func-exp)))

(define func-exp->body
  (lambda (func-exp)
    (car (cdr (cdr (cdr (cdr func-exp)))))))

(define run-exp->func
  (lambda (run-exp)
    (cadr run-exp)))

(define run-exp->parameter
  (lambda (run-exp)
    (cadddr run-exp))) 


;math helper
(define math-machine
  (lambda (operator lc-exp1-value lc-exp2-value)
    (cond
      ((eq? operator '+) (+ lc-exp1-value lc-exp2-value))
      ((eq? operator '-) (- lc-exp1-value lc-exp2-value))
      ((eq? operator '*) (* lc-exp1-value lc-exp2-value))
      ((eq? operator '/) (/ lc-exp1-value lc-exp2-value))
      (else #f))))





;Parse/Unparse
(define parse ;parse-expression
  (lambda (codex)
    (cond
      ((eq? (car codex) 'domath) (math-exp ;constructor
                                (domath-exp->operator codex) ;operator
                                (parse (domath-exp->lc-exp1 codex))  ;first exp/num
                                (parse (domath-exp->lc-exp2 codex)))) ;second exp/num
      ((eq? (car codex) 'literal) (lit-exp (literal-exp->value codex)))
      ((eq? (car codex) 'get-value) (var-exp
                                     (get-value-exp->value codex))) ;var-exp value
      ((eq? (car codex) 'func) (lambda-exp
                                (func-exp->value codex)             ;lambda-exp value
                                (parse (func-exp->body codex))))    ;lambda-exp body
      ((eq? (car codex) 'run) (app-exp
                               (parse (run-exp->func codex))          ;app-exp lambda
                               (parse (run-exp->parameter codex)))))));app-exp param name


;take parsed code and execute it
(define apply-expression
  (lambda (lc-exp env)
    (cond
      ((math-exp? lc-exp) (math-machine
                           (math-exp->operator lc-exp)
                           (apply-expression (math-exp->lc-exp1 lc-exp) env)   ;since both are lc-exp, the math machine needs the actual #s not the exp, so we run apply
                           (apply-expression (math-exp->lc-exp2 lc-exp) env)))
      ((lit-exp? lc-exp) (lit-exp->value lc-exp))
      ((var-exp? lc-exp) (apply-env (var-exp->var-name lc-exp) env))                           ;get variable name from var-exp, look up value with apply-env, resolve value
      ((lambda-exp? lc-exp) (apply-expression (lambda-exp->body lc-exp) env))                  ;execute body of lambda-exp, which is a lc-exp, so call apply on it
      ((app-exp? lc-exp) (let* ((the-lambda (app-exp->lambda-exp lc-exp))                      ;let creates a local env
                                (the-lambda-param-name (lambda-exp->parameter-name the-lambda));3 variable here: 
                                (the-parameter-value (apply-expression (app-exp->parameter-input lc-exp) env))       ;(lambda-exp from app-exp),(name of param in lambda-exp), (new value for that param)
                                (the-new-env (extend-env the-lambda-param-name the-parameter-value env)))  ;update env with above variables
                           (apply-expression the-lambda the-new-env))))))

(define run-program
  (lambda (codex-src env)
    (apply-expression (parse codex-src) env)))

;test code here
(define codex-example '(run (func gets a does (get-value a)) with (get-value c)))
(parse codex-example)



(define test-env (extend-env* '(c d e) '(1 2 3) (empty-env)))
;(apply-expression (var-exp 'c) test-env)
(run-program codex-example test-env)

(define codextest '(get-value c))
(run-program codextest test-env)

(define codextest2 '(literal 7))
(run-program codextest2 test-env)

(define math-codex '(domath (get-value c) / (run (func gets a does (get-value a)) with (literal 7))))
(parse math-codex)
(run-program math-codex test-env)

      









     