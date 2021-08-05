#lang racket

(require
  racket/function
  (prefix-in ee/ ee-lib))

(provide
  with-scope
  thunk/in-scope
  add-scope
  (rename-out [ee/bind! bind!])
  lookup)

(define current-scope (make-parameter #f))

(define-syntax-rule (with-scope body ...)
  (ee/with-scope sc
    (parameterize ([current-scope sc])
      body ...)))

(define-syntax-rule (thunk/in-scope body ...)
  (let ([ctx (ee/current-def-ctx)])
    (thunk
      (parameterize ([ee/current-def-ctx ctx])
        body ...))))

(define (add-scope stx)
  (ee/add-scope stx (current-scope)))

(define (lookup name [pred (λ (x) #t)])
  (define res (ee/lookup name pred))
  (unless res
    (raise-syntax-error #f "No declaration found for this identifier" name))
  res)