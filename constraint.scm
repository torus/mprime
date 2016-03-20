;; cvar: variable with constraints
;; cfunc: function describing the constraint
;; cgraph: constraint system graph

(define (cvar-create val def?)
  (let1 cvar `((value . ,val)
               (defined? . ,def?))
    cvar))

;; vars: [(cvar . visited)]
(define (cgraph-create vars)
  (acons 'vars vars ()))

(define (cgraph-add-cvar cgraph cvar)
  (cgraph-create (cons `((cvar . ,cvar)
                        (visited? . #f))
                      (cdr (assq 'vars cgraph)))))

