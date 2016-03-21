;; cvar: variable with constraints
;; cfunc: function describing the constraint
;; cgraph: constraint system graph

(use srfi-11)

(use gauche.record)

(define-record-type cvar #t #t
  (value)
  (defined?))

(define-record-type cgraph #t #t
  (cvars))

(define-record-type cgraph-node #t #t
  (cvar)
  (visited?))

(define (cgraph-add-cvar! cgraph cvar)
  (cgraph-cvars-set! cgraph (cons (make-cgraph-node cvar #f)
                                  (cgraph-cvars cgraph)))
  cgraph)

(define-record-type cfunc #t #t inputs outputs proc)

(define (cfunc-update! cfunc)
  (let1 inputs (cfunc-inputs cfunc)
    (when (and (map cvar-defined? inputs))
      (let-values ((vars (apply (cfunc-proc cfunc) (map cvar-value inputs))))
        (for-each (^[cvar val]
                    (cvar-value-set! cvar val)
                    (cvar-defined?-set! cvar #t))
                  (cfunc-outputs cfunc) vars)))))
