;; cvar: variable with constraints
;; cfunc: function describing the constraint
;; cgraph: constraint system graph

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
