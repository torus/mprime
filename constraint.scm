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

(define-record-type cgraph-var-node #t #t
  cvar
  (visited?)
  (outputs))

(define-record-type cgraph-func-node #t #t
  cfunc
  (inputs)
  (outputs))

;; (define (cgraph-add-cvar! cgraph cvar)
;;   (cgraph-cvars-set! cgraph (cons (make-cgraph-node cvar #f)
;;                                   (cgraph-cvars cgraph)))
;;   cgraph)

(define-record-type cfunc #t #t proc)

(define (cgraph-func-node-update! cfunc-node)
  (let ((inputs (map cgraph-var-node-cvar (cgraph-func-node-inputs cfunc-node)))
        (cfunc (cgraph-func-node-cfunc cfunc-node)))
    (let1 vars (cfunc-apply cfunc inputs)
      (when vars
        (for-each (^[cvar val]
                      (cvar-value-set! cvar val)
                      (cvar-defined?-set! cvar #t))
                    (map cgraph-var-node-cvar
                         (cgraph-func-node-outputs cfunc-node)) vars)
        ))))

;; cfunc [cvar] => [value] or #f
(define (cfunc-apply cfunc inputs)
  (if (every (cut eq? #t <>) (map cvar-defined? inputs))
      (let-values ((vars (apply (cfunc-proc cfunc) (map cvar-value inputs))))
        vars)
      #f))
