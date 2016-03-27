;; cvar: variable with constraints
;; cfunc: function describing the constraint
;; cgraph: constraint system graph

(use srfi-11)

(use gauche.record)

(define-record-type cvar #t #t
  (value)
  (defined?))

(define-record-type cgraph-var-node #t #t
  cvar
  (visited?)
  (outputs))

(define-record-type cgraph-func-node #t #t
  cfunc
  (inputs)
  (outputs))

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


;; Unitities

(define (cgraph-new-var-node)
  (make-cgraph-var-node (make-cvar #f #f) #f ()))

(define (cgraph-new-func-node proc)
  (make-cgraph-func-node (make-cfunc proc) () ()))

(define (cgraph-connect! cfunc-node inputs outputs)
  (for-each (lambda (node)
              (cgraph-var-node-outputs-set! node (cons cfunc-node
                                                       cgraph-var-node-outputs-set!)))
            inputs)
  (cgraph-func-node-inputs-set! cfunc-node inputs)
  (cgraph-func-node-outputs-set! cfunc-node outputs))

(define (cgraph-update-vars! node-value-pairs)
  (for-each (lambda (pair)
              (let1 cvar (cgraph-var-node-cvar (car pair))
                (cvar-value-set! cvar (cdr pair))
                (cvar-defined?-set! cvar #t))
              node-value-pairs)
            node-value-pairs))
