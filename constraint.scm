;; cvar: variable with constraints
;; cfunc: function describing the constraint
;; cgraph: constraint system graph

(use srfi-11)

(use gauche.record)
(use util.queue)

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
                         (filter (^n (not (cgraph-var-node-visited? n)))
                                 (cgraph-func-node-outputs cfunc-node))) vars)
        )
      vars)))

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
                                                       (cgraph-var-node-outputs node))))
            inputs)
  (cgraph-func-node-inputs-set! cfunc-node inputs)
  (cgraph-func-node-outputs-set! cfunc-node outputs))

(define (cgraph-set-cvars! node-value-pairs)
  (for-each (lambda (pair)
              (let1 cvar (cgraph-var-node-cvar (car pair))
                (cvar-value-set! cvar (cdr pair))
                (cvar-defined?-set! cvar #t))
              node-value-pairs)
            node-value-pairs))

;; Graph
(define (cgraph-update! node-value-pairs)
  (define queue (make-queue))
  (define visited ())
  (define (visit! n)
    (enqueue! queue n)
    (cgraph-var-node-visited?-set! n #t)
    (set! visited (cons n visited)))
  (define visited? cgraph-var-node-visited?)

  (cgraph-set-cvars! node-value-pairs)

  (for-each visit! (map car node-value-pairs))
  (let loop ()
    (unless (queue-empty? queue)
      (let ((node (dequeue! queue))
            (targets ()))

        (for-each (lambda (func-node)
                    (when (cgraph-func-node-update! func-node)
                      (set! targets (append targets (cgraph-func-node-outputs func-node)))))
                  (cgraph-var-node-outputs node))

        (for-each (^(node)
                   (unless (visited? node)
                     (visit! node)))
                  targets)
        (loop))))

  ;; Clean up visited? flags
  (for-each (cut cgraph-var-node-visited?-set! <> #f) visited)
  )
