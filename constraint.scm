;; mecs-var: variable with constraints
;; cfunc: function describing the constraint
;; cgraph: constraint system graph

(use srfi-11)

(use gauche.record)
(use util.queue)

(define-record-type mecs-var #t #t
  (value)
  (defined?))

(define-record-type mecs-var-node #t #t
  var
  (visited?)
  (outputs))

(define-record-type cgraph-func-node #t #t
  cfunc
  (inputs)
  (outputs))

(define-record-type cfunc #t #t proc)

(define (cgraph-func-node-update! cfunc-node)
  (let ((inputs (map mecs-var-node-var (cgraph-func-node-inputs cfunc-node)))
        (cfunc (cgraph-func-node-cfunc cfunc-node)))
    (let1 vars (cfunc-apply cfunc inputs)
      (when vars
        (for-each (^[mecs-var val]
                      (mecs-var-value-set! mecs-var val)
                      (mecs-var-defined?-set! mecs-var #t))
                    (map mecs-var-node-var
                         (filter (^n (not (mecs-var-node-visited? n)))
                                 (cgraph-func-node-outputs cfunc-node))) vars)
        )
      vars)))

;; cfunc [mecs-var] => [value] or #f
(define (cfunc-apply cfunc inputs)
  (if (every (cut eq? #t <>) (map mecs-var-defined? inputs))
      (let-values ((vars (apply (cfunc-proc cfunc) (map mecs-var-value inputs))))
        vars)
      #f))


;; Unitities

(define (mecs-new-var)
  (make-mecs-var-node (make-mecs-var #f #f) #f ()))

(define (mecs-new-func proc)
  (make-cgraph-func-node (make-cfunc proc) () ()))

(define (cgraph-connect! cfunc-node inputs outputs)
  (for-each (lambda (node)
              (mecs-var-node-outputs-set! node (cons cfunc-node
                                                       (mecs-var-node-outputs node))))
            inputs)
  (cgraph-func-node-inputs-set! cfunc-node inputs)
  (cgraph-func-node-outputs-set! cfunc-node outputs))

(define (cgraph-set-cvars! node-value-pairs)
  (for-each (lambda (pair)
              (let1 var (mecs-var-node-var (car pair))
                (mecs-var-value-set! var (cdr pair))
                (mecs-var-defined?-set! var #t))
              node-value-pairs)
            node-value-pairs))

;; Graph
(define (cgraph-update! node-value-pairs)
  (define queue (make-queue))
  (define visited ())
  (define (visit! n)
    (enqueue! queue n)
    (mecs-var-node-visited?-set! n #t)
    (set! visited (cons n visited)))
  (define visited? mecs-var-node-visited?)

  (cgraph-set-cvars! node-value-pairs)

  (for-each visit! (map car node-value-pairs))
  (let loop ()
    (unless (queue-empty? queue)
      (let ((node (dequeue! queue))
            (targets ()))

        (for-each (lambda (func-node)
                    (when (cgraph-func-node-update! func-node)
                      (set! targets (append targets (cgraph-func-node-outputs func-node)))))
                  (mecs-var-node-outputs node))

        (for-each (^(node)
                   (unless (visited? node)
                     (visit! node)))
                  targets)
        (loop))))

  ;; Clean up visited? flags
  (for-each (cut mecs-var-node-visited?-set! <> #f) visited)
  )
