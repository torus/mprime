;; MEPHISTO Constraint System

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

(define-record-type mecs-func-node #t #t
  func
  (inputs)
  (outputs))

(define-record-type mecs-func #t #t proc)

(define (mecs-func-node-update! cfunc-node)
  (let ((inputs (map mecs-var-node-var (mecs-func-node-inputs cfunc-node)))
        (cfunc (mecs-func-node-func cfunc-node)))
    (let1 vars (mecs-func-apply cfunc inputs)
      (when vars
        (for-each (^[mecs-var val]
                      (mecs-var-value-set! mecs-var val)
                      (mecs-var-defined?-set! mecs-var #t))
                    (map mecs-var-node-var
                         (filter (^n (not (mecs-var-node-visited? n)))
                                 (mecs-func-node-outputs cfunc-node))) vars)
        )
      vars)))

;; mecs-func [mecs-var] => [value] or #f
(define (mecs-func-apply cfunc inputs)
  (if (every (cut eq? #t <>) (map mecs-var-defined? inputs))
      (guard (exc
              [(mecs-skip-calculation? exc)
               #f])
        (let-values ((vars (apply (mecs-func-proc cfunc) (map mecs-var-value inputs))))
          vars))
      #f))

;; Unitities

(define (mecs-new-var)
  (make-mecs-var-node (make-mecs-var #f #f) #f ()))

(define (mecs-new-func proc)
  (make-mecs-func-node (make-mecs-func proc) () ()))

(define (mecs-connect! cfunc-node inputs outputs)
  (for-each (lambda (node)
              (mecs-var-node-outputs-set! node (cons cfunc-node
                                                       (mecs-var-node-outputs node))))
            inputs)
  (mecs-func-node-inputs-set! cfunc-node inputs)
  (mecs-func-node-outputs-set! cfunc-node outputs))

(define (mecs-set-vars! node-value-pairs)
  (for-each (lambda (pair)
              (let1 var (mecs-var-node-var (car pair))
                (mecs-var-value-set! var (cdr pair))
                (mecs-var-defined?-set! var #t))
              node-value-pairs)
            node-value-pairs))

(define-condition-type <mecs-skip-calculation> <condition>
  mecs-skip-calculation?
  )

;; Graph
(define (mecs-update! node-value-pairs)
  (define queue (make-queue))
  (define visited ())
  (define (visit! n)
    (enqueue! queue n)
    (mecs-var-node-visited?-set! n #t)
    (set! visited (cons n visited)))
  (define visited? mecs-var-node-visited?)
  (define func-visited (make-hash-table))

  (mecs-set-vars! node-value-pairs)

  (for-each visit! (map car node-value-pairs))
  (let loop ()
    (unless (queue-empty? queue)
      (let ((node (dequeue! queue))
            (targets ()))

        (for-each (lambda (func-node)
                    (when (and
                           (not (hash-table-exists? func-visited func-node))
                           (mecs-func-node-update! func-node))
                          (set! targets (append targets (mecs-func-node-outputs func-node)))
                          (hash-table-put! func-visited func-node #t)
                          ))
                  (mecs-var-node-outputs node))

        (for-each (^(node)
                   (unless (visited? node)
                     (visit! node)))
                  targets)
        (loop))))

  ;; Clean up visited? flags
  (for-each (cut mecs-var-node-visited?-set! <> #f) visited)
  )
