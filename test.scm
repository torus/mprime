(use gauche.test)
(test-start "constraint")
(load "./constraint")

(test* "make-cvar" #t (cvar? (make-cvar 123 #t)))

(test* "make-cgraph-func-node" #t
       (cgraph-func-node?
        (let ((v1 (make-cvar 5 #t))
              (v2 (make-cvar 3 #t))
              (v3 (make-cvar #f #f)))
          (make-cgraph-func-node (make-cfunc +) (list v1 v2) (list v3)))))

(test "cfunc-apply" '(8)
      (lambda ()
        (let ((v1 (make-cvar 5 #t))
              (v2 (make-cvar 3 #t)))
          (let1 cfunc (make-cfunc +)
            (cfunc-apply cfunc (list v1 v2))))))

(test "cfunc-apply - multiple values" '(1 2)
      (lambda ()
        (let ((v1 (make-cvar 5 #t))
              (v2 (make-cvar 3 #t)))
          (let1 cfunc (make-cfunc quotient&remainder)
            (cfunc-apply cfunc (list v1 v2))))))

(test "cgraph-func-node-update!" '((5 3 8) (#t #t #t))
      (lambda ()
        (let ((v1 (make-cvar 5 #t))
              (v2 (make-cvar 3 #t))
              (v3 (make-cvar #f #f))
              (cfunc (make-cfunc +)))
          (let ((n1 (make-cgraph-var-node v1 #f #f))
                (n2 (make-cgraph-var-node v2 #f #f))
                (n3 (make-cgraph-var-node v3 #f #f)))
            (let ((nf (make-cgraph-func-node cfunc `(,n1 ,n2) `(,n3))))
              (cgraph-var-node-outputs-set! n1 `(,nf))
              (cgraph-var-node-outputs-set! n2 `(,nf))
              (cgraph-func-node-update! nf)
              (list (map cvar-value `(,v1 ,v2 ,v3))
                    (map cvar-defined? `(,v1 ,v2 ,v3))))))))

(test "cgraph-func-node-update! - multiple values" '((5 3 1 2) (#t #t #t #t))
      (lambda ()
        (let ((v1 (make-cvar 5 #t))
              (v2 (make-cvar 3 #t))
              (v3 (make-cvar #f #f))
              (v4 (make-cvar #f #f))
              (cfunc (make-cfunc quotient&remainder)))
          (let ((n1 (make-cgraph-var-node v1 #f #f))
                (n2 (make-cgraph-var-node v2 #f #f))
                (n3 (make-cgraph-var-node v3 #f #f))
                (n4 (make-cgraph-var-node v4 #f #f)))
            (let ((nf (make-cgraph-func-node cfunc `(,n1 ,n2) `(,n3 ,n4))))
              (cgraph-var-node-outputs-set! n1 `(,nf))
              (cgraph-var-node-outputs-set! n2 `(,nf))
              (cgraph-func-node-update! nf)
              (list (map cvar-value `(,v1 ,v2 ,v3 ,v4))
                    (map cvar-defined? `(,v1 ,v2 ,v3 ,v4))))))))

(test "cgraph-connect! and cgraph-set-cvars!" 8
      (lambda ()
        (let ((n1 (cgraph-new-var-node))
              (n2 (cgraph-new-var-node))
              (n3 (cgraph-new-var-node))
              (nf (cgraph-new-func-node +)))
          (cgraph-connect! nf `(,n1 ,n2) `(,n3))
          (cgraph-set-cvars! `((,n1 . 5) (,n2 . 3)))
          (cgraph-func-node-update! nf)

          (cvar-value (cgraph-var-node-cvar n3))
          )))

(define (desc text proc)
  (proc))

(desc "Fahrenheit-Celsius example from SICP"
      (lambda ()
        (let ((nC (cgraph-new-var-node))
              (nu (cgraph-new-var-node))
              (nv (cgraph-new-var-node))
              (nF (cgraph-new-var-node))
              (n*9 (cgraph-new-func-node (cut * <> 9)))
              (n/9 (cgraph-new-func-node (cut / <> 9)))
              (n*5 (cgraph-new-func-node (cut * <> 5)))
              (n/5 (cgraph-new-func-node (cut / <> 5)))
              (n+32 (cgraph-new-func-node (cut + <> 32)))
              (n-32 (cgraph-new-func-node (cut - <> 32))))
          (cgraph-connect! n*9 `(,nC) `(,nu))
          (cgraph-connect! n/9 `(,nu) `(,nC))
          (cgraph-connect! n/5 `(,nu) `(,nv))
          (cgraph-connect! n*5 `(,nv) `(,nu))
          (cgraph-connect! n+32 `(,nv) `(,nF))
          (cgraph-connect! n-32 `(,nF) `(,nv))

          (cgraph-set-cvars! `((,nF . 212)))
          (cgraph-func-node-update! n-32)
          (cgraph-func-node-update! n*5)
          (cgraph-func-node-update! n/9)

          (test* "212F in Celsius" 100 (cvar-value (cgraph-var-node-cvar nC)))

          (cgraph-set-cvars! `((,nC . 25)))
          (cgraph-func-node-update! n*9)
          (cgraph-func-node-update! n/5)
          (cgraph-func-node-update! n+32)

          (test* "25C in Fahrenheit" 77 (cvar-value (cgraph-var-node-cvar nF)))
          )))

(define (debug-proc name op)
  (lambda args
    ;; (print `(,name ,@args))
    (apply op args)))

(desc "Graph"
      (lambda ()
        (let ((n1 (cgraph-new-var-node))
              (n3 (cgraph-new-var-node))
              (n5 (cgraph-new-var-node))
              (n6 (cgraph-new-var-node))
              (n8 (cgraph-new-var-node))
              (f2 (cgraph-new-func-node (debug-proc "+" +)))
              (f4 (cgraph-new-func-node (debug-proc "*" *)))
              (f7 (cgraph-new-func-node (debug-proc "/" /)))
              (f9 (cgraph-new-func-node (debug-proc "-" -))))
          (cgraph-connect! f2 `(,n1 ,n5) `(,n3))
          (cgraph-connect! f4 `(,n1 ,n8) `(,n5))
          (cgraph-connect! f7 `(,n5 ,n6) `(,n8))
          (cgraph-connect! f9 `(,n6)     `(,n3))

          (cgraph-update! `((,n1 . 2) (,n8 . 3)))
          ;; (print (map (^n (cvar-value (cgraph-var-node-cvar n))) (list n1 n3 n5 n6 n8)))
          ;; (print (map (^n (cvar-defined? (cgraph-var-node-cvar n))) (list n1 n3 n5 n6 n8)))

          (test* "n1" 2 (cvar-value (cgraph-var-node-cvar n1)))
          (test* "n3" 8 (cvar-value (cgraph-var-node-cvar n3)))
          (test* "n5" 6 (cvar-value (cgraph-var-node-cvar n5)))
          (test* "n8" 3 (cvar-value (cgraph-var-node-cvar n8)))

          (cgraph-update! `((,n6 . 3)))
          ;; (print (map (^n (cvar-value (cgraph-var-node-cvar n))) (list n1 n3 n5 n6 n8)))
          ;; (print (map (^n (cvar-defined? (cgraph-var-node-cvar n))) (list n1 n3 n5 n6 n8)))

          (test* "n3" -3 (cvar-value (cgraph-var-node-cvar n3)))
          (test* "n5" 4 (cvar-value (cgraph-var-node-cvar n5)))
          (test* "n6" 3 (cvar-value (cgraph-var-node-cvar n6)))
          (test* "n8" 2 (cvar-value (cgraph-var-node-cvar n8)))
        )))

(test-end :exit-on-failure #t)
