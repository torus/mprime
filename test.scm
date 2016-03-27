(use gauche.test)
(test-start "constraint")
(load "./constraint")

(test* "make-cvar" #t (cvar? (make-cvar 123 #t)))

;; (test* "cgraph-add-cvar!"
;;        #t
;;        (let ((g (make-cgraph ())))
;;          (cgraph? (cgraph-add-cvar! g (make-cvar 123 #t)))))

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


(test-end :exit-on-failure #t)
