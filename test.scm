(use gauche.test)
(test-start "constraint")
(load "./constraint")

(test* "make-cvar" #t (cvar? (make-cvar 123 #t)))

(test* "cgraph-add-cvar!"
       #t
       (let ((g (make-cgraph ())))
         (cgraph? (cgraph-add-cvar! g (make-cvar 123 #t)))))

(test* "make-cfunc" #t
       (cfunc?
        (let ((v1 (make-cvar 5 #t))
              (v2 (make-cvar 3 #t))
              (v3 (make-cvar #f #f)))
          (make-cfunc (list v1 v2) (list v3) +))))

(test "cfunc-update!" '((8) (#t))
      (lambda ()
        (let ((v1 (make-cvar 5 #t))
              (v2 (make-cvar 3 #t))
              (v3 (make-cvar #f #f)))
          (let1 cfunc (make-cfunc (list v1 v2) (list v3) +)
            (cfunc-update! cfunc)
            (let1 outputs (cfunc-outputs cfunc)
              (list (map cvar-value outputs)
                    (map cvar-defined? outputs)))))))

(test "cfunc-update! - multiple values" '((1 2) (#t #t))
      (lambda ()
        (let ((v1 (make-cvar 5 #t))
              (v2 (make-cvar 3 #t))
              (v3 (make-cvar #f #f))
              (v4 (make-cvar #f #f)))
          (let1 cfunc (make-cfunc (list v1 v2) (list v3 v4) quotient&remainder)
            (cfunc-update! cfunc)
            (let1 outputs (cfunc-outputs cfunc)
              (list (map cvar-value outputs)
                    (map cvar-defined? outputs)))))))

(test-end :exit-on-failure #t)
