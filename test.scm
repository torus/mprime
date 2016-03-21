(use gauche.test)
(test-start "constraint")
(load "./constraint")

(test* "make-cvar" #t (cvar? (make-cvar 123 #t)))

(test* "cgraph-add-cvar!"
       #t
       (let ((g (make-cgraph ())))
         (cgraph? (cgraph-add-cvar! g (make-cvar 123 #t)))))

(test-end :exit-on-failure #t)
