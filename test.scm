(use gauche.test)
(test-start "constraint")
(load "./constraint")

(test* "cvar-create" `((value . 123) (defined? . #t)) (cvar-create 123 #t))
(test* "cgraph-add-cvar"
       `((vars ((cvar . ((value . 123)
                         (defined? . #t)))
                (visited? . #f))))
       (let ((g (cgraph-create ())))
         (cgraph-add-cvar g (cvar-create 123 #t))))

(test-end :exit-on-failure #t)
