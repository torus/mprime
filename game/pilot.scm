(load "../constraint")

(define (skip-if con)
  (when con (raise (condition (<mecs-skip-calculation>)))))

(define loc:home (mecs-new-var))
(define ui:home->smith (mecs-new-var))
(define ui:smith->home (mecs-new-var))
(define loc:smith (mecs-new-var))
(define ui:smith->hill (mecs-new-var))
(define ui:hill->smith (mecs-new-var))
(define loc:hill (mecs-new-var))
(define has-stone? (mecs-new-var))
(define ui:pick-stone (mecs-new-var))
(define ui:pick-knife (mecs-new-var))
(define has-knife? (mecs-new-var))
(define ui:smith->hilou (mecs-new-var))
(define loc:hilou (mecs-new-var))

(define show-home
  (mecs-new-func (lambda (loc:home)    ; -> ()
                   (skip-if (not loc:home))
                   (print "わたしの家")
                   (print "お母さん「鍛冶屋さんで包丁を作ってもらって、」")
                   (print "お母さん「ヒロウ村のおじいちゃんに持って行ってちょうだい」")
                   (values))))
(mecs-connect! show-home `(,loc:home) ())

(define home->smith
 (mecs-new-func (lambda (ui:home->smith) ; -> home smith
                  (skip-if (not ui:home->smith))
                  (print "鍛冶屋さんへ移動")
                  (values #f #t))))
(define smith->home
 (mecs-new-func (lambda (ui:smith->home) ; -> home smith
                  (skip-if (not ui:smith->home))
                  (print "わたしの家へ移動")
                  (values #t #f))))

(define show-smith
  (mecs-new-func (lambda (loc:smith has-stone?)    ; -> ()
                   (skip-if (not loc:smith))
                   (print "鍛冶屋")
                   (if has-stone?
                       (begin
                         (print "鍛冶屋「これは良い石だ。見つけてきてくれてありがとう」")
                         (print "鍛冶屋「この包丁をおじいさんに持って行きなさい」"))
                       (begin
                         (print "わたし「鍛冶屋さんこんにちは」")
                         (print "わたし「ヒロウ村のおじいさんの為に包丁をください」")
                         (print "鍛冶屋「あいにく石を切らしてしまって包丁が作れないんだよ」")
                         (print "鍛冶屋「山へ行って石を拾ってきてくれるかい？」")
                         )
                       )
                   (values))))
(mecs-connect! show-smith `(,loc:smith ,has-stone?) ())

(define pick-knife
 (mecs-new-func (lambda (ui:pick-knife) ; -> has-knife?
                  (skip-if (not ui:pick-knife))
                  (print "包丁を手に入れた")
                  #t)))
(mecs-connect! pick-knife `(,ui:pick-knife) `(,has-knife?))

(mecs-connect! home->smith `(,ui:home->smith) `(,loc:home ,loc:smith))
(mecs-connect! smith->home `(,ui:smith->home) `(,loc:home ,loc:smith))

(define show-hill
  (mecs-new-func (lambda (loc:home)    ; -> ()
                   (skip-if (not loc:home))
                   (print "山")
                   (print "わたし「あそこに光る石がある！」")
                   (print "わたし「あれを鍛冶屋さんに持って行けばいいのね」")
                   (values))))
(mecs-connect! show-hill `(,loc:hill) ())

(define smith->hill
 (mecs-new-func (lambda (ui:smith->hill) ; -> smith hill
                  (skip-if (not ui:smith->hill))
                  (print "山へ移動")
                  (values #f #t))))
(define hill->smith
 (mecs-new-func (lambda (ui:hill->smith) ; -> smith hill
                  (skip-if (not ui:hill->smith))
                  (print "鍛冶屋へ移動")
                  (values #t #f))))
(mecs-connect! smith->hill `(,ui:smith->hill) `(,loc:smith ,loc:hill))
(mecs-connect! hill->smith `(,ui:hill->smith) `(,loc:smith ,loc:hill))

(define pick-stone
 (mecs-new-func (lambda (ui:pick-stone) ; -> has-stone?
                  (skip-if (not ui:pick-stone))
                  (print "光る石を拾った")
                  #t)))

(mecs-connect! pick-stone `(,ui:pick-stone) `(,has-stone?))

;; ヒロウ村へ
;; 一方通行
(define smith->hilou
 (mecs-new-func (lambda (ui:smith->hilou) ; -> smith hilou
                  (skip-if (not ui:smith->hilou))
                  (print "ヒロウ村へ移動")
                  (values #f #t))))
(mecs-connect! smith->hilou `(,ui:smith->hilou) `(,loc:smith ,loc:hilou))

(define show-hilou
  (mecs-new-func (lambda (loc:hilou)    ; -> ()
                   (skip-if (not loc:hilou))
                   (print "ヒロウ村")
                   (print "わたし「おじちゃんはどこかな？」")
                   (print "村人「おじいちゃんは港の近くの家にいるよ」")
                   (values))))
(mecs-connect! show-hilou `(,loc:hilou) ())
