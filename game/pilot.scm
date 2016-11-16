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

(define (add-location! name proc loc . inputs)
  (mecs-connect!
   (mecs-new-func
    (lambda (loc . rest)    ; -> ()
      (skip-if (not loc))
      (print #`"[,|name|]")
      (apply proc rest)
      (values)))
   (cons loc inputs) ()))

(define (add-path! trigger from to)
  (mecs-connect!
   (mecs-new-func (lambda (trigger)
                    (skip-if (not trigger))
                    (print "--->---")
                    (values #f #t)))
   `(,trigger) `(,from ,to)))

(define (show-trigger trigger-name destination)
  (print #`"* ,|destination| ,`(mecs-update! `((,,trigger-name . #t)))")
  )

(add-location!
 "わたしの家"
 (lambda ()
   (print "お母さん「鍛冶屋のシナガーさんに包丁を作ってもらって、」")
   (print "お母さん「ヒロウ村のおじいちゃんに持って行ってちょうだい」")
   (show-trigger 'ui:home->smith "鍛冶屋さんへ")
   )
 loc:home)

(add-path! ui:home->smith loc:home loc:smith)
(add-path! ui:smith->home loc:smith loc:home)

(add-location!
 "鍛冶屋"
 (lambda (has-stone?)
   (if has-stone?
       (begin
         (print "鍛冶屋シナガー「これは良い石だ。見つけてきてくれてありがとう」")
         (print "鍛冶屋シナガー「この包丁をおじいさんに持って行きなさい」")
         (show-trigger 'ui:pick-knife "包丁を受け取る")
         (show-trigger 'ui:smith->hilou "ヒロウ村へ"))
       (begin
         (print "わたし「鍛冶屋さんこんにちは」")
         (print "わたし「ヒロウ村のおじいさんの為に包丁をください」")
         (print "鍛冶屋シナガー「あいにく砥石がなくて包丁が作れないんだよ」")
         (print "鍛冶屋シナガー「オシャーゲ山へ行って光るアカッカ石を拾ってきてくれるかい？」")
         (show-trigger 'ui:smith->hill "オシャーゲ山へ")
         )
       ))
 loc:smith has-stone?)

(define pick-knife
 (mecs-new-func (lambda (ui:pick-knife) ; -> has-knife?
                  (skip-if (not ui:pick-knife))
                  (print "シナガーの包丁を手に入れた")
                  #t)))
(mecs-connect! pick-knife `(,ui:pick-knife) `(,has-knife?))

(add-location!
 "山"
 (lambda ()
   (print "わたし「あそこに光る石がある！」")
   (print "わたし「あれを鍛冶屋さんに持って行けばいいのね」")
   (show-trigger 'ui:pick-stone "石を拾う")
   )
 loc:hill)

(add-path! ui:smith->hill loc:smith loc:hill)
(add-path! ui:hill->smith loc:hill loc:smith)

(define pick-stone
 (mecs-new-func (lambda (ui:pick-stone) ; -> has-stone?
                  (skip-if (not ui:pick-stone))
                  (print "光る石を拾った")
                  #t)))

(mecs-connect! pick-stone `(,ui:pick-stone) `(,has-stone?))

;; ヒロウ村へ
;; 一方通行
(add-path! ui:smith->hilou loc:smith loc:hilou)

(add-location!
 "ヒロウ村"
 (lambda ()
   (print "わたし「おじちゃんはどこかな？」")
   (print "村人「おじいちゃんは港の近くの家にいるよ」")
   )
 loc:hilou)
