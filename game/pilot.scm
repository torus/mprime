(load "../constraint")


;; fourteen

(define (skip-if con)
  (when con (raise (condition (<mecs-skip-calculation>)))))

(define chapter-1 (make-hash-table))
(define (ch1 key)
  (hash-table-get chapter-1 key))

(define (ft-add-var! table key)
  (hash-table-put! table key (mecs-new-var)))

(ft-add-var! chapter-1 'scn:home)
(ft-add-var! chapter-1 'ui:home->smith-1)
(ft-add-var! chapter-1 'ui:smith-1->home)
(ft-add-var! chapter-1 'scn:smith-1)
(ft-add-var! chapter-1 'scn:smith-2)
(ft-add-var! chapter-1 'scn:smith-3)
(ft-add-var! chapter-1 'ui:smith-1->hill-1)
(ft-add-var! chapter-1 'ui:hill-2->smith-2)
(ft-add-var! chapter-1 'scn:hill-1)
(ft-add-var! chapter-1 'scn:hill-2)
(ft-add-var! chapter-1 'has-stone?)
(ft-add-var! chapter-1 'ui:pick-stone)
(ft-add-var! chapter-1 'ui:pick-knife)
(ft-add-var! chapter-1 'has-knife?)
(ft-add-var! chapter-1 'ui:smith-3->hilou)
(ft-add-var! chapter-1 'scn:hilou)

(define (ft-add-scene! name proc loc . inputs)
  (mecs-connect!
   (mecs-new-func
    (lambda (loc . rest)    ; -> ()
      (skip-if (not loc))
      (print #`"[,|name|]")
      (apply proc rest)
      (values)))
   (cons loc inputs) ()))

(define (ft-add-path! trigger from to)
  (mecs-connect!
   (mecs-new-func (lambda (trigger)
                    (skip-if (not trigger))
                    (print "--->---")
                    (values #f #t)))
   `(,trigger) `(,from ,to)))

(define (ft-show-trigger trigger-name destination)
  (print #`"* ,|destination| ,`(mecs-update! `((,,trigger-name . #t)))")
  )

(ft-add-scene!
 "わたしの家"
 (lambda ()
   (print "お母さん「鍛冶屋のシナガーさんに包丁を作ってもらって、」")
   (print "お母さん「ヒロウ村のおじいちゃんに持って行ってちょうだい」")
   (ft-show-trigger 'ui:home->smith-1 "鍛冶屋さんへ")
   )
 (ch1 'scn:home))

(ft-add-path! (ch1 'ui:home->smith-1) (ch1 'scn:home) (ch1 'scn:smith-1))
(ft-add-path! (ch1 'ui:smith-1->home) (ch1 'scn:smith-1) (ch1 'scn:home))

(ft-add-scene!
 "鍛冶屋 1"
 (lambda ()
   (print "わたし「鍛冶屋さんこんにちは」")
   (print "わたし「ヒロウ村のおじいさんの為に包丁をください」")
   (print "鍛冶屋シナガー「あいにく砥石がなくて包丁が作れないんだよ」")
   (print "鍛冶屋シナガー「オシャーゲ山へ行って光るアカッカ石を拾ってきてくれるかい？」")
   (ft-show-trigger 'ui:smith-1->hill-1 "オシャーゲ山へ"))
 (ch1 'scn:smith-1))

(ft-add-scene!
 "鍛冶屋 2"
 (lambda ()
   (print "鍛冶屋シナガー「これは良い石だ。見つけてきてくれてありがとう」")
   (print "鍛冶屋シナガー「この包丁をおじいさんに持って行きなさい」")
   (ft-show-trigger 'ui:pick-knife "包丁を受け取る"))
 (ch1 'scn:smith-2))

(ft-add-scene!
 "鍛冶屋 3"
 (lambda ()
   (print "鍛冶屋シナガー「ヒロウ村は坂を降りたところだよ」")
   (ft-show-trigger 'ui:smith-3->hilou "ヒロウ村へ"))
 (ch1 'scn:smith-3))

(ft-add-path! (ch1 'ui:pick-knife) (ch1 'scn:smith-2) (ch1 'scn:smith-3))

(ft-add-scene!
 "オシャーゲ山 1"
 (lambda ()
   (print "わたし「あそこに光る石がある！」")
   (ft-show-trigger 'ui:pick-stone "石を拾う")
   )
 (ch1 'scn:hill-1))

(ft-add-scene!
 "オシャーゲ山 2"
 (lambda ()
   (print "わたし「これを鍛冶屋さんに持って行けばいいのね」")
   (ft-show-trigger 'ui:hill-2->smith-2 "鍛冶屋さんへ")
   )
 (ch1 'scn:hill-2))

(ft-add-path! (ch1 'ui:pick-stone) (ch1 'scn:hill-1) (ch1 'scn:hill-2))

(ft-add-path! (ch1 'ui:smith-1->hill-1) (ch1 'scn:smith-1) (ch1 'scn:hill-1))
(ft-add-path! (ch1 'ui:hill-2->smith-2) (ch1 'scn:hill-2) (ch1 'scn:smith-2))

;; ヒロウ村へ
(ft-add-path! (ch1 'ui:smith-3->hilou) (ch1 'scn:smith-3) (ch1 'scn:hilou))

(ft-add-scene!
 "ヒロウ村"
 (lambda ()
   (print "わたし「おじちゃんはどこかな？」")
   (print "村人「おじいちゃんは港の近くの家にいるよ」")
   )
 (ch1 'scn:hilou))
