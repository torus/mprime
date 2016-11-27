(load "../constraint")


;; fourteen

(define (skip-if con)
  (when con (raise (condition (<mecs-skip-calculation>)))))

(define-record-type ft-tale #t #t
  vars)

(define (ft-new-tale)
  (make-ft-tale (make-hash-table)))

(define *tale* (ft-new-tale))

(define (ft-add-var! tale key)
  (hash-table-put! (ft-tale-vars tale) key (mecs-new-var)))

(ft-add-var! *tale* 'scn:home)
(ft-add-var! *tale* 'ui:home->smith-1)
(ft-add-var! *tale* 'ui:smith-1->home)
(ft-add-var! *tale* 'scn:smith-1)
(ft-add-var! *tale* 'scn:smith-2)
(ft-add-var! *tale* 'scn:smith-3)
(ft-add-var! *tale* 'ui:smith-1->hill-1)
(ft-add-var! *tale* 'ui:hill-2->smith-2)
(ft-add-var! *tale* 'scn:hill-1)
(ft-add-var! *tale* 'scn:hill-2)
(ft-add-var! *tale* 'has-stone?)
(ft-add-var! *tale* 'ui:pick-stone)
(ft-add-var! *tale* 'ui:pick-knife)
(ft-add-var! *tale* 'has-knife?)
(ft-add-var! *tale* 'ui:smith-3->hilou)
(ft-add-var! *tale* 'scn:hilou)

(define (ft-add-scene! tale name proc loc . inputs)
  (mecs-connect!
   (mecs-new-func
    (lambda (loc . rest)    ; -> ()
      (skip-if (not loc))
      (print #`"[,|name|]")
      (apply proc rest)
      (values)))
   (map (cut hash-table-get (ft-tale-vars tale) <>) (cons loc inputs)) ()))

(define (ft-add-path! tale trigger from to)
  (mecs-connect!
   (mecs-new-func (lambda (trigger)
                    (skip-if (not trigger))
                    (print "--->---")
                    (values #f #t)))
   (list (hash-table-get (ft-tale-vars tale) trigger))
   (map (cut hash-table-get (ft-tale-vars tale) <>) `(,from ,to))))

(define (ft-show-trigger trigger-name destination)
  (print #`"* ,|destination| ,`(mecs-update! `((,,trigger-name . #t)))")
  )

(define (tale-add-scene! name proc loc . inputs)
  (apply ft-add-scene! *tale* name proc loc inputs))

(define (tale-add-path! trigger from to)
  (ft-add-path! *tale* trigger from to))

(define (tale-initialize!)
  (mecs-update! `((,(hash-table-get (ft-tale-vars *tale*) 'scn:home) . #t))))

(define (tale-trigger! trigger)
  (mecs-update! `((,(hash-table-get (ft-tale-vars *tale*) trigger) . #t))))

(tale-add-scene!
 "わたしの家"
 (lambda ()
   (print "お母さん「鍛冶屋のシナガーさんに包丁を作ってもらって、」")
   (print "お母さん「ヒロウ村のおじいちゃんに持って行ってちょうだい」")
   (ft-show-trigger 'ui:home->smith-1 "鍛冶屋さんへ")
   )
 'scn:home)

(tale-add-path! 'ui:home->smith-1 'scn:home 'scn:smith-1)
(tale-add-path! 'ui:smith-1->home 'scn:smith-1 'scn:home)

(tale-add-scene!
 "鍛冶屋 1"
 (lambda ()
   (print "わたし「鍛冶屋さんこんにちは」")
   (print "わたし「ヒロウ村のおじいさんの為に包丁をください」")
   (print "鍛冶屋シナガー「あいにく砥石がなくて包丁が作れないんだよ」")
   (print "鍛冶屋シナガー「オシャーゲ山へ行って光るアカッカ石を拾ってきてくれるかい？」")
   (ft-show-trigger 'ui:smith-1->hill-1 "オシャーゲ山へ"))
 'scn:smith-1)

(tale-add-scene!
 "鍛冶屋 2"
 (lambda ()
   (print "鍛冶屋シナガー「これは良い石だ。見つけてきてくれてありがとう」")
   (print "鍛冶屋シナガー「この包丁をおじいさんに持って行きなさい」")
   (ft-show-trigger 'ui:pick-knife "包丁を受け取る"))
 'scn:smith-2)

(tale-add-scene!
 "鍛冶屋 3"
 (lambda ()
   (print "鍛冶屋シナガー「ヒロウ村は坂を降りたところだよ」")
   (ft-show-trigger 'ui:smith-3->hilou "ヒロウ村へ"))
 'scn:smith-3)

(tale-add-path! 'ui:pick-knife 'scn:smith-2 'scn:smith-3)

(tale-add-scene!
 "オシャーゲ山 1"
 (lambda ()
   (print "わたし「あそこに光る石がある！」")
   (ft-show-trigger 'ui:pick-stone "石を拾う")
   )
 'scn:hill-1)

(tale-add-scene!
 "オシャーゲ山 2"
 (lambda ()
   (print "わたし「これを鍛冶屋さんに持って行けばいいのね」")
   (ft-show-trigger 'ui:hill-2->smith-2 "鍛冶屋さんへ")
   )
 'scn:hill-2)

(tale-add-path! 'ui:pick-stone 'scn:hill-1 'scn:hill-2)

(tale-add-path! 'ui:smith-1->hill-1 'scn:smith-1 'scn:hill-1)
(tale-add-path! 'ui:hill-2->smith-2 'scn:hill-2 'scn:smith-2)

;; ヒロウ村へ
(tale-add-path! 'ui:smith-3->hilou 'scn:smith-3 'scn:hilou)

(tale-add-scene!
 "ヒロウ村"
 (lambda ()
   (print "わたし「おじちゃんはどこかな？」")
   (print "村人「おじいちゃんは港の近くの家にいるよ」")
   )
 'scn:hilou)
