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
      (print name)
      (apply proc rest)
      (values)))
   (cons loc inputs) ()))

(define (add-path! trigger from to)
  (mecs-connect!
   (mecs-new-func (lambda (trigger)
                    (skip-if (not trigger))
                    (values #f #t)))
   `(,trigger) `(,from ,to)))

(add-location!
 "わたしの家"
 (lambda ()
   (print "お母さん「鍛冶屋さんで包丁を作ってもらって、」")
   (print "お母さん「ヒロウ村のおじいちゃんに持って行ってちょうだい」")
   )
 loc:home)

(add-path! ui:home->smith loc:home loc:smith)
(add-path! ui:smith->home loc:smith loc:home)

(add-location!
 "鍛冶屋"
 (lambda (has-stone?)
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
       ))
 loc:smith has-stone?)

(define pick-knife
 (mecs-new-func (lambda (ui:pick-knife) ; -> has-knife?
                  (skip-if (not ui:pick-knife))
                  (print "包丁を手に入れた")
                  #t)))
(mecs-connect! pick-knife `(,ui:pick-knife) `(,has-knife?))

(add-location!
 "山"
 (lambda ()
   (print "わたし「あそこに光る石がある！」")
   (print "わたし「あれを鍛冶屋さんに持って行けばいいのね」"))
 loc:home)

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
