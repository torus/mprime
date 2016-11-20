(use gauche.test)

(test-start "名称未定。パイロット版")
(load "./pilot.scm")

(test-section "初めの町")

(mecs-update! `((,loc:home . #t)))

(mecs-update! `((,ui:home->smith-1 . #t)))

;(mecs-update! `((,has-knife? . #f) (,has-stone? . #f)))

(mecs-update! `((,ui:smith-1->hill . #t)))

(mecs-update! `((,ui:pick-stone . #t)))

(mecs-update! `((,ui:hill->smith-2 . #t)))

(mecs-update! `((,ui:pick-knife . #t)))

(mecs-update! `((,ui:smith-3->hilou . #t)))

(test-end)
