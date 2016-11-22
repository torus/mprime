(use gauche.test)

(test-start "名称未定。パイロット版")
(load "./pilot.scm")

(test-section "初めの町")

(mecs-update! `((,scn:home . #t)))

(mecs-update! `((,ui:home->smith-1 . #t)))

(test* "smith-1" #t (mecs-var-value (mecs-var-node-var scn:smith-1)))

(mecs-update! `((,ui:smith-1->hill-1 . #t)))

(test* "hill-1" #t (mecs-var-value (mecs-var-node-var scn:hill-1)))

(mecs-update! `((,ui:pick-stone . #t)))

(test* "hill-2" #t (mecs-var-value (mecs-var-node-var scn:hill-2)))

(mecs-update! `((,ui:hill-2->smith-2 . #t)))

(test* "smith-2" #t (mecs-var-value (mecs-var-node-var scn:smith-2)))

(mecs-update! `((,ui:pick-knife . #t)))

(mecs-update! `((,ui:smith-3->hilou . #t)))

(test-end)
