(use gauche.test)

(test-start "名称未定。パイロット版")
(load "./pilot.scm")

(test-section "初めの町")

(define ch1 (cut hash-table-get *tale* <>))

(mecs-update! `((,(ch1 'scn:home) . #t)))

(mecs-update! `((,(ch1 'ui:home->smith-1) . #t)))

(test* "smith-1" #t (mecs-var-value (mecs-var-node-var (ch1 'scn:smith-1))))

(mecs-update! `((,(ch1 'ui:smith-1->hill-1) . #t)))

(test* "hill-1" #t (mecs-var-value (mecs-var-node-var (ch1 'scn:hill-1))))

(mecs-update! `((,(ch1 'ui:pick-stone) . #t)))

(test* "hill-2" #t (mecs-var-value (mecs-var-node-var (ch1 'scn:hill-2))))

(mecs-update! `((,(ch1 'ui:hill-2->smith-2) . #t)))

(test* "smith-2" #t (mecs-var-value (mecs-var-node-var (ch1 'scn:smith-2))))

(mecs-update! `((,(ch1 'ui:pick-knife) . #t)))

(mecs-update! `((,(ch1 'ui:smith-3->hilou) . #t)))

(test-end)
