(use gauche.test)

(test-start "名称未定。パイロット版")
(load "./pilot.scm")

(test-section "初めの町")

(define ch1 (cut hash-table-get (ft-tale-vars *tale*) <>))

(tale-initialize!)

(tale-trigger! 'ui:home->smith-1)

(test* "smith-1" #t (mecs-var-value (mecs-var-node-var (ch1 'scn:smith-1))))

(tale-trigger! 'ui:smith-1->hill-1)

(test* "hill-1" #t (mecs-var-value (mecs-var-node-var (ch1 'scn:hill-1))))

(tale-trigger! 'ui:pick-stone)

(test* "hill-2" #t (mecs-var-value (mecs-var-node-var (ch1 'scn:hill-2))))

(tale-trigger! 'ui:hill-2->smith-2)

(test* "smith-2" #t (mecs-var-value (mecs-var-node-var (ch1 'scn:smith-2))))

(tale-trigger! 'ui:pick-knife)

(tale-trigger! 'ui:smith-3->hilou)

(test-end)
