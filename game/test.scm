(use gauche.test)

(test-start "名称未定。パイロット版")
(load "./pilot.scm")

(test-section "初めの町")

(define (get-value label)
  (mecs-var-value (mecs-var-node-var (hash-table-get (ft-tale-vars *tale*) label))))

(tale-initialize!)

(tale-trigger! 'ui:home->smith-1)

(test* "smith-1" #t (get-value 'scn:smith-1))

(test "trigger 'ui:pick-stone is not available" (test-error <trigger-is-not-available>)
      (cut tale-trigger! 'ui:pick-stone))

(tale-trigger! 'ui:smith-1->hill-1)

(test* "hill-1" #t (get-value 'scn:hill-1))

(tale-trigger! 'ui:pick-stone)

(test* "hill-2" #t (get-value 'scn:hill-2))

(tale-trigger! 'ui:hill-2->smith-2)

(test* "smith-2" #t (get-value 'scn:smith-2))

(tale-trigger! 'ui:pick-knife)

(tale-trigger! 'ui:smith-3->hilou)

(test-end)
