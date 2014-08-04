(ns t3.ai-test
  (:require [clojure.test :refer :all]
            [t3.ai :as ai]
            [t3.board :as b]))

(def _ nil)
(def x :x)
(def o :o)

(def board1 [ x _ _
              _ o o
              _ x x])

(def board-fork [_ _ x
                 _ o _
                 x _ _])

(def board-block-fork [x _ _
                       _ x _
                       _ _ o])


(def board-block-fork2 [x _ _
                        _ o _
                        _ _ x])


(def board-force [_ _ x
                  _ o _
                  _ _ _])

(defn make-cell-list [board index]
  (b/index->cell board  (ai/index->win-pattern index)))

(defn move-rating-test [board index]
  (last
   (first
    (ai/rate-moves board o (set [index])))))

(deftest board-tests
  (testing "Cell Tests"
    (is (ai/win? x (make-cell-list board1 6)))
    (is (not (ai/win? x (make-cell-list board1 2))))
    (is (ai/block? x (make-cell-list board1 3)))
    (is (not (ai/block? x (make-cell-list board1 2 ))))
    (is (ai/fork? x (make-cell-list board-fork 0 )))
    (is (not (ai/fork? x (make-cell-list board-fork 1 ))))
    (is (ai/force? x (make-cell-list board-force 5)))
    (is (not (ai/force? x (make-cell-list board-force 6))))
    (is (ai/block-fork? o (make-cell-list board-block-fork 2)))
    (is (not (ai/block-fork? o (make-cell-list board-fork 3))))
    (is (ai/opposite-corner? o 6 board-force))
    (is (not (ai/opposite-corner? o 8 board-force)))
    (is (ai/corner? 0))
    (is (ai/corner? 2))
    (is (ai/corner? 6))
    (is (ai/corner? 8))
    (is (not (ai/corner? 5)))
    (is (ai/center? 4))
    (is (not (ai/center? 5))))
  (testing "Move Ratings"
    (is (= (move-rating-test board-block-fork 2) 128))
    (is (= (move-rating-test board-block-fork 5) 64))
    (is (= (move-rating-test board-block-fork2 2) 32))
    (is (= (move-rating-test board-block-fork2 5) 64))
    (is (= (move-rating-test board-fork 0)  32))
    (is (= (move-rating-test board-fork 1)  64))
    (is (= (move-rating-test board-fork 5)  64))
    (is (= (move-rating-test board-force 1) 64))
    (is (= (move-rating-test board-force 0) 4))
    ))
