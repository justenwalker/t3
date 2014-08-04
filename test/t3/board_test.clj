(ns t3.board-test
  (:require [clojure.test :refer :all]
            [t3.board :as board]))

(def board-draw [:x :o :x
                 :o :x :o
                 :o :x :o])

(def board-winner-o [:o :x :x
                     :o :o :x
                     :x :x :o])

(def board-winner-x [:x :x :x
                     :o :o :x
                     :x :x :o])

(deftest board-tests
  (testing "New Board"
    (testing "all tiles are nil"
      (is (every? nil? board/new-board)))
    (testing "has 9 tiles"
      (is (= (count board/new-board) 9))))
  (testing "Index->Cells"
    (is (= (board/index->cell board-draw [3 4 5]) [:o :x :o])))
  (testing "Win Conditions"
    (is (= (board/winner? board-winner-o) :o))
    (is (= (board/winner? board-winner-x) :x))
    (is (= (board/winner? board-draw) false))))


