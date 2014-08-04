(ns t3.ai
  (:require [t3.board :as b]))

(defn opponent [mark] (get {:x :o :o :x} mark))

(defn opposite-corner [index] (get {0 8, 2 6, 6 2, 8 0} index))

(defn index->win-pattern
  "Convert a board index to the win patterns it contains"
  [index]
  (get b/index-win-patterns index))

(defn has? [mark n cell-list]
  "true if the cell-list has n number of marks"
  (= n (apply + (map #(if (= %1 mark) 1 0) cell-list))))

(defn empty-cells? [n cell-list]
  "true if the cell-list has n number of empty cells"
  (= n (apply + (map #(if (nil? %1) 1 0) cell-list))))

(defn empty-middle-cell? [cell-list]
  "true if the cell-list an empty middle"
  (-> cell-list
      rest
      first
      nil?))

(defn win? [mark cells-list]
  "true if 2 existing marks are in the row/col/diag (ie: a winning move)"
  (some true? (map #(has? mark 2 %) cells-list)))

(defn block? [mark cells-list]
  "true if the opponent would win by making this move (ie: a block)"
  (some true? (map #(has? (opponent mark) 2 %) cells-list)))

(defn fork? [mark cells-list]
  "true if marking the cell will create two ways to win next turn"
  (>=
   (apply +
          (map #(if (and (has? mark 1 %)
                         (empty-cells? 2 %)) 1 0)
               cells-list)) 2))

(defn force-middle? [mark cells-list]
  "true if marking this forces an opponent to block you"
  (some true? (map #(and (has? mark 2 %)
                         (empty-middle-cell? %)) cells-list)))

(defn force? [mark cells-list]
  "true if marking this forces an opponent to block you"
  (some true? (map #(and (has? mark 1 %)
                         (empty-cells? 2 %)) cells-list)))

(defn block-fork? [mark cells-list]
  "true if marking the cell blocks a fork without forceing a new one"
  (fork? (opponent mark) cells-list))

(defn corner?
  "true if the index is a corner"
  [index] (contains? #{0 2 6 8} index))

(defn opposite-corner? [mark index board]
  "true if opponent is in opposite corner"
  (if (corner? index)
    (= (opponent mark)
       (get board (opposite-corner index)))
    false))

(defn center? [index] (= index 4))

(defn rate-move [board mark index]
  "Assign a point value to a mark on the board at the given index"
  (let [ index-list  (index->win-pattern index)
         next-board  (assoc board index mark)
         cells-list  (b/index->cell board index-list)
         cells-list2 (b/index->cell next-board index-list)
         force-middle (force-middle? mark cells-list2)
         force (force? mark cells-list)
         block-fork (block-fork? mark cells-list)
         corner (corner? index)]
    (cond
      (win? mark cells-list)  1024     ;; Win right now
      (block? mark cells-list) 512     ;; Prevent player from winning next turn
      (fork? mark cells-list)  256     ;; Make 2 possible win moves next turn
      (and force-middle                ;; Force an edge move via a blocked fork
            block-fork) 128
      (and force (not corner)) 64      ;; Force a block with an edge move
      block-fork 32                    ;; Block opponent's fork
      (center? index) 16               ;; Take center
      (opposite-corner? mark index board) 8 ;; Take opposite corner
      corner 4
      :else 0))) ;; Take any empty corner

(defn rate-moves [board mark moves]
  (->> moves
       (map #(rate-move board mark %))
       (zipmap moves)
       (sort-by last >)))

(defn unbeatable
  "AI that always picks the best available move"
  [mark {:keys [board moves] :as game}]
  (->> (rate-moves board mark moves)
       (map first)
       (first)))

(defn beatable
  "AI that sometimes takes the second best move"
  [difficulty mark {:keys [board moves] :as game}]
  (let [best-moves (rate-moves board mark moves)
        n (count moves)
        p (rand 10)
        i (if (< p difficulty) 0 (mod 1 n))]
    (first (nth best-moves i))))
