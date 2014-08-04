(ns t3.board)

;; A new empty board with no markers
(def new-board (vec (repeat 9 nil)))

;; List of patterns which are possible win conditoins
(def win-patterns
  '( ;; Rows
    [0 1 2]
    [3 4 5]
    [6 7 8]
    ;; Columns
    [0 3 6]
    [1 4 7]
    [2 5 8]
    ;; Diagonals
    [0 4 8]
    [2 4 6]))

;; map of move index to possible win-patterns in which it is contained
(def index-win-patterns
  (reduce (fn [h pattern]
            (reduce #(assoc %1 %2 (concat [pattern] (or (get %1 %2) []))) h pattern))
          {} win-patterns))

(defn index->cell
  "Converts indicies into their corresponding marks on the game board"
  [board indicies]
  (map (fn [x] (if (sequential? x)
                 (index->cell board x)
                 (get board x))) indicies))

(defn win?
  "Returns the wining marker (:x or :o) if all 3 tiles are identical.
  Otherwise returns false"
  [tiles]
  (reduce #(if (identical? %1 %2) (or %1 false) false) tiles))

(defn winner?
  "Checks the board for possible winners
  Returns the winner if found.
  If there is no winner, it returns false."
  [board]
  (loop [patterns win-patterns]
    (if (empty? patterns) false ;; No more patterns to check
      (let [winner (win? (index->cell board (first patterns)))]
        (if winner winner
          (recur (rest patterns)))))))
