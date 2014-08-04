(ns t3.print)

(def player-names {:x "X" :o "O"})

(def font-height 3)

(def letter-x (str "\\ /"
                   " x "
                   "/ \\"))

(def letter-o (str " _ "
                   "/o\\"
                   "\\_/"))

(def hbar "---+---+---")

(def vbar "|||")

(defn empty-cell [i] (str "    " i "    "))

(defn scan-letter [i letter]
  (let [font-width (/ (count letter) font-height)
        start (* i font-width)
        end   (+ start font-width)] (subs letter start end )))

(defn to-letter [[i cell]]
  (condp = cell
    :x letter-x
    :o letter-o
    (empty-cell i)))

(defn line->letters
  [row line]
  (let [n (* 3 row)
        m (+ n 3)]
    (interpose vbar
               (map to-letter
                    (partition 2
                               (interleave (range n m) line))))))

(defn print-board-line
  [[row line]]
  (let [letters (line->letters row line)]
    (doseq [i (range 0 font-height)]
      (let [l (map (partial scan-letter i) letters)]
        (println (apply str l))))))

(defn zipboard
  "Zips each board row with it's row number"
  [board]
  (partition 2
             (interleave
              (range 0 3)
              (partition 3 board))))


(defn print-board [board]
  (let [zip (zipboard board)]
    (doseq [z zip]
      (let [row (first z)]
        (print-board-line z)
        (if (< row 2) (println hbar))))))

(defn print-title
  "Prints the Tic-Tac-Toe title"
  []
  (println
"  _______        ______            ______        \n"
"/_  __(_)______/_  __/__ ________/_  __/__  ___ \n"
" / / / / __/___// / / _ `/ __/___// / / _ \\/ -_)\n"
"/_/ /_/\\__/    /_/  \\_,_/\\__/    /_/  \\___/\\__/\n"
"+---+---+---+---+---+---+---+---+---+---+---+---+"))

(defn print-player-turn
  "Print player's turn"
  [{:keys [game players turn] :as game-state}]
  (let [player (nth players turn)
        mark (:mark player)
        turn (get player-names mark)]
    (println "| Player's Turn: " turn " |")
    (println "======================")))

(defn print-game-board
  "Prints the gameboard"
  [{:keys [game players turn] :as game-state}]
  (let [board (:board game)]
    (print-board board)))

(defn print-winner [winner]
  (println "| Winner! Player " (get player-names winner) " |")
  (println "======================"))

(defn print-draw []
  (println "|   Game Over    |")
  (println "| Everyone Loses |")
  (println "=================="))

(defn clear-screen
  "Clears the screen"
  []
  (let [esc (char 27)]
    (print (str esc "[2J")) ;; ANSI: clear screen
    (print (str esc "[;H")) ;; ANSI: cursor to top-left
    ))
