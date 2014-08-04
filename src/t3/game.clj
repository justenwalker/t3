(ns t3.game
  (:require [t3.players :as p]
            [t3.board :as b]))

(defn new-game
  "Create a new game state with an empty board and two players"
  [player1 player2]
  {
   :game {
          :board b/new-board
          :moves (set (range 0 9))}
   :players [(player1 :x) (player2 :o)]
   :moves 9
   :turn 0})

(defn do-move
  "Makes a player perform a move on the game board
  Returns a new game state"
  [{:keys [movefn mark ] :as player}
   {:keys [board  moves] :as game}]
  (let [m (movefn game)]
    (assert (contains? moves m)) ;; Must be a valid move
    {:board (assoc board m mark)
     :moves (disj moves m)}))

(defn do-game-step
  "Perform a game step"
  [{:keys [game players turn] :as game-state}]
  (let [
        player (nth players turn)
        ng     (do-move player game)
        board  (:board ng)
        win    (b/winner? board)
        moves  (count (:moves ng))]
    (if (:computer player) (Thread/sleep 1000))
    (assoc game-state
      :game ng
      :turn (mod (inc turn) 2)
      :win   win
      :moves moves)))
