(ns t3.players
  (:require [t3.ai    :as ai]
            [t3.input :as i]))

;; Players
(defn new-player
  "Create a new player which makes moves using the movefn"
  [movefn xo]
  (assert (contains? #{:x :o} xo))
  {:movefn  (partial movefn xo)
   :mark xo})

(defn unbeatable-player
  "Makes an unbeatable ai player using the given mark"
  [mark] (assoc (new-player ai/unbeatable mark) :computer true))

(defn ai-player
  "Makes an ai player using the given mark and difficulty"
  [difficulty] (fn [mark] (assoc (new-player (partial ai/beatable difficulty) mark) :computer true)))

(defn human-player
  "Makes a human player"
  [mark] (new-player i/kb-move mark))
