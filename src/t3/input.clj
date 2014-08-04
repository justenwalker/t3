(ns t3.input
  (:require [clojure.string :as s]))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\d+" s)))

(defn get-input [prompt]
  (print prompt)
  (flush)
  (read-line))

(defn get-valid-input [prompt validfn]
  (loop [input (get-input prompt)]
    (let [in (validfn input)]
      (if (not (nil? in)) in
        (recur (get-input (str input " is not valid - try again\n" prompt)))))))

(defn print-available-moves [mark game]
  (let [moves (:moves game)]
    (println
     (str "Moves: " (s/join "," (sort < (seq moves)))))))

(defn validate-human-input [game input]
  (try
    (let [moves (:moves game)
          in    (parse-int input)]
      (if (contains? moves in) in nil))
    (catch Exception e nil)))

(defn kb-move [mark game]
  (print-available-moves mark game)
  (get-valid-input "Your Move:" (partial validate-human-input game)))
