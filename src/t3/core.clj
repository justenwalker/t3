(ns t3.core
  (:require [t3.players :as p]
            [t3.board :as b]
            [t3.print :as pb]
            [t3.game :as g]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class :main true))

;; Command Line Options
(def cli-options
  [["-p" "--players PLAYERS" "Number of human players (0 , 1, or 2)"
    :id :players
    :default 1
    :parse-fn #(Integer/parseInt %)
    :valildate [#(<= 0 % 2) "Valid values are 0, 1 or 2"]]
   ["-a" "--ai DIFFICULTY" "AI Difficulty: 1 (trivial) to 10 (unbeatable)"
    :id :ai
    :default 10
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 0 % 10) "Must be between 1 and 10"]]])

(defn usage [options-summary]
  (->> ["Tic-Tac-Toe"
        ""
        "Usage: t3 [options] {help|play}"
        ""
        options-summary
        ]
       (string/join \newline)))

(defn error-msg [errors]
  (str "There was an error parsing your commands line options:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))


(defn play-game [game]
  (loop [g game]
    (pb/clear-screen)
    (pb/print-title)
    (pb/print-player-turn g)
    (pb/print-game-board g)
    (let [ng (g/do-game-step g)
          winner (:win ng)
          end (= 0 (:moves ng))]
      (cond
       winner
       (do
         (pb/clear-screen)
         (pb/print-title)
         (pb/print-winner winner)
         (pb/print-game-board ng))
       end
       (do
         (pb/clear-screen)
         (pb/print-title)
         (pb/print-draw)
         (pb/print-game-board ng))
       :else (recur ng)))))

(defn start! [{:keys [players ai]}]
  (let [game (case players
               0 (g/new-game (p/ai-player ai) (p/ai-player ai))
               1 (g/new-game p/human-player (p/ai-player ai))
               2 (g/new-game p/human-player p/human-player))]
    (play-game game)))

(defn -main
  "Application Entrypoint"
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
     (not= (count arguments) 1) (exit 1 (usage summary))
     errors (exit 1 (error-msg errors)))
    (case (first arguments)
      "play" (start! options)
      "help" (exit 0 (usage summary))
      (exit 1 (usage summary)))))
