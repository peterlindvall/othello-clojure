(ns tutorial.ai
  (:use [othello.core])
  (:use [clojure.repl :only (doc)]
        [test.core :only (is=)]))

(defn
  get-valid-moves [board player]
  (filter #(valid-board-move? board player (first %) (second %)) (keys board)))

(defn
  debug [& objects]
  (do
    (print (apply str (conj objects "\n")))
    (first objects)))

(defn
  #^{:test (fn []
             (let [board (simple-string->board ".BBWB."
                                               "...B.."
                                               "......")
                   state {:board board :player-in-turn "W"}]
               (is= (:move (minimax-move-strategy state '("W" "B") 1)) [1 3])))}
  minimax-move-strategy
  ([state players] (minimax-move-strategy state players Double/POSITIVE_INFINITY))
  ([state players depth]
    (let [stop-depth depth
          get-score (fn [board player]
                      (count (filter (partial = player) (vals board))))
          player (:player-in-turn state)
          maximizing-player? (partial = player)
          minimax (fn minimax [state depth prev-move prev-player]
                    (let [board (:board state)
                          player-in-turn (:player-in-turn state)
                          take-move (fn [taker default-value]
                                      (let [score-moves (conj
                                                          (map
                                                            #(minimax (move state players player-in-turn (first %1) (second %1)) (inc depth) %1 player-in-turn)
                                                            (seq (get-valid-moves board player-in-turn)))
                                                          {:score default-value :move nil})
                                            score-to-take (apply taker (map #(:score %1) score-moves))]
                                        (first (filter #(= score-to-take (:score %1)) score-moves))))]
                      (if (or (= depth stop-depth) (nil? player-in-turn))
                        ; Reached depth limit or in terminal node.
                        {:score (get-score board prev-player)
                         :move  prev-move}
                        (if (maximizing-player? player-in-turn)
                          (debug (take-move max Double/NEGATIVE_INFINITY) "(depth: " depth ") maximizing player")
                          (debug (take-move min Double/POSITIVE_INFINITY) "(depth: " depth ") minimizing player")))))]
      (minimax state 0 nil player))))