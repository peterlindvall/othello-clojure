(ns othello.move-strategy
  "Move strategies for anyone who needs them."
  (:require [othello.core :as core])
  (:use [test.core :only (is=)]
        [clojure.test :only (deftest)]
        util.core))

(defn
  #^{:doc  "A move strategy."
     :test (fn []
             (let [board (core/simple-string->board "...."
                                                    ".WB."
                                                    ".BW."
                                                    "....")]
               (is= (upper-left-strategy board "W") [0 2])
               (is= (upper-left-strategy board "B") [0 1])))}
  upper-left-strategy [board player]
  (first
    (filter
      #(othello.core/valid-board-move? board player (first %1) (second %1))
      (sort (keys board)))))

(defn- get-score [board player]
  (count (filter (partial = player) (vals board))))

;;TODO: The score heuristic does not count the opponent score (which might make the player lose because the own score is maximized but the opponent score is still higher).
;;TODO: How does it handle multiple players?
;;TODO: Speedup with the alpha-beta pruning?
;;TODO: Do the subtrees in parallel?
(defn
  #^{:doc
           "Minimax algorithm as described here: http://en.wikipedia.org/wiki/Minimax.
           Looks ahead in time (by given depth argument) and makes the move that results in the highest score for the player.
           This should theoretically be the best AI possible for an Othello game."
     :test (fn []
             (let [board (core/simple-string->board "..BBWB.."
                                                    "....B..."
                                                    "........")
                   board-end (core/simple-string->board "BBBBBB.."
                                                        "....B..."
                                                        "....WWW.")
                   state {:board board :player-in-turn "W"}
                   state-end {:board board-end :player-in-turn nil}]
               (comment (is= (:move (minimax-move-strategy state '("W" "B") 10)) [4 2]))
               (is= (:move (minimax-move-strategy state-end '("W" "B") 10)) nil)))}
  minimax-move-strategy
  ([state players] (minimax-move-strategy state players Double/POSITIVE_INFINITY))
  ([state players depth]
   (let [stop-depth depth
         player (:player-in-turn state)
         maximizing-player? (partial = player)
         minimax (fn minimax [state depth]
                   (let [board (:board state)
                         player-in-turn (:player-in-turn state)
                         take-move (fn [taker default-value]
                                     (apply taker (conj
                                                    (map
                                                      #(minimax (core/move state players player-in-turn (first %1) (second %1)) (inc depth))
                                                      (seq (core/get-valid-moves board player-in-turn)))
                                                    default-value)))]
                     (if (or (= depth stop-depth) (core/game-over? state))
                       ; Reached depth limit or in terminal node.
                       (get-score board player)
                       (if (maximizing-player? player-in-turn)
                         (take-move max Double/NEGATIVE_INFINITY)
                         (take-move min Double/POSITIVE_INFINITY)))))]
     (if (core/game-over? state)
       nil
       (let [score-moves (map
                           #(do {:score (minimax (core/move state players player (first %1) (second %1)) 1)
                                 :move  %1})
                           (seq (core/get-valid-moves (:board state) player)))
             max-score (apply max (map #(:score %1) score-moves))]
         (first (filter #(= max-score (:score %1)) score-moves)))))))

;;------------------------------------
;; Integration tests of this namespace
;;------------------------------------

(deftest minimax-move-strategy-test
  (let [initial-board (core/simple-string->board "..BBWB.."
                                                 "....B..."
                                                 "........")
        players '("W" "B")
        states (atom [{:board initial-board :player-in-turn "W"}])
        move (fn [state depth]
               (let [coord (:move (minimax-move-strategy state players depth))]
                 (if (nil? coord)
                   nil
                   (core/move state players (:player-in-turn state) (first coord) (second coord)))))]
    (do
      (while (not (core/game-over? (last @states)))
        (let [current-state (last @states)
              new-state (move current-state 10)]
          (reset! states (conj @states new-state))))
      (comment (print (apply str (map #(str (core/state->string %1)) @states))))
      (let [last-state (last @states)
            last-board (:board last-state)]
        (is= (get-score last-board "W") 3)
        (is= (get-score last-board "B") 7)))))