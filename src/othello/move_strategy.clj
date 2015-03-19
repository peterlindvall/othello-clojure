(ns othello.move-strategy
  "Move strategies for anyone who needs them."
  (:require [othello.core :as core]
            [othello.board :as board])
  (:use [test.core :only (is=)]
        [clojure.test :only (deftest testing)]
        util.core))

(defn
  #^{:doc  "A move strategy."
     :test (fn []
             (let [board (core/simple-string->board "...."
                                                    ".WB."
                                                    ".BW."
                                                    "....")]
               (is= (upper-left-strategy {:board board :player-in-turn "W"}) [0 2])
               (is= (upper-left-strategy {:board board :player-in-turn "B"}) [0 1])))}
  upper-left-strategy
  ([state] (upper-left-strategy state nil))
  ([state _]
   (let [board (:board state)
         player (:player-in-turn state)]
     (first
       (filter
         #(othello.core/valid-board-move? board player (first %1) (second %1))
         (sort (keys board)))))))


;;TODO: The score heuristic does not count the opponent score (which might make the player lose because the own score is maximized but the opponent score is still higher).
;;TODO: How does it handle multiple players?
;;TODO: Speedup with the alpha-beta pruning?
(defn
  #^{:doc
           "Minimax algorithm as described here: http://en.wikipedia.org/wiki/Minimax.
           Will make an optimal move for the player-in-turn of the given state.
           Looks ahead in time (by given depth argument) and makes the move that results in the highest score for the player.
           This should theoretically be the best AI possible for an Othello game.
           Assumes that the opponent is playing in an optimal way to maximize the opponent score."
     :test (fn []
             (let [board (core/simple-string->board "..BBWB.."
                                                    "....B..."
                                                    "........")
                   board-end (core/simple-string->board "BBBBBB.."
                                                        "....B..."
                                                        "....WWW.")
                   state {:board board :player-in-turn "W"}
                   state-end {:board board-end :player-in-turn nil}]
               (comment (is= (minimax-move-strategy state '("W" "B") 10) [4 2]))
               (is= (minimax-move-strategy state-end '("W" "B") 10) nil)))}
  minimax-move-strategy
  ([state players] (minimax-move-strategy state players Double/POSITIVE_INFINITY))
  ([state players depth]
   (let [stop-depth depth
         ; The player that we want to play as good as possible for.
         ; Also called the maximizing player (we want to maximize the score).
         player (:player-in-turn state)
         maximizing-player? (partial = player)
         ; Measures how good the board is for the maximizing player.
         ; The higher value the better.
         score-heuristic (fn [board player] (core/get-score board player))
         ; The actual minimax algorithm.
         ; Will only return the best score heuristic value of all moves possible by the maximizing player.
         ; The actual move that resulted in the score will not be returned.
         minimax (fn minimax [state depth]
                   (let [board (:board state)
                         player-in-turn (:player-in-turn state)
                         ; Evaluates all valid moves for the player in turn and returns the move defined by the taker function argument (min/max in this algorithm).
                         take-move (fn [taker]
                                     (let [free-nodes (count (filter #(nil? (second %1)) board))
                                           map-fn (resolve (if (> free-nodes 6)
                                                             'pmap
                                                             'map))]
                                       (apply taker
                                              (map-fn
                                                #(minimax (core/move state players player-in-turn (first %1) (second %1)) (inc depth))
                                                (core/get-valid-moves board player-in-turn)))))]
                     (if (or (= depth stop-depth) (core/game-over? state))
                       ; Reached depth limit or in terminal node.
                       ; Evaluate the board with the score heuristic.
                       (score-heuristic board player)
                       (if (maximizing-player? player-in-turn)
                         ; The player that we want to play as good as possible for.
                         (take-move max)
                         ; The opponent (that we assume will play as good as possible.)
                         ; This is why we want to minimize the possible score the opponent can achieve.
                         (take-move min)))))]
     (if (core/game-over? state)
       nil
       ; Since the maximizing algorithm only returns the best possible score value, we need to keep track of the first moves ourselves.
       ; By calling the maximizing algorithm on all possible moves that the player has for the beginning state,
       ; and then picking the move that resulted in the highest score we have picked the best move possible.
       (let [score-moves (pmap
                           #(do {:score (minimax (core/move state players player (first %1) (second %1)) 1)
                                 :move  %1})
                           (core/get-valid-moves (:board state) player))
             max-score (apply max (map #(:score %1) score-moves))]
         (:move (first (filter #(= max-score (:score %1)) score-moves))))))))

;;------------------------------------
;; Integration tests of this namespace
;;------------------------------------

(deftest minimax-move-strategy-test
  (testing "Soundness"
    (let [initial-board (core/simple-string->board "..BBWB.."
                                                   "....B..."
                                                   "........")
          players '("W" "B")
          states (atom [{:board initial-board :player-in-turn "W"}])
          move (fn [state depth]
                 (let [coord (minimax-move-strategy state players depth)]
                   (if (nil? coord)
                     nil
                     (core/move state players (:player-in-turn state) (first coord) (second coord)))))]
      (do
        (time (while (not (core/game-over? (last @states)))
                (let [current-state (last @states)
                      new-state (move current-state 10)]
                  (reset! states (conj @states new-state)))))
        ; Uncomment this row to print the states.
        ;(print (apply str (map #(str (core/state->string %1)) @states)))
        (let [last-state (last @states)
              last-board (:board last-state)]
          (is= (core/get-score last-board "W") 3)
          (is= (core/get-score last-board "B") 7)))))
  (testing "Performance"
    (let [players ["W" "B"]
          initial-board (core/simple-string->board "...."
                                                   ".WB."
                                                   ".BW."
                                                   "....")
          state {:board initial-board :player-in-turn "W"}]
      (time (minimax-move-strategy state players 5)))))