(ns tutorial.ai
  (:use [othello.core]
        [othello.mutator])
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

;;TODO: The score heuristic does not count the opponent score (which might make the player lose because the own score is maximized but the opponent score is still higher).
(defn
  #^{:test (fn []
             (let [board (simple-string->board "..BBWB.."
                                               "....B..."
                                               "........")
                   board-end (simple-string->board "BBBBBB.."
                                                   "....B..."
                                                   "....WWW.")
                   state {:board board :player-in-turn "W"}
                   state-end {:board board-end :player-in-turn nil}]
               (comment (is= (:move (minimax-move-strategy state '("W" "B") 2)) [4 2]))
               (is= (:move (minimax-move-strategy state-end '("W" "B") 2)) nil)))}
  minimax-move-strategy
  ([state players] (minimax-move-strategy state players Double/POSITIVE_INFINITY))
  ([state players depth]
   (let [stop-depth depth
         get-score (fn [board player]
                     (count (filter (partial = player) (vals board))))
         player (:player-in-turn state)
         maximizing-player? (partial = player)
         minimax (fn minimax [state depth]
                   (let [board (:board state)
                         player-in-turn (:player-in-turn state)
                         take-move (fn [taker default-value]
                                     (apply taker (conj
                                                    (map
                                                      #(minimax (move state players player-in-turn (first %1) (second %1)) (inc depth))
                                                      (seq (get-valid-moves board player-in-turn)))
                                                    default-value)))]
                     (if (or (= depth stop-depth) (nil? player-in-turn))
                       ; Reached depth limit or in terminal node.
                       (get-score board player)
                       (if (maximizing-player? player-in-turn)
                         (take-move max Double/NEGATIVE_INFINITY)
                         (take-move min Double/POSITIVE_INFINITY)))))]
     (if (nil? player)
       nil
       (let [score-moves (map
                           #(do {:score (minimax (move state players player (first %1) (second %1)) 1)
                                 :move  %1})
                           (seq (get-valid-moves (:board state) player)))
             max-score (apply max (map #(:score %1) score-moves))]
         (first (filter #(= max-score (:score %1)) score-moves)))))))


;; Creating the board
(def board (simple-string->board "..BBWB.."
                                 "....B..."
                                 "........"))

(def games (atom {}))

(new-game! games board (list "W" "B") "1")

(defn mv [depth]
  (let [state (get-state (get-game games "1"))
        coord (:move (minimax-move-strategy state '("W" "B") depth))]
    (if (nil? (debug coord "\n"))
      false
      (do
        (move! games "1" (:player-in-turn state) (first coord) (second coord))
        true))))

(defn print-game [] (print (state->string (get-state (get-game games "1")))))

(loop [depth 4]
  (when (> depth 0)
    (mv depth)
    (print-game)
    (recur (- depth 1))))

(defn play [depth]
  (do
    (def games (atom {}))
    (new-game! games board (list "W" "B") "1")
    (loop [d depth]
      (when (and (> d 0) (not (nil? (:player-in-turn (get-state (get-game games "1"))))))
        (let [ongoing (mv d)]
          (print-game)
          (if ongoing
            (recur (- d 1))
            (recur 0)))))))

(play 100)

(print-game)

(mv 8)

(print-game)

(mv 7)

(print-game)


(move! games "1" "B" 1 0)

(print (state->string (get-state (get-game games "1"))))

(undo! games "1" 1)

(print (state->string (get-state (get-game games "1"))))