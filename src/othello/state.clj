(ns othello.state
  "Namespace for holding all Othello games in memory state."
  (:require [othello.core :as othello]))

; The model

(def games (atom {}))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn create-game! [board players]
  (let [game {:board (ref board)
              :players players
              :player-in-turn (ref (first players))
              :id (uuid)}]
    (swap! games assoc (:id game) game)))

(defn get-games []
  @games)

;
; Old stuff below.
;


(defn add-to-history [history key identity old new]
  (dosync
    (alter history conj new)))


; The board with a history added
(def board (ref ()))
(def board-history (ref ()))
(add-watch board :history (partial add-to-history board-history))

; The player in turn with a history added
(def player-in-turn (ref ()))
(def player-in-turn-history (ref ()))
(add-watch player-in-turn :history (partial add-to-history player-in-turn-history))

(def players (atom ()))

; Mutating the model
; TODO: Write some kind of (integration) tests for these methods
; TODO: How to get better names in the API but still use good names for the refs?
(comment (defn create-game! [a-board the-players]
           (do
             (reset! players the-players)
             (dosync
               (ref-set board a-board)
               (ref-set player-in-turn (first the-players)))
             nil)))

(defn move! [player x y]
  (do
    (when (not= player @player-in-turn)
      (throw (IllegalArgumentException. "The player is not in turn.")))
    (dosync
      (alter board #(othello/move % player x y))
      (alter player-in-turn #(othello/next-player-in-turn @board @players %))
      nil)))

(defn undo!
  ([] (undo! 1))
  ([number-of-moves]
    (do
      (when (< (dec (count @board-history)) number-of-moves)
        (throw (IllegalArgumentException. "You can not undo, the history contains too few moves.")))
      (dosync
        (ref-set board (nth @board-history number-of-moves))
        (alter board-history (partial drop (inc number-of-moves)))
        (ref-set player-in-turn (nth @player-in-turn-history number-of-moves))
        (alter player-in-turn-history (partial drop (inc number-of-moves)))
        nil))))




; For REPL use
;(create-game! (square-board 4 '("W" "B")) '("W" "B"))
;(print (board->string @board))
;(print (history->string @board-history board->string))
;(move! "W" 0 2)
;@player-in-turn
;@player-in-turn-history
;(move! "B" 2 3)
;(move! "W" 3 2)
;(print (history->string @board-history board->string))
;(print (board->string @board))
;(undo!)
;(move! "B" 0 1)
;(move! "W" 0 0)
;(move! "B" 1 0)
;(move! "W" 2 0)
;(move! "B" 3 0)
;(move! "W" 3 1)
;(move! "B" 3 3)
;(move! "W" 1 3)
;(move! "B" 0 3)
;(print (board->string @board))
