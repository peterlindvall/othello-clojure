(ns othello.state
  "Namespace for holding all Othello games in memory state."
  (:require [othello.core :as othello])
  (:use [clojure.test :only (deftest is are run-tests)]
        [clojure.repl :only (doc)]
        [othello.utils :only (uuid maps-equal?)]))

; The model

(def games (atom {}))

; ==== Games collection manipulation ====

(defn- add-to-history! [history key identity old new]
  (swap! history conj new))

(defn-
  #^{:doc "Checks if a given game id exist in the games collection."
     :test (fn []
             (is (contains-game? {"game1" {}} "game1"))
             (is (not (contains-game? {"game1" {}} "game2"))))}
  contains-game? [games id]
  (contains? games id))

(defn-
  #^{:doc  "Creates a game consisting of the given board, players and id.
            The first player in the players list will be the first one to play."}
  create-game [board players id]
  (let [state (atom {})
        state-history (atom '())]
    (do
      (add-watch state :history (partial add-to-history! state-history))
      (reset! state {:board board :player-in-turn (first players)})
      {:state   state
       :players players
       :id      id})))

(defn-
  #^{:doc  "Adds the given game to the games container."
     :test (fn []
             (let [game1 (create-game (othello/simple-string->board "WB") ["W" "B"] "game1")
                   game2 (create-game (othello/simple-string->board ".BWB") ["B" "W"] "game2")]
               (is (maps-equal? (add-new-game {} game1) {"game1" game1}))
               (is (maps-equal? (add-new-game {:game1 game1} game2) {"game1" game1 "game2" game2}))
               (is (thrown? IllegalArgumentException (add-new-game {"game1" {}} game1)))))}
  add-new-game [games game]
  (do
    (when (contains-game? games (game :id)) (throw (IllegalArgumentException. "Game with given id already exist.")))
    (assoc games (:id game) game)))

(defn-
  #^{:doc "Removes the gave with given id from the games collection."
     :test (fn []
             (is (= (remove-game {"game1" {}} "game1") {}))
             (is (= (remove-game {"game1" {} "game2" {:test "test"}} "game1") {"game2" {:test "test"}}))
             (is (thrown? IllegalArgumentException (remove-game {} "game1"))))}
  remove-game [games id]
  (do
    (when (not (contains-game? games id)) (throw (IllegalArgumentException. "Game with given id does not exist.")))
    (dissoc games id)))

(defn
  #^{:doc "Creates a game from the given board, players and id and adds it to the games container."}
  new-game! [board players]
  (let [game (create-game board players (uuid))]
    (swap! games add-new-game game)))

(defn
  #^{:doc "Removes the gave with given id."}
  remove-game! [id]
  (do
    (if (contains-game? @games id)
      (swap! remove-game id))
    nil))

(defn-
  #^{:doc "Gets the ids of all games."}
  list-games []
  (keys @games))

; ==== Game manipulation ====

; TODO

;
; Old stuff below.
;


; The board with a history added
(def board (ref ()))
(def board-history (ref ()))
(add-watch board :history (partial add-to-history! board-history))

; The player in turn with a history added
(def player-in-turn (ref ()))
(def player-in-turn-history (ref ()))
(add-watch player-in-turn :history (partial add-to-history! player-in-turn-history))

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