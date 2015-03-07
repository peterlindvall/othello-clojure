(ns othello.mutator
  "Namespace for holding all Othello games in memory state."
  (:require [othello.core :as core])
  (:use [clojure.test :only (deftest is are run-tests)]
        [clojure.repl :only (doc)]
        [util.core :only (uuid ->value add-to-history!)]
        [test.core :only (is=)]
        [othello.board :only (square-board)]))

(defn-
  #^{:doc  "Checks if a given game id exist is included in the games."
     :test (fn []
             (is (contains-game? {"game1" {}} "game1"))
             (is (not (contains-game? {"game1" {}} "game2"))))}
  contains-game? [games id]
  (contains? games id))

(defn-
  #^{:doc "Creates a game consisting of the given board, players and id.
  The states vector represents the history of all states, with the last state being the current state.
  The first player in the players list will be the first one to play."}
  create-game [board players id]
  (let [states (atom [])]
    (reset! states [{:board board :player-in-turn (first players)}])
    {:states  states
     :players players
     :id      id}))

(defn
  #^{:doc "Returns the current state of the game."}
  get-state [game]
  (last (->value (:states game))))

(defn-
  #^{:doc  "Adds the given game to the games container."
     :test (fn []
             (let [game1 (create-game (core/simple-string->board "WB") ["W" "B"] "game1")
                   game2 (create-game (core/simple-string->board ".BWB") ["B" "W"] "game2")]
               (is= (->value (add-new-game {} game1)) (->value {"game1" game1}))
               (is= (->value (add-new-game {"game1" game1} game2)) (->value {"game1" game1 "game2" game2}))
               (is (thrown? IllegalArgumentException (add-new-game {"game1" {}} game1)))))}
  add-new-game [games game]
  (do
    (when (contains-game? games (game :id)) (throw (IllegalArgumentException. "Game with given id already exist.")))
    (assoc games (:id game) game)))

(defn-
  #^{:doc  "Removes the gave with given id from the games collection."
     :test (fn []
             (is= (remove-game {"game1" {}} "game1") {})
             (is= (remove-game {"game1" {} "game2" {:test "test"}} "game1") {"game2" {:test "test"}})
             (is (thrown? IllegalArgumentException (remove-game {} "game1"))))}
  remove-game [games id]
  (do
    (when (not (contains-game? games id)) (throw (IllegalArgumentException. "Game with given id does not exist.")))
    (dissoc games id)))

;; Mutable part of the namespace

(defn
  #^{:doc "Creates a game from the given board, players and id and adds it to the games container."}
  new-game!
  ([games board players] (new-game! games board players (uuid)))
  ([games board players id]
    (let [game (create-game board players id)]
      (swap! games add-new-game game)
      (:id game))))

(defn
  #^{:doc "Removes the gave with given id."}
  remove-game! [games id]
  (do
    (if
      (contains-game? @games id)
      (swap! games remove-game id))
    nil))


(defn
  #^{:doc "Gets the ids of all games."}
  list-games [games]
  (keys @games))


(defn
  #^{:doc "Returns an immutable representation of the game with the given id."}
  get-game [games id]
  (->value (get @games id)))


(defn
  #^{:doc "Makes a move on the game with the given id."}
  move!
  ([games id player x y]
  (do
    (when-not (contains-game? @games id) (throw (IllegalArgumentException. "There is no game with the given id.")))
    (let [game (get @games id)
          state (get-state game)
          states (:states game)]
      (when-not (core/valid-board-move? (:board state) player x y)
        (throw (IllegalArgumentException. "Can not move at that position.")))
      (when (not= (:player-in-turn state) player)
        (throw (IllegalArgumentException. "The player is not in turn.")))
      (swap! states #(conj %1 (core/move (last %1) (:players game) player x y))))))
  ([games id player strategy]
    (when-not (contains-game? @games id) (throw (IllegalArgumentException. "There is no game with the given id.")))
    (let [game (get @games id)
          state (get-state game)
          states (:states game)]
      (when (not= (:player-in-turn state) player)
        (throw (IllegalArgumentException. "The player is not in turn.")))
      (swap! states #(conj %1
                           (let [coordinate (strategy (:board (get-state (get-game games id))) player)
                                 x (first coordinate)
                                 y (second coordinate)]
                             (core/move (last %1) (:players game) player x y)))))))





(defn undo!
  #^{:doc "Undo the given number of moves at the game with given id."}
  [games id number-of-moves]
  (let [game (get @games id)
        states (:states game)]
    (when (< (dec (count @states)) number-of-moves)
      (throw (IllegalArgumentException. "You can not undo, the history contains too few moves.")))
    (swap! states (partial drop-last number-of-moves))
    nil))

;; A move strategy

(defn- upper-left-strategy [board player]
  (first
    (filter
      #(othello.core/valid-board-move? board player (first %1) (second %1))
      (sort (keys board)))))

;; Integration tests of this namespace

(deftest games-scenario
  (let [board (core/simple-string->board "...."
                                         ".WB."
                                         ".BW."
                                         "....")
        players '("B" "W")
        games (atom {})
        our-assert (fn [id & expected-board-as-string]
                     (is=
                       (apply core/simple-string->board expected-board-as-string)
                       (:board (get-state (get-game games id)))))]
    (new-game! games board players "1")
    (new-game! games board players "2")
    (new-game! games board players "3")
    (new-game! games board players "4")
    ;; On board 1
    (move! games "1" "B" 0 1)
    (move! games "1" "W" 2 0)
    (move! games "1" "B" 3 1)
    ;; On board 2 with a strategy
    (move! games "2" "B" upper-left-strategy)
    ;; On board 3
    (move! games "3" "B" 0 1)
    (undo! games "3" 1)
    ;; On board 4
    (remove-game! games "4")
    ;; Asserts
    (our-assert "1"
                "..W."
                "BBBB"
                ".BW."
                "....")
    (our-assert "2"
                "...."
                "BBB."
                ".BW."
                "....")
    (our-assert "3"
                "...."
                ".WB."
                ".BW."
                "....")
    (is (not (contains-game? @games "4")))))


(deftest parallelism
  (let [players                    '("W" "B")
        board-size                 8
        board                      (othello.board/square-board board-size players)
        games                      (atom {})
        game-id                    "1"
        play-until-none-is-in-turn (fn [games game-id player thread-id]
                                     (if (not (nil? (:player-in-turn (get-state (get-game games game-id)))))
                                       (do
                                         (try
                                           (move! games game-id player upper-left-strategy)
                                           (catch Exception e "Retrying"))
                                         (recur games game-id player thread-id))))]
    (new-game! games board players game-id)
    (def futures [(future (play-until-none-is-in-turn games game-id (first players) "A"))
                  (future (play-until-none-is-in-turn games game-id (first players) "B"))
                  (future (play-until-none-is-in-turn games game-id (first players) "C"))
                  (future (play-until-none-is-in-turn games game-id (second players) "D"))])
    (doseq [f futures] (deref f 1000 "Stopped!\n"))
    (is= (:board (get-state (get-game games game-id)))
         (core/simple-string->board "WWWWWWWB"
                                    "WWWBBWWB"
                                    "WWWWWBWB"
                                    "WWWBWWBB"
                                    "WWWWBWBB"
                                    "WWWWWBWB"
                                    "WWWWWWBB"
                                    "BBBBBBBB"))))
