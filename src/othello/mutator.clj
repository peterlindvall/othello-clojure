(ns othello.mutator
  "Namespace for operations on the state."
  (:require [othello.core :as core]
            [othello.move-strategy :as move-strategy])
  (:use [clojure.test :only (deftest is are run-tests)]
        [clojure.repl :only (doc)]
        [util.core :only (uuid ->value add-to-history!)]
        [test.core :only (is=)]
        [othello.board :only (square-board)]))


(defn-
  #^{:doc  "Checks if a game with the given id exists."
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
  #^{:doc  "Removes the game with given id from the games container."
     :test (fn []
             (is= (remove-game {"game1" {}} "game1") {})
             (is= (remove-game {"game1" {} "game2" {:test "test"}} "game1") {"game2" {:test "test"}})
             (is (thrown? IllegalArgumentException (remove-game {} "game1"))))}
  remove-game [games id]
  (do
    (when (not (contains-game? games id)) (throw (IllegalArgumentException. "Game with given id does not exist.")))
    (dissoc games id)))


(defn create-player
  ([id color] (create-player id color "HUMAN" nil))
  ([id color type strategy]
   (do
     (when-not (or (= type "HUMAN") (= type "COMPUTER"))
       (throw (IllegalArgumentException. "Wrong type. Must be HUMAN or COMPUTER.")))
     (when (and (= type "COMPUTER") (nil? strategy))
       (throw (IllegalArgumentException. "Computers must have a strategy.")))
     (when (and (= type "HUMAN") (not (nil? strategy)))
       (throw (IllegalArgumentException. "Humans have their own strategies.")))
     (let [base {:id    id
                 :color color
                 :type  type}]
       (if (= type "COMPUTER")
         (assoc base :strategy (atom strategy))
         base)))))


(defn
  #^{:doc "Returns the current state of the game."}
  get-state [game]
  ;Since only states is a derefable, tell ->value to stop at depth 1.
  ;Not using @ since given game might be a value object (with no derefables in it).
  (last (->value (:states game) 1)))


;;------------------------------
;; Functions mutating states
;;------------------------------

(defn
  #^{:doc "Creates a game with the given board, players and id and adds it to the games container."}
  new-game!
  ([games board players] (new-game! games board players (uuid)))
  ([games board players id]
   (let [game (create-game board players id)]
     (swap! games add-new-game game)
     (:id game))))


(defn
  #^{:doc "Removes the game with given id."}
  remove-game! [games id]
  (do
    (if
      (contains-game? @games id)
      (swap! games remove-game id))
    nil))


(defn
  #^{:doc "Returns the ids of the games in the games container."}
  list-games [games]
  (keys @games))


(defn
  #^{:doc "Returns an immutable representation of the game with the given id."}
  get-game [games id]
  (->value (get @games id)))


(defn
  #^{:doc
     "Makes a move on the game with the given id.
     If strategy given, it must have the signature [state players]."}
  move!
  ([games id player x y]
   (move! games id player (fn [& _] [x y])))
  ([games id player strategy]
   (when-not (contains-game? @games id) (throw (IllegalArgumentException. "There is no game with the given id.")))
   (let [game (get @games id)
         players (:players game)
         state (get-state game)
         states (:states game)]
     (when (not= (:player-in-turn state) player)
       (throw (IllegalArgumentException. "The player is not in turn.")))
     (swap! states #(conj %1
                          (let [coordinate (strategy (last %1) players)
                                x (first coordinate)
                                y (second coordinate)]
                            (core/move (last %1) (:players game) player x y)))))))


(defn undo!
  #^{:doc "Undoes the given number of moves at the game with given id."}
  [games id number-of-moves]
  (let [game (get @games id)
        states (:states game)]
    (when (< (dec (count @states)) number-of-moves)
      (throw (IllegalArgumentException. "You can not undo, the history contains too few moves.")))
    (swap! states (partial drop-last number-of-moves))
    nil))


;;------------------------------------
;; Integration tests of this namespace
;;------------------------------------

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
                       (:board (get-state (get @games id)))))]
    (new-game! games board players "1")
    (new-game! games board players "2")
    (new-game! games board players "3")
    (new-game! games board players "4")
    ;; On board 1
    (move! games "1" "B" 0 1)
    (move! games "1" "W" 2 0)
    (move! games "1" "B" 3 1)
    ;; On board 2 with a strategy
    (move! games "2" "B" move-strategy/upper-left-strategy)
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
  (let [players '("W" "B")
        board-size 8
        board (othello.board/square-board board-size players)
        games (atom {})
        game-id "1"
        play-until-none-is-in-turn (fn [games game-id player thread-id]
                                     (if (not (nil? (:player-in-turn (get-state (get @games game-id)))))
                                       (do
                                         (try
                                           (move! games game-id player move-strategy/upper-left-strategy)
                                           (catch Exception e "Retrying"))
                                         (recur games game-id player thread-id))))]
    (new-game! games board players game-id)
    (time
      (do
        (def futures [(future (play-until-none-is-in-turn games game-id (first players) "A"))
                      (future (play-until-none-is-in-turn games game-id (first players) "B"))
                      (future (play-until-none-is-in-turn games game-id (first players) "C"))
                      (future (play-until-none-is-in-turn games game-id (second players) "D"))])
        (doseq [f futures] (deref f 1000 "Stopped!\n"))))
    (is= (:board (get-state (get @games game-id)))
         (core/simple-string->board "WWWWWWWB"
                                    "WWWBBWWB"
                                    "WWWWWBWB"
                                    "WWWBWWBB"
                                    "WWWWBWBB"
                                    "WWWWWBWB"
                                    "WWWWWWBB"
                                    "BBBBBBBB"))))
