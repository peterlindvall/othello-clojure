(ns othello.core
  "A collection of pure functions for handling Othello games."
  (:use [clojure.test :only (is run-tests)]
        [clojure.repl :only (doc)]
        [test.core :only (is=)]))

(defn
  #^{:doc  "Creates a board from the given string arguments. Nodes can be left out with space characters.
            The dot character cannot be used as player id. There can be up to 10 players."
     :test (fn []
             (is= (string->board '("W" "B") (list "0..1"
                                                  " 0.."
                                                  "111.1"))
                  {[0 0] "W" [1 0] nil [2 0] nil [3 0] "B"
                   [1 1] "W" [2 1] nil [3 1] nil
                   [0 2] "B" [1 2] "B" [2 2] "B" [3 2] nil [4 2] "B"})
             (is (thrown? IllegalArgumentException (string->board (range 11) nil)))
             (is (thrown? IllegalArgumentException (string->board '(".") nil))))}
  string->board
  ([players string-board]
    (let [map-strategy (fn [value] (nth players (read-string value)))]
      (string->board players string-board map-strategy)))
  ([players string-board map-strategy]
    (do
      (when (> (count players) 10) (throw (IllegalArgumentException. "number of players cannot be more than 10")))
      (when (some #(= "." %) players) (throw (IllegalArgumentException. "the dot character cannot be used as player id")))
      (let
        [indexed-elements (fn [seq] (map-indexed vector seq))]
        ; ([0 ([0 \W] [1 \.] [2 \.] [3 \B])] [1 ([0 \W] [1 \W] [2 \.] [3 \.])] [2 ([0 \B] [1 \B] [2 \B] [3 \.])])
        (into {} (for [row (indexed-elements (map indexed-elements string-board))
                       :let [y (first row)]
                       x-and-occupant (second row)
                       :let [x (first x-and-occupant)
                             occupant (str (second x-and-occupant))]]
                   (condp = occupant
                     " " ()
                     "." [[x y] nil]
                     [[x y] (map-strategy occupant)])))))))

(defn
  #^{:doc  "A board is created from the string. The player ids must be of length one. The dot character cannot be used as player id."
     :test (fn []
             (is= (simple-string->board ".BW"
                                        " BBW"
                                        "W. B")
                  {[0 0] nil [1 0] "B" [2 0] "W"
                   [1 1] "B" [2 1] "B" [3 1] "W"
                   [0 2] "W" [1 2] nil [3 2] "B"}))}
  simple-string->board [& string-board]
  (string->board '() string-board identity))

(defn
  #^{:doc  "Determines if the given board contains the given coordinate."
     :test (fn []
             (let [board (simple-string->board ".W")]
               (is (contains-coordinate? board 0 0))
               (is (not (contains-coordinate? board 1 1)))))}
  contains-coordinate? [board x y]
  (contains? board [x y]))

(defn
  #^{:doc  "Returns the id for the player occupying the given coordinate."
     :test (fn []
             (let [board {[0 0] "." [1 0] "W" [2 0] "."
                          [0 1] "B" [1 1] "W" [2 1] "W"}]
               (is= (get-occupant board 0 1) "B")
               (is= (get-occupant board 2 0) nil)
               (is (thrown? IllegalArgumentException (get-occupant board 3 3)))))}
  get-occupant [board x y]
  (do
    (when (not (contains-coordinate? board x y)) (throw (IllegalArgumentException. "The board does not contain the given coordinate.")))
    (if (not= (get board [x y]) ".")
      (get board [x y]))))

(defn-
  #^{:doc  "Returns the max value of all coordinates n-th position"
     :test (fn []
             (let [board {[0 0] "." [1 3] "." [7 2] "."}]
               (is= (max-coordinate board 0) 7)
               (is= (max-coordinate board 1) 3)))}
  max-coordinate [board n]
  (apply max (map #(nth % n) (keys board))))

(defn
  #^{:doc  "Returns nice string version of the board."
     :test (fn []
             (let [board (simple-string->board ".W."
                                               " BWB"
                                               "..B."
                                               "W")]
               (is= (board->string board) ".W. \n BWB\n..B.\nW   \n")))}
  board->string [board]
  (let [max-x (max-coordinate board 0)
        max-y (max-coordinate board 1)]
    (apply str (for
                 [y (range (inc max-y))
                  x (range (inc max-x))
                  :let [max-x? (= x max-x)
                        contains-coordinate? (contains-coordinate? board x y)]]
                 (cond
                   (and (not contains-coordinate?) max-x?) " \n"
                   (not contains-coordinate?) " "
                   (and max-x? (nil? (get-occupant board x y))) ".\n"
                   (nil? (get-occupant board x y)) "."
                   (= max-x x) (str (get-occupant board x y) "\n")
                   :else (get-occupant board x y))))))

(defn
  #^{:doc  "Returns true if a player is occupying the node at the given coordinates."
     :test (fn []
             (let [board (simple-string->board ".WB")]
               (is (not (marked? board 0 0)))
               (is (marked? board 1 0))
               (is (thrown? IllegalArgumentException (marked? board 1 1)))))}
  marked? [board x y]
  (let [occupant? (get-occupant board x y)]
    (not (nil? occupant?))))

(defn-
  #^{:doc  "A new board is returned where the given player now is ocuupying the coordinate [x y]."
     :test (fn []
             (let [board (simple-string->board ".W."
                                               "BW.")]
               (is= (mark board "X" 0 0)
                    (simple-string->board "XW."
                                          "BW."))
               (is= (mark board "X" 1 0)
                    (simple-string->board ".X."
                                          "BW."))
               (is (thrown? IllegalArgumentException (mark board "X" 3 0)))))}
  mark [board player x y]
  (do
    (when (not (contains-coordinate? board x y)) (throw (IllegalArgumentException. "The board does not contain the given coordinate.")))
    (assoc-in board [[x y]] player)))

(defn-
  #^{:doc  "Returns a new board taken the move by the player in the given direction into account. The coordinate [x y] where the move is made will not be occupied."
     :test (fn []
             (let [board (simple-string->board "..WWB"
                                               "..BB."
                                               ".....")]
               (is= (move-in-direction board "B" 1 0 1 0)
                    (simple-string->board "..BBB"
                                          "..BB."
                                          "....."))
               (is= (move-in-direction board "B" 0 0 1 0) board)
               (is= (move-in-direction board "B" 0 0 -1 0) board)
               (is= (move-in-direction board "W" 1 2 1 -1)
                    (simple-string->board "..WWB"
                                          "..WB."
                                          "....."))
               (is (thrown? IllegalArgumentException (move-in-direction board "W" 5 0 -1 0)))))}
  move-in-direction [board player x y dx dy]
  (do
    (when (not (contains-coordinate? board x y)) (throw (IllegalArgumentException. "The board does not contain the given coordinate.")))
    (let [swap-pieces-in-direction (fn [board player x y dx dy]
                                     (let [new-x (+ x dx)
                                           new-y (+ y dy)
                                           contains-coordinate? (contains-coordinate? board new-x new-y)]
                                       (cond
                                         (not contains-coordinate?) nil
                                         (nil? (get-occupant board new-x new-y)) nil
                                         (= player (get-occupant board new-x new-y)) board
                                         :else (recur (mark board player new-x new-y) player new-x new-y dx dy))))
          new-board (swap-pieces-in-direction board player x y dx dy)]
      (or new-board board))))

(defn
  #^{:doc  "Returns a new board with the given move into account."
     :test (fn []
             (let [board (simple-string->board ".BW."
                                               "BB.."
                                               "BBB."
                                               "WWWW")]
               (is= (move-on-board board "W" 0 0) (simple-string->board "WWW."
                                                                        "WW.."
                                                                        "WBW."
                                                                        "WWWW"))
               (is= (move-on-board board "B" 3 0) (simple-string->board ".BBB"
                                                                        "BB.."
                                                                        "BBB."
                                                                        "WWWW"))
               (is (thrown? IllegalArgumentException (move-on-board board "B" 0 0)))
               (is (thrown? IllegalArgumentException (move-on-board board "B" 1 0)))))}
  move-on-board [board player x y]
  (let [directions '([-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1])
        new-board (reduce #(move-in-direction %1 player x y (first %2) (second %2)) board directions)]
    (if (not= board new-board)
      (mark new-board player x y)
      (throw (IllegalArgumentException. "Can not move at that coordinate")))))

(defn
  #^{:doc  "Determines if a move on the given board is valid."
     :test (fn []
             (let [board (simple-string->board "..WB")]
               (is (valid-board-move? board "B" 1 0))
               (is (not (valid-board-move? board "B" 0 0)))
               (is (not (valid-board-move? board "B" 2 0)))))}
  valid-board-move? [board player x y]
  (and
    (not (marked? board x y))
    (try
      (when (move-on-board board player x y) true)
      (catch Exception e false))))

(defn
  #^{:doc  "Determines if the given player has a valid move on the given board."
     :test (fn []
             (let [board (simple-string->board "..BW")]
               (is (has-valid-move board "W"))
               (is (not (has-valid-move board "B")))))}
  has-valid-move [board player]
  (not (nil? (some
               #(valid-board-move? board player (first %) (second %))
               (keys board)))))

(defn
  #^{:doc  "Returns the next player in turn. The current-player is the player that already made a move and the board is
            the board after the move."
     :test (fn []
             (let [board (simple-string->board ".WB"
                                               "WWW"
                                               "OOO")
                   players '("W" "B" "O")]
               (is= (next-player-in-turn board players "W") "B")
               (is= (next-player-in-turn board players "B") "O")
               (is= (next-player-in-turn board players "O") "B"))
             (is= (next-player-in-turn (simple-string->board ".WWWWB.") '("W" "B") "W") "B")
             (is (nil? (next-player-in-turn (simple-string->board "WWW") '("W" "B") "W"))))}
  next-player-in-turn [board players current-player]
  (let [original-index (.indexOf players current-player)
        get-next-index #(mod (inc %1) (count players))]
    (loop [next-index (get-next-index original-index)]
      (let [next-player (nth players next-index)]
        (cond
          (has-valid-move board next-player) next-player
          (= next-index original-index) nil
          :else (recur (get-next-index next-index)))))))

(defn
  #^{:doc  "Returns a new state with a new board and a new player-in-turn."
     :test (fn []
             (let [board (simple-string->board "..BBWB.")
                   state {:board board :player-in-turn "W"},
                   players '("W" "B")]
               (is= (move state players "W" 1 0)
                    {:board (simple-string->board ".WWWWB.") :player-in-turn "B"})
               (is (thrown? IllegalArgumentException (move state players "W" 3 0)))))}
  move [state players player x y]
  (do
    (when (not= (:player-in-turn state) player) (throw (IllegalArgumentException. "The player is not in turn.")))
    (let [board-after-move (move-on-board (:board state) player x y)]
      {:board          board-after-move
       :player-in-turn (next-player-in-turn board-after-move players player)})))

(defn
  #^{:doc  "Merges a history of items into a string."
     :test (fn []
             (let [history (list
                             (simple-string->board ".BWBW."
                                                   "..BBW.")
                             (simple-string->board "WWWBW."
                                                   "..BBW.")
                             (simple-string->board "WWWBW."
                                                   "..BBBB"))]
               (is= (history->string history board->string)
                    ".BWBW.\n..BBW.\n\nWWWBW.\n..BBW.\n\nWWWBW.\n..BBBB\n")))}
  history->string [history item->string]
  (clojure.string/join "\n" (for [item history] (item->string item))))

(defn
  #^{:doc  "Returns a string representation of the state."
     :test (fn []
             (is= (state->string
                    {:board (simple-string->board ".WBO."
                                                  "OBWWW")
                     :player-in-turn "O"})
                  (str ".WBO.\n"
                       "OBWWW\n"
                       "Player in turn: O\n")))}
  state->string [state]
  (str
    (board->string (:board state))
    "Player in turn: "
    (:player-in-turn state)
    "\n"))
