(ns othello.core
  (:use [clojure.test :only (is are run-tests)]))

(defn
  #^{:doc  "Creates a square board of the given even size for two players."
     :test (fn []
             (is (= (square-board 4 ["W" "B"])
                    {[0 0] nil [1 0] nil [2 0] nil [3 0] nil
                     [0 1] nil [1 1] "W" [2 1] "B" [3 1] nil
                     [0 2] nil [1 2] "B" [2 2] "W" [3 2] nil
                     [0 3] nil [1 3] nil [2 3] nil [3 3] nil}))
             (is (thrown? IllegalArgumentException (square-board 5 ["W" "B"])))
             (is (thrown? IllegalArgumentException (square-board 6 ["W" "B" "R"]))))}
  square-board [board-size players]
    (do
      (when (odd? board-size) (throw (IllegalArgumentException. "board-size must be even")))
      (when (not= (count players) 2) (throw (IllegalArgumentException. "there must be two players")))
      (let [middle+ (/ board-size 2)
            middle- (- middle+ 1)]
        (into {} (for [y (range board-size)
                       x (range board-size)]
                   (cond
                     (or
                       (and (= x middle-) (= y middle-))
                       (and (= x middle+) (= y middle+))) [[x y] (first players)]
                     (or
                       (and (= x middle-) (= y middle+))
                       (and (= x middle+) (= y middle-))) [[x y] (second players)]
                     :else [[x y] nil]))))))

(defn
  #^{:doc  "Creates a board from the given string arguments. Nodes can be left out with space characters."
     :test (fn []
             (is (= (string-to-board '("W" "B") '("0..1" " 0.." "111.1"))
                    {[0 0] "W" [1 0] nil [2 0] nil [3 0] "B"
                     [1 1] "W" [2 1] nil [3 1] nil
                     [0 2] "B" [1 2] "B" [2 2] "B" [3 2] nil [4 2] "B"})))}
  string-to-board
  ([players string-board]
    (let [map-strategy (fn [value] (nth players (read-string value)))]
      (string-to-board players string-board map-strategy)))
  ([players string-board map-strategy]
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
                   [[x y] (map-strategy occupant)]))))))

(defn
  #^{:doc  "A board is created from the string. The player ids must be of length one and excluding the dot character."
     :test (fn []
             (is (simple-string-to-board ".BW"
                                         " BBW"
                                         "W. B")
                 {[0 0] nil [1 0] "B" [2 0] "W"
                  [1 1] "B" [2 1] "B" [3 1] "W"
                  [0 2] "W" [1 2] nil [3 2] "B"}))}
  simple-string-to-board [& string-board]
  (string-to-board '() string-board identity))

(defn
  #^{:doc  "Creates a diamond board for three players."
     :test (fn []
             (is (= (diamond-board "W" "B" "R") (simple-string-to-board "    .    "
                                                                        "   ...   "
                                                                        "  .....  "
                                                                        " ..WBR.. "
                                                                        "...RWB..."
                                                                        " ..BRW.. "
                                                                        "  .....  "
                                                                        "   ...   "
                                                                        "    .    "))))}
  diamond-board [& players]
  (do
    (when (not= (count players)) (throw IllegalArgumentException "There must be three players"))
    (string-to-board players '("    ."
                               "   ..."
                               "  ....."
                               " ..012.."
                               "...201..."
                               " ..120.."
                               "  ....."
                               "   ..."
                               "    ."))))

(defn
  #^{:doc  "Determines if the given board contains the given coordinate."
     :test (fn []
             (let [board (simple-string-to-board ".W")]
              (is (true? (contains-coordinate board 0 0)))
              (is (false? (contains-coordinate board 1 1)))))}
  contains-coordinate [board x y]
  (contains? board [x y]))

(defn
  #^{:doc  "Acquire the occupant player id at the given coordinate."
     :test (fn []
             (let [board {[0 0] "." [1 0] "W" [2 0] "."
                          [0 1] "B" [1 1] "W" [2 1] "W"}]
               (is (= (get-occupant board 0 1) "B"))
               (is (= (get-occupant board 2 0) nil))
               (is (thrown? IllegalArgumentException (get-occupant board 3 3)))))}
  get-occupant [board x y]
  (cond
    (and (contains? board [x y]) (not= (get board [x y]) ".")) (get board [x y])
    (contains? board [x y]) nil
    :else (throw (IllegalArgumentException. "The board does not contain the given coordinate."))))

(defn-
  #^{:doc  "Returns the max value of all coordinates n-th position"
     :test (fn []
             (let [board {[0 0] "." [1 3] "." [7 2] "."}]
               (is (max-coordinate board 0) 7)
               (is (max-coordinate board 1) 3)))}
  max-coordinate [board n]
  (apply max (map #(nth % n) (keys board))))

(defn
  #^{:doc  "A nice string version of the board."
     :test (fn []
             (let [board (simple-string-to-board ".W."
                                                 " BWB"
                                                 "W")]
               (is (= (board-to-string board) ".W. \n BWB\nW   \n"))))}
  board-to-string [board]
  (let [max-x (max-coordinate board 0)
        max-y (max-coordinate board 1)]
    (apply str (for
                 [y (range (inc max-y))
                  x (range (inc max-x))
                  :let [max-x? (= x max-x)
                        contains-coordinate? (contains-coordinate board x y)]]
                 (cond
                   (and (not contains-coordinate?) max-x?) " \n"
                   (not contains-coordinate?) " "
                   (nil? (get-occupant board x y)) "."
                   (= max-x x) (str (get-occupant board x y) "\n")
                   :else (get-occupant board x y))))))

(defn
  #^{:doc  "Returns true if a player is occupying the node at the given coordinates."
     :test (fn []
             (let [board (simple-string-to-board ".WB")]
               (is (false? (is-marked board 0 0)))
               (is (true? (is-marked board 1 0)))))}
  is-marked [board x y]
  (let [occupant? (get-occupant board x y)]
    (not (nil? occupant?))))

(defn-
  #^{:doc  "If the the board is not marked at the position [x y] then the board is updated with the player marked at [x y]"
     :test (fn []
             (let [board (simple-string-to-board ".W."
                                                 "BW.")]
               (is (= (mark board "X" 0 0)
                      (simple-string-to-board "XW."
                                              "BW.")))
               (is (thrown? IllegalArgumentException (mark board "X" 3 0)))))}
  mark [board player x y]
  (update-in board [[x y]] (fn [node]
                             (if (not (contains-coordinate board x y))
                               (throw (IllegalArgumentException. "The board does not contain the given coordinate."))
                               player))))

(defn-
  #^{:doc  "Returns a new board taken the move by the player in the given direction into account. The coordinate [x y] where the move is made will not be occupied."
     :test (fn []
             (let [board (simple-string-to-board "..WWB"
                                                 "..BB."
                                                 ".....")]
               (is (= (move-in-direction board "B" 1 0 1 0)
                      (simple-string-to-board "..BBB"
                                              "..BB."
                                              ".....")))
               (is (= board (move-in-direction board "B" 0 0 1 0)))
               (is (= board (move-in-direction board "B" 0 0 -1 0)))
               (is (= (move-in-direction board "W" 1 2 1 -1)
                      (simple-string-to-board "..WWB"
                                              "..WB."
                                              ".....")))
               (is (thrown? IllegalArgumentException (move-in-direction board "W" 5 0 -1 0)))))}
  move-in-direction [board player x y dx dy]
  (let [swap-pieces-in-direction (fn [board player x y dx dy]
                                   (let [new-x (+ x dx)
                                         new-y (+ y dy)
                                         contains-coordinate? (contains-coordinate board new-x new-y)]
                                     (cond
                                       (not contains-coordinate?) nil
                                       (nil? (get-occupant board new-x new-y)) nil
                                       (= player (get-occupant board new-x new-y)) board
                                       :else (recur (mark board player new-x new-y) player new-x new-y dx dy))))
        new-board (swap-pieces-in-direction board player x y dx dy)]
    (if (contains-coordinate board x y)
      (cond
        (= new-board nil) board
        :else new-board)
      (throw (IllegalArgumentException. "The board does not contain the given coordinate.")))))

(defn
  #^{:doc  "Returns a new board with the given move into account."
     :test (fn []
             (let [board (simple-string-to-board ".BW."
                                                 "BB.."
                                                 "BBB."
                                                 "WWWW")]
               (is (= (move board "W" 0 0) (simple-string-to-board "WWW."
                                                                   "WW.."
                                                                   "WBW."
                                                                   "WWWW")))
               (is (= (move board "B" 3 0) (simple-string-to-board ".BBB"
                                                                   "BB.."
                                                                   "BBB."
                                                                   "WWWW")))
               (is (thrown? IllegalArgumentException (move board "B" 0 0)))
               (is (thrown? IllegalArgumentException (move board "B" 1 0)))))}
  move [board player x y]
  (let [directions '([-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1])
        new-board (reduce #(move-in-direction %1 player x y (first %2) (second %2)) board directions)]
    (if (not= board new-board)
      (mark new-board player x y)
      (throw (IllegalArgumentException. "Can not move at that coordinate")))))

(defn
  #^{:doc  "Determines if the given player has a valid move."
     :test (fn []
             (let [board (simple-string-to-board "..BW")]
               (is (true? (has-valid-move board "W")))
               (is (false? (has-valid-move board "B")))))}
  has-valid-move [board player]
  (if (some
        #(and
          (not (is-marked board (first %) (second %)))
          (try
            (when (move board player (first %) (second %)) true)
            (catch Exception e false)))
            ;(not= board (move board player (first %) (second %))))
        (keys board))
    true
    false))

(defn
  #^{:doc  "Returns the next player in turn."
     :test (fn []
             (let [board (simple-string-to-board ".WB"
                                                 "WWW"
                                                 "OOO")
                   players '("W" "B" "O")]
               (is (= (next-player-in-turn board players "W") "B"))
               (is (= (next-player-in-turn board players "B") "O"))
               (is (= (next-player-in-turn board players "O") "B"))))}
  next-player-in-turn [board players current]
  (let [current-index (.indexOf players current)
        get-next-index (fn [index]
                         (if (= (inc index) (count players))
                           0
                           (inc index)))]
    (loop [next-index (get-next-index current-index)]
      (let [next-player (nth players next-index)]
        (cond
          (has-valid-move board next-player) next-player
          :else (recur (get-next-index next-index)))))))
