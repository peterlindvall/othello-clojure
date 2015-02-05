(ns othello.core
  (:use [clojure.test :only (deftest is are run-tests)]
        [clojure.repl :only (doc)]))

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
  #^{:doc  "Creates a board from the given string arguments. Nodes can be left out with space characters.
            The dot character cannot be used as player id. The number of players cannot be more than 10."
     :test (fn []
             (is (= (string->board '("W" "B") '("0..1"
                                                 " 0.."
                                                 "111.1"))
                    {[0 0] "W" [1 0] nil [2 0] nil [3 0] "B"
                               [1 1] "W" [2 1] nil [3 1] nil
                     [0 2] "B" [1 2] "B" [2 2] "B" [3 2] nil [4 2] "B"}))
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
  #^{:doc  "A board is created from the string. The player ids must be of length one and excluding the dot character."
     :test (fn []
             (is (simple-string->board ".BW"
                                       " BBW"
                                       "W. B")
                 {[0 0] nil [1 0] "B" [2 0] "W"
                  [1 1] "B" [2 1] "B" [3 1] "W"
                  [0 2] "W" [1 2] nil [3 2] "B"}))}
  simple-string->board [& string-board]
  (string->board '() string-board identity))

(defn
  #^{:doc  "Creates a diamond board for three players."
     :test (fn []
             (is (= (diamond-board "W" "B" "R") (simple-string->board "    .    "
                                                                      "   ...   "
                                                                      "  .....  "
                                                                      " ..WBR.. "
                                                                      "...RWB..."
                                                                      " ..BRW.. "
                                                                      "  .....  "
                                                                      "   ...   "
                                                                      "    .    ")))
             (is (thrown? IllegalArgumentException (diamond-board "R" "G")))
             (is (thrown? IllegalArgumentException (diamond-board "R" "G" "B" "A"))))}
  diamond-board [& players]
  (do
    (when (not= (count players) 3) (throw (IllegalArgumentException. "There must be three players")))
    (string->board players '("    ."
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
             (let [board (simple-string->board ".W")]
               (is (true? (contains-coordinate? board 0 0)))
               (is (false? (contains-coordinate? board 1 1)))))}
  contains-coordinate? [board x y]
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
  (do
    (when (not (contains-coordinate? board x y)) (throw (IllegalArgumentException. "The board does not contain the given coordinate.")))
    (if (not= (get board [x y]) ".")
      (get board [x y]))))

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
             (let [board (simple-string->board ".W."
                                               " BWB"
                                               "..B."
                                               "W")]
               (is (= (board->string board) ".W. \n BWB\n..B.\nW   \n"))))}
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
               (is (false? (marked? board 0 0)))
               (is (true? (marked? board 1 0)))
               (is (thrown? IllegalArgumentException (marked? board 1 1)))))}
  marked? [board x y]
  (let [occupant? (get-occupant board x y)]
    (not (nil? occupant?))))

(defn-
  #^{:doc  "A new board is returned where the given player now is ocuupying the coordinate [x y]."
     :test (fn []
             (let [board (simple-string->board ".W."
                                               "BW.")]
               (is (= (mark board "X" 0 0)
                      (simple-string->board "XW."
                                            "BW.")))
               (is (= (mark board "X" 1 0)
                      (simple-string->board ".X."
                                            "BW.")))
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
               (is (= (move-in-direction board "B" 1 0 1 0)
                      (simple-string->board "..BBB"
                                            "..BB."
                                            ".....")))
               (is (= board (move-in-direction board "B" 0 0 1 0)))
               (is (= board (move-in-direction board "B" 0 0 -1 0)))
               (is (= (move-in-direction board "W" 1 2 1 -1)
                      (simple-string->board "..WWB"
                                            "..WB."
                                            ".....")))
               (is (thrown? IllegalArgumentException (move-in-direction board "W" 5 0 -1 0)))))}
  move-in-direction [board player x y dx dy]
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
    (if (contains-coordinate? board x y)
      (cond
        (= new-board nil) board
        :else new-board)
      (throw (IllegalArgumentException. "The board does not contain the given coordinate.")))))

(defn
  #^{:doc  "Returns a new board with the given move into account."
     :test (fn []
             (let [board (simple-string->board ".BW."
                                               "BB.."
                                               "BBB."
                                               "WWWW")]
               (is (= (move board "W" 0 0) (simple-string->board "WWW."
                                                                 "WW.."
                                                                 "WBW."
                                                                 "WWWW")))
               (is (= (move board "B" 3 0) (simple-string->board ".BBB"
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
             (let [board (simple-string->board "..BW")]
               (is (true? (has-valid-move board "W")))
               (is (false? (has-valid-move board "B")))))}
  has-valid-move [board player]
  (not (nil? (some
               #(and
                 (not (marked? board (first %) (second %)))
                 (try
                   (when (move board player (first %) (second %)) true)
                   (catch Exception e false)))
               (keys board)))))

(defn
  #^{:doc  "Returns the next player in turn."
     :test (fn []
             (let [board (simple-string->board ".WB"
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
               (is (= (history->string history board->string)
                      ".BWBW.\n..BBW.\n\nWWWBW.\n..BBW.\n\nWWWBW.\n..BBBB\n"))))}
  history->string [history item->string]
  (clojure.string/join "\n"
    (for [item history]
      (item->string item))))

; The mutable part of the namespace
; The model

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
(defn create-game! [a-board the-players]
  (do
    (reset! players the-players)
    (dosync
      (ref-set board a-board)
      (ref-set player-in-turn (first the-players)))
    nil))

(defn move! [player x y]
  (do
    (when (not= player @player-in-turn)
      (throw (IllegalArgumentException. "The player is not in turn.")))
    (dosync
      (alter board #(move % player x y))
      (alter player-in-turn #(next-player-in-turn @board @players %))
      nil)))

(defn undo!
  ([] (undo! 1))
  ([number-of-moves]
    (do
      (when (< (dec (count @board-history)) number-of-moves)
        (throw (IllegalArgumentException. "You can not undo, the history contains to few moves.")))
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
