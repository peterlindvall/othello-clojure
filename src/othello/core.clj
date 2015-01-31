(ns othello.core
  (:use [clojure.test :only (is are run-tests)]))

(defn
  #^{:doc  "Creates a square board of the given even size for two players. "
     :test (fn []
             (is (= (square-board 4 ["W" "B"])
                    {[0 0] "." [1 0] "." [2 0] "." [3 0] "."
                     [0 1] "." [1 1] "W" [2 1] "B" [3 1] "."
                     [0 2] "." [1 2] "B" [2 2] "W" [3 2] "."
                     [0 3] "." [1 3] "." [2 3] "." [3 3] "."}))
             (is (thrown? IllegalArgumentException (square-board 5)))
             (is (thrown? IllegalArgumentException (square-board 6 ["W" "B" "R"]))))}
  square-board [board-size players]
  (do
    (when (odd? board-size) (throw (IllegalArgumentException. "board-size must be even")))
    (when (not= (count players) 2) (throw (IllegalArgumentException. "there must be two players")))
    (into {} (for [y (range board-size)
                   x (range board-size)]
               (let [middle+ (/ board-size 2)
                     middle- (- middle+ 1)]
                 (cond
                   (or
                     (and (= x middle-) (= y middle-))
                     (and (= x middle+) (= y middle+))) [[x y] (first players)]
                   (or
                     (and (= x middle-) (= y middle+))
                     (and (= x middle+) (= y middle-))) [[x y] (second players)]
                   :else [[x y] "."]))))))

(defn-
  #^{:doc "Acquire the occupant player id at the given coordinate."
     :test (fn []
             (let [board {[0 0] "." [1 0] "W" [2 0] "."
                          [0 1] "B" [1 1] "W" [2 1] "W"}]
               (is (= (get-occupant board 0 1) "B"))
               (is (= (get-occupant board 2 0) "."))
               (is (= (get-occupant board 3 3) nil))))}
  get-occupant [board x y]
  (get board [x y]))

(defn-
  #^{:doc  "Returns the max value of all coordinates n-th position"
     :test (fn []
             (let [board {[0 0] "." [1 3] "." [7 2] "."}]
               (is (max-coordinate board 0) 7)
               (is (max-coordinate board 1) 3)))}
  max-coordinate [board n]
  (apply max (map #(nth % n) (keys board))))

(defn-
  #^{:doc  "Creates a board from the given string arguments."
     :test (fn []
             (is (= (string-to-board "W..B" " W.." "BBB.")
                    {[0 0] "W" [1 0] "." [2 0] "." [3 0] "B"
                     [1 1] "W" [2 1] "." [3 1] "."
                     [0 2] "B" [1 2] "B" [2 2] "B" [3 2] "."})))}
  string-to-board [& string-board]
  ; ([0 ([0 \W] [1 \.] [2 \.] [3 \B])] [1 ([0 \W] [1 \W] [2 \.] [3 \.])] [2 ([0 \B] [1 \B] [2 \B] [3 \.])])
  (into {} (for [row (map-indexed vector (map #(map-indexed vector (seq %)) string-board))
                 :let [y (first row)]
                 x-and-occupant (second row)
                 :let [x (first x-and-occupant)
                       occupant (str (second x-and-occupant))]]
             (when (not= " " occupant)
               [[x y] occupant]))))

(defn
  #^{:doc  "A nice string version of the board"
     :test (fn []
             (let [board (string-to-board ".W."
                                          "BW "
                                          "W  ")]
               (is (board-to-string board) ".W.\nBW\nW")))}
  board-to-string [board]
  (let [max-x (max-coordinate board 0)
        max-y (max-coordinate board 1)]
    (for
      [x (range (inc max-x))
       y (range (inc max-y))
       :let [occupant (get-occupant board x y)]]
      (cond
        (and (nil? occupant) (= max-y y)) " \n"
        (nil? occupant) " "
        (= max-y y) (str occupant "\n")
        :else occupant))))

(defn-
  #^{:doc  "Returns true if a player is occupying the node at the given coordinates"
     :test (fn []
             (let [board (string-to-board ".WB")]
               (is (false? (is-marked board 0 0))
               (is (true? (is-marked board 1 0))))))}
  is-marked [board x y]
  (let [occupant? (get-occupant board x y)]
    (not= "." occupant?)))

(defn-
  #^{:doc  "If the the board is not marked at the position [x y] then the board is updated with the player marked at [x y]"
     :test (fn []
             (let [board
                   (string-to-board ".W."
                                    "BW.")]
               (is (update-board board "X" 0 0)
                   (string-to-board "XW."
                                    "BW."))))}
  update-board [board player x y]
  (update-in board [[x y]] (fn [arg] player)))