(ns othello.boards
  "Collection of functions for creating different types of predesigned Othello boards."
  (require [othello.core :as othello])
  (:use [clojure.test :only (is)]
        [test.core :only (is=)]))

(defn
  #^{:doc  "Creates a square board of the given even size for two players."
     :test (fn []
             (is= (square-board 4 ["W" "B"])
                  {[0 0] nil [1 0] nil [2 0] nil [3 0] nil
                   [0 1] nil [1 1] "W" [2 1] "B" [3 1] nil
                   [0 2] nil [1 2] "B" [2 2] "W" [3 2] nil
                   [0 3] nil [1 3] nil [2 3] nil [3 3] nil})
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
  #^{:doc  "Creates a diamond board for three players."
     :test (fn []
             (is= (diamond-board "W" "B" "R") (othello/simple-string->board "    .    "
                                                                            "   ...   "
                                                                            "  .....  "
                                                                            " ..WBR.. "
                                                                            "...RWB..."
                                                                            " ..BRW.. "
                                                                            "  .....  "
                                                                            "   ...   "
                                                                            "    .    "))
             (is (thrown? IllegalArgumentException (diamond-board "R" "G")))
             (is (thrown? IllegalArgumentException (diamond-board "R" "G" "B" "A"))))}
  diamond-board [& players]
  (do
    (when (not= (count players) 3) (throw (IllegalArgumentException. "There must be three players")))
    (othello/string->board players '("    ."
                                      "   ..."
                                      "  ....."
                                      " ..012.."
                                      "...201..."
                                      " ..120.."
                                      "  ....."
                                      "   ..."
                                      "    ."))))