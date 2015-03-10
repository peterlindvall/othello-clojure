(ns othello.move-strategy
  "Move strategies for anyone who needs them."
  (:require [othello.core :as core])
  (:use [test.core :only (is=)]))

(defn
  #^{:doc  "A move strategy."
     :test (fn []
             (let [board (core/simple-string->board "...."
                                                    ".WB."
                                                    ".BW."
                                                    "....")]
               (is= (upper-left-strategy board "W") [0 2])
               (is= (upper-left-strategy board "B") [0 1])))}
  upper-left-strategy [board player]
  (first
    (filter
      #(othello.core/valid-board-move? board player (first %1) (second %1))
      (sort (keys board)))))

