(ns tutorial.mutator
  (:use [othello.core])
  (:use [othello.mutator])
  (:use [clojure.repl :only (doc)]))

;; Creating the board
(def board (simple-string->board "...."
                                 ".WB."
                                 ".BW."
                                 "...."))

(print (board->string board))

(def games (atom {}))

(new-game! games board (list "W" "B"))

(list-games games)

(move! games "1" "W" 2 0)

(print (state->string (:state (get-game games "1"))))



