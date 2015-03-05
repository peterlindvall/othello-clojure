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

(new-game! games board (list "W" "B") "1")

(list-games games)

(move! games "1" "W" 2 0)

(print (state->string (get-state (get-game games "1"))))

(move! games "1" "B" 1 0)

(print (state->string (get-state (get-game games "1"))))

(undo! games "1" 1)

(print (state->string (get-state (get-game games "1"))))


