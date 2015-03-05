(ns tutorial.core
  (:use [othello.core])
  (:use [clojure.repl :only (doc)]))

;; Creating the board
(def board (simple-string->board "...."
                                 ".WB."
                                 ".BW."
                                 "...."))

;; Showing the board
board

(contains-coordinate? board 0 0)
(contains-coordinate? board 5 0)

;; A string representation of the board
(doc board->string)
(board->string board)
(print (board->string board))

(marked? board 0 0)
(marked? board 1 1)

(doc move-on-board)
(print (board->string (move-on-board board "B" 1 0)))
(print (board->string board))

(valid-board-move? board "W" 0 0)
(valid-board-move? board "W" 2 0)

(has-valid-move board "W")
(has-valid-move board "R")

(next-player-in-turn board '("W" "B") "W")
(next-player-in-turn board '("W" "R" "B") "W")

;; A state is determined by the board and who is in turn
(def state {:board board :player-in-turn "W"})

state
(print (state->string state))

(move state '("W" "B") "B" 1 0)
(print (state->string (move state '("W" "B") "W" 2 0)))
(print (state->string state))
