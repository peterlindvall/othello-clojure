(ns othello.state
  "Namespace for holding all Othello games in memory state."
  (:require [othello.mutator :as mutator]))

(def games (atom {}))

(defn
  #^{:doc "Creates a game from the given board, players and id and adds it to the games container."}
  new-game! [board players]
  (mutator/new-game! games board players))

(defn
  #^{:doc "Removes the gave with given id."}
  remove-game! [id]
  (mutator/remove-game! games id))

(defn
  #^{:doc "Gets the ids of all games."}
  list-games []
  (mutator/list-games games))

(defn
  #^{:doc "Makes a move on the game with the given id."}
  move! [id player x y]
  (mutator/move! games id player x y))

(defn undo!
  ([id] (undo! id 1))
  ([id number-of-moves]
     (mutator/undo! games id number-of-moves)))

(defn get-game [id]
  (mutator/get-game games id))