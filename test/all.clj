(ns all
  (:use clojure.test))

(deftest all
  (let [namespaces '(
                      "othello.board"
                      "othello.core"
                      "othello.move-strategy"
                      "othello.mutator"
                      "othello.state"
                      "test.core"
                      "util.core"
                      )]
    (apply require (map symbol namespaces))
    (is (successful? (apply run-tests (map symbol namespaces))))))