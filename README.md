[![Build Status](https://travis-ci.org/tomas81508/othello-clojure.svg?branch=master)](https://travis-ci.org/tomas81508/othello-clojure)

# othello-clojure
Laborations on othello in Clojure.

We want to use pure functions as much as possible, that is functions completely determined by the (immutable) input
values.

We have a focus on tests being strongly connected to the functionality it tests. In order to get tests in the same
modularity as the functions being tested, they are written as metadata to the functions. In this way the tests will be a
complement to the documentation.

Many tests uses our own syntax (is= actual expected message). This is mainly due to the fact that we often mis-wrote the
notation (is (= actual expected) message) and that clojure is not giving us indications of this. If we mis-wrote a
test it never failed and thus gave us a false indication of what functionality was tested.
