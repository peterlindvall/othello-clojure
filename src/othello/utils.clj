(ns othello.utils
  "Assorted utility functions. Might be moved out from Othello package later."
  (:use [clojure.test :only (deftest is are run-tests)]
        [clojure.repl :only (doc)]
        [clojure.walk :only (postwalk)]
        [test.core :only (is=)]))

(defn
  #^{:doc
           "Works like clojure.core/deref except that if the given value is not something dereferable,
           the value will simply be returned. If the given value is a collection, it will be traversed and dereferenced."
     :test (fn []
             (is= (soft-deref "test") "test")
             (is= (soft-deref (atom "test")) "test")
             (is= (soft-deref (ref "test")) "test")
             (is= (soft-deref (agent "test")) "test")
             (is= (soft-deref nil) nil)
             (is= (soft-deref {:test (ref "test")}) {:test "test"})
             (is= (soft-deref {:outer {:test (ref "test")}}) {:outer {:test "test"}})
             (is= (soft-deref {:outer (ref {:test (ref "test")})}) {:outer {:test "test"}})
             (is= (soft-deref (list (atom {:very (ref "nested")}) #{(atom "so") "nested"})) (list {:very "nested"} #{"so" "nested"})))}
  soft-deref
  [coll]
  (postwalk (fn
              [value]
              (if (instance? clojure.lang.IDeref value)
                (soft-deref @value)
                value)) coll))
(defn
  #^{:doc "Generates a random UUID."}
  uuid
  []
  (str (java.util.UUID/randomUUID)))

