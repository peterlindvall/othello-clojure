(ns util.core
  "Assorted utility functions."
  (:use [clojure.test :only (is)]
        [clojure.repl :only (doc)]
        [clojure.walk :only (postwalk)]
        [test.core :only (is=)]
        [clojure.test :only (run-tests)]))

(defn
  #^{:doc  "Works like clojure.core/deref except that if the given value is not something dereferable,
          the value will simply be returned. If the given value is a collection, it will be traversed and dereferenced.
          The option depth argument tells how deep the algorithm should go in the structure.
          Depth of 0 will return the exact object given as argument.
          Default depth is infinity."
     :test (fn []
             (is= (->value "test") "test")
             (is= (->value (atom "test")) "test")
             (is= (->value (ref "test")) "test")
             (is= (->value (agent "test")) "test")
             (is= (->value nil) nil)
             (is= (->value {:test (ref "test")}) {:test "test"})
             (is= (->value {:outer {:test (ref "test")}}) {:outer {:test "test"}})
             (is= (->value {:outer (ref {:test (ref "test")})}) {:outer {:test "test"}})
             (is= (->value (list (atom {:very (ref "nested")}) #{(atom "so") "nested"})) (list {:very "nested"} #{"so" "nested"}))
             (let [inner {:test (ref "test")}
                   outer {:outer (ref inner)}]
               (is= (->value outer 0) outer)
               (is= (->value outer 1) {:outer inner})))}
  ->value
  ([object] (->value object Double/POSITIVE_INFINITY))
  ([object depth]
   (if (= depth 0)
     object
     (postwalk (fn [value]
                 (if (instance? clojure.lang.IDeref value)
                   (->value @value (dec depth))
                   value))
               object))))
(defn
  #^{:doc "Generates a random UUID."}
  uuid []
  (str (java.util.UUID/randomUUID)))

(defn
  #^{:doc
     "Works exactly like clojure.core/print but also returns the value of the first argument.
     Appens a \n character."}
  return-print [& objects]
  (do
    (print (apply str (conj objects "\n")))
    (first objects)))

;; Mutable part of the namespace

(defn add-to-history! [history key identity old new]
  (dosync (alter history conj new)))


