(ns util.core
  "Assorted utility functions."
  (:use [clojure.test :only (is)]
        [clojure.repl :only (doc)]
        [clojure.walk :only (postwalk)]
        [test.core :only (is=)]))

(defn
  #^{:doc "Works like clojure.core/deref except that if the given value is not something dereferable,
          the value will simply be returned. If the given value is a collection, it will be traversed and dereferenced."
     :test (fn []
             (is= (->value "test") "test")
             (is= (->value (atom "test")) "test")
             (is= (->value (ref "test")) "test")
             (is= (->value (agent "test")) "test")
             (is= (->value nil) nil)
             (is= (->value {:test (ref "test")}) {:test "test"})
             (is= (->value {:outer {:test (ref "test")}}) {:outer {:test "test"}})
             (is= (->value {:outer (ref {:test (ref "test")})}) {:outer {:test "test"}})
             (is= (->value (list (atom {:very (ref "nested")}) #{(atom "so") "nested"})) (list {:very "nested"} #{"so" "nested"})))}
  ->value [object]
  (postwalk (fn [value]
              (if (instance? clojure.lang.IDeref value)
                (->value @value)
                value))
            object))
(defn
  #^{:doc "Generates a random UUID."}
  uuid []
  (str (java.util.UUID/randomUUID)))

;; Mutable part of the namespace

(defn add-to-history! [history key identity old new]
  (dosync (alter history conj new)))


