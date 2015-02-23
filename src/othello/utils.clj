(ns othello.utils
  "Assorted utility functions. Might be moved out from Othello package later."
  (:use [clojure.test :only (deftest is are run-tests)]
        [clojure.repl :only (doc)]))

(defn
  #^{:doc
           "Works like clojure.core/deref except that if the given value is not something dereferencable,
           the value will simply be returned."
     :test (fn []
             (is (= (soft-deref "test") "test"))
             (is (= (soft-deref (atom "test")) "test"))
             (is (= (soft-deref (ref "test")) "test"))
             (is (= (soft-deref (agent "test")) "test"))
             (is (= (soft-deref nil) nil)))}
  soft-deref
  [value]
  (if (or
        (instance? clojure.lang.Ref value)
        (instance? clojure.lang.Atom value)
        (instance? clojure.lang.Agent value))
    @value
    value))

(defn
  #^{:doc
           "Compares if two maps are equal by value. Dereferences all dereferencable containers for their values
           before comparing. So this is like a softer version of the closure clojure.core/= function."
     :test (fn []
             (is (not (maps-equal? {:test "test" :wrong "key"} {:test "test" :another "key"})))
             (is (not (maps-equal? {:value (ref "value")} {:value "other"})))
             (is (maps-equal? {:test (ref "test")} {:test "test"}))
             (is (maps-equal? {:outer {:test (ref "test")}} {:outer {:test "test"}}))
             (is (maps-equal? {:outer (ref {:test (ref "test")})} {:outer {:test "test"}})))
     }
  maps-equal?
  [map1 map2]
  (and
    (= (count map1) (count map2))
    (reduce (fn [equal key]
              (let [value1 (soft-deref (get map1 key))
                    value2 (soft-deref (get map2 key))]
                (and
                  equal
                  (if (instance? clojure.lang.PersistentArrayMap value1)
                    (recur value1 value2)
                    (= value1 value2))
                  ))) true (keys map1))))

(defn
  #^{:doc "Generates a random UUID."}
  uuid
  []
  (str (java.util.UUID/randomUUID)))

