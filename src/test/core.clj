(ns test.core
  (:use [clojure.test :only (is)]))

(defmacro is=
  ([actual expected] `(is= ~actual ~expected nil))
  ([actual expected message]
    `(let [equal# (= ~actual ~expected)]
       (do
         (comment (when-not equal#
                    (println "Actual:" ~actual "\nExpected:" ~expected)))
         (is (= ~actual ~expected) ~message)))))