(ns molodtsov.core
  (:use
   [clojure.test :refer [deftest run-tests is]]
   )
  )

(defn main
  "I don't do a whole lot."
  []
  (println "Hello, World!")
  (run-tests 'molodtsov.core)
  )

(deftest test1
  (is (= (+ 2 3) 5)))
