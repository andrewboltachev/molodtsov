(ns molodtsov.core
  (:use
   [clojure.test :refer [deftest run-tests is]]
   )
  )

(defn re-sub [pattern f string]
  (let [all-nil? (fn [xs] (not-any? (comp not nil?) xs))
        rest-nil? (fn [xs] (all-nil? (rest xs)))
        full-pattern (re-pattern (str pattern "|.+?"))
        tokens (re-seq full-pattern string)
        ]
    (clojure.string/join (map (fn [token]
                                (if (sequential? token)
                                  (if (rest-nil? token)
                                    (first token)
                                    (apply f token))
                                  (if
                                    (re-matches pattern token)
                                    (f token)
                                    token))
                                ) tokens))))

(defn o2m [word]
  (let [word 1]
    word))

(defn main
  "I don't do a whole lot."
  []
  ;(println "Hello, World!")
  (run-tests 'molodtsov.core)
  (println
    ""
    ))

(deftest test1
  (is (= (+ 2 3) 5)))


(deftest test-re-sub-simple
  (is (=
       ; ...
       (re-sub "и" (fn [x] (if (= x "и") "і" x)) "аэиоу")
       "аэіоу"
       )))

(deftest test-re-sub-simple
  (is (=
       ; ...
       (re-sub #"([дзлнст])([еёюя])" (fn [_ a b] (str a "ь" ({"е" "э"
                                                          "ё" "о"
                                                          "ю" "у"
                                                          "я" "а"} b))) "дядя вася тётя зина")
       "дьадьа васьа тьотьа зина"
       )))
