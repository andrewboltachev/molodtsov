(ns molodtsov.core
  (:use
   [clojure.test :refer [deftest run-tests is]]
   )
  )

;; utils
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

;; constants
(def russian-letters-small "абвгдеёжзийклмнопрстуфхцчшщъыьэюя")

(def komi-letters-small (str russian-letters-small "іӧ"))

(def komi-and-molodtsov-letters-small (str komi-letters-small "ԁјҗԇԃԅԉԋԍԏ"))

(def komi-and-molodtsov-letters
  (str komi-and-molodtsov-letters-small
       (clojure.string/upper-case komi-and-molodtsov-letters-small)))

;; functions
(defn o2m [word]
  (let [word (re-sub "([дзлнст])и" (fn [_ a] (str a "ьи")) word)]
    word))

;; stuff
(defn convert-string [string f]
  (let [pattern (re-pattern (str "[" komi-and-molodtsov-letters "]+"
                                      "|"
                                      "[^" komi-and-molodtsov-letters "]+"))
        tokens (re-seq pattern string)
        tokens-converted (map (fn [token]
                                (if (re-matches pattern token)
                                  (f token)
                                  token)
                                ) tokens)]
    (clojure.string/join tokens)))

(defn main
  "I don't do a whole lot."
  []
  ;(println "Hello, World!")
  (run-tests 'molodtsov.core)
  (let [s "Тані ті верманныд пӧртны коми гижӧдъяс Молодцов гижанногысь ӧнія гижанногӧ да мӧдарӧ."]
    (println
      (convert-string s o2m))
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
