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
  (let [
        word (re-sub "([дзлнст])и" (fn [_ a] (str a "ьи")) word)
        word (clojure.string/replace word #"і" "и")
        word (clojure.string/replace word #"ддж" "дждж")
        word (clojure.string/replace word #"ддз" "дздз")
        word (clojure.string/replace word #"ттш" "тштш")
        word (re-sub "([бвгжйкмпрчш])е" (fn [_ a] (str a "э")) word) ; й ?
        word (re-sub #"([дзлнст])([еёюя])" (fn [_ a b] (str a "ь" ({"е" "э"
                                                          "ё" "о"
                                                          "ю" "у"
                                                          "я" "а"} b))) word)
    ]
    word))

(defn word-caps-mode [word]
  (let [count-pred (fn [pred coll]
                     (count (filter pred coll)))
        is-upper-char? (fn [x] (= (str x) (clojure.string/upper-case (str x))))
        upper-letters (count-pred is-upper-char? word)
        lower-letters (count-pred (comp not is-upper-char?) word)]
    ; ...
    (cond
      (> upper-letters lower-letters)
      2
      (and (= upper-letters 1) (>= (count word) 1) (is-upper-char? (first word)))
      1
      :else
      0)))

;; stuff
(defn convert-string [string f]
  (let [pattern (re-pattern (str "[" komi-and-molodtsov-letters "]+"
                                      "|"
                                      "[^" komi-and-molodtsov-letters "]+"))
        tokens (re-seq pattern string)
        tokens-converted (map (fn [token]
                                (if (re-matches pattern token)
                                  (let [
                                    converted (f token)]
                                    )
                                  token)
                                ) tokens)]
    (clojure.string/join tokens-converted)))

(defn main
  "I don't do a whole lot."
  []
  ;(println "Hello, World!")
  (run-tests 'molodtsov.core)
  (let [s "Тані ті верманныд пӧртны коми гижӧдъяс Молодцов гижанногысь ӧнія гижанногӧ да мӧдарӧ. Дерт жӧ. Дядя Вася Тётя Зина."]
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


(deftest test-word-caps-mode-0
  (is (= (word-caps-mode "абу") 0)))

(deftest test-word-caps-mode-2
  (is (= (word-caps-mode "аБУ") 2)))

(deftest test-word-caps-mode-0a
  (is (= (word-caps-mode "абУ") 0)))

(deftest test-word-caps-mode-1
  (is (= (word-caps-mode "Абу") 1)))
