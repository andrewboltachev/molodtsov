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
                                    (if (string? f)
                                      f
                                      (f token))
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
        repl01 (fn [_ a b] (str a "ь" ({"е" "э"
                                        "ё" "о"
                                        "ю" "у"
                                        "я" "а"} b)))
        word (re-sub #"([дзлнст])([еёюя])" repl01 word)
        word (clojure.string/replace word #"дж" "җ")
        word (clojure.string/replace word #"дз" "ԇ")
        word (clojure.string/replace word #"тш" "щ")
        repl02 (fn [_ a] (str ({"е" "йэ"
                                "ё" "йо"
                                "ю" "йу"
                                "я" "йа"} a)))
        word (re-sub "([еёюя])" repl02 word)
        word (clojure.string/replace word #"ъ" "")
        repl03 (fn [_ a] (str ({"д" "ԃ"
                                "з" "ԅ"
                                "л" "ԉ"
                                "н" "ԋ"
                                "с" "ԍ"
                                "т" "ԏ"} a)))
        word (re-sub "([дзлнст])ь" repl03 word)
        word (clojure.string/replace word #"ԇь" "ԇ")
        word (clojure.string/replace word #"э" "е")
        word (clojure.string/replace word #"д" "ԁ")
        word (clojure.string/replace word #"и" "і")
        word (clojure.string/replace word #"й" "ј")
    ]
    word))

(defn m2o [word]
  (let [
        util-iotate {"а" "я"
                     "о" "ё"
                     "у" "ю"}

        util-soft2hard {
                        "ԃ" "д"
                        "ԅ" "з"
                        "ԉ" "л"
                        "ԋ" "н"
                        "ԍ" "с"
                        "ԏ" "т"}
        
        util-first2hard {
                         "ԃԃ" "дԃ"
                         "ԅԅ" "зԅ"
                         "ԉԉ" "лԉ"
                         "ԋԋ" "нԋ"
                         "ԍԍ" "сԍ"
                         "ԏԏ" "тԏ"
                        }

        repl-step2-06 (fn [_ a] (util-first2hard a))

        repl-step3-01 (fn [_ a b] (str a "ъ" (util-iotate b)))
        repl-step3-02 (fn [_ a b] (str a "ъе"))
        repl-step3-05 (fn [_ a b] (str (util-soft2hard a) "ь" (util-iotate b)))
        repl-step3-06 (fn [_ a] (str (util-soft2hard a) "ье"))
        repl-step3-07 (fn [_ a] (str a "і"))
        repl-step3-08 (fn [_ a] (str (util-soft2hard a) "и"))
        repl-step3-09 (fn [_ a b] (str (util-soft2hard a) (util-iotate b)))
        repl-step3-10 (fn [_ a] (str (util-soft2hard a) "е"))
        repl-step3-11 (fn [_ a] (str (util-soft2hard a) "ь"))

        repl-step4-02 (fn [_ a b] (str "чь" (util-iotate b)))
        repl-step4-04 (fn [_ a b] (str "дзь" (util-iotate b)))
        repl-step5-01 (fn [_ a] (util-iotate a))
        repl-step6-01 (fn [_ a] (str a "е"))


        step-1 [
                ["ј" "й"]
                ["і" "и"]
                ["е" "э"]
                ["ԁ" "д"]
                ]

        word (reduce (fn [word [k v]]
                       (clojure.string/replace
                         word
                         (re-pattern k)
                         v)) word step-1)

	step-2 [
		["ԇԇ" "дԇ"]
		["җҗ" "ддж"]
		["щщ" "ттш"]
		["җ" "дж"]
		["щ" "тш"]
		["(ԃԃ|ԅԅ|ԉԉ|ԋԋ|ԍԍ|ԏԏ)" repl-step2-06]
	]

        word (reduce (fn [word [k v]]
                       (re-sub
                         (re-pattern k)
                         v
                         word)) word step-2)

	step-3 [
		["([бвгжкмпрфхцш])й([аоу])" repl-step3-01]
		["([бвгжкмпрфхцш])йэ" repl-step3-02]
		["([дзлнст])й([аоу])" repl-step3-01]
		["([дзлнст])йэ" repl-step3-02]
		["([ԃԅԉԋԍԏ])й([аоу])" repl-step3-05]
		["([ԃԅԉԋԍԏ])йэ" repl-step3-06]
		["([дзлнст])и" repl-step3-07]
		["([ԃԅԉԋԍԏ])и" repl-step3-08]
		["([ԃԅԉԋԍԏ])([аоу])" repl-step3-09]
		["([ԃԅԉԋԍԏ])э" repl-step3-10]
		["([ԃԅԉԋԍԏ])" repl-step3-11]
	]

        word (reduce (fn [word [k v]]
                       (re-sub
                         (re-pattern k)
                         v
                         word)) word step-3)

	step-4 [
		["чэ" "че"]
		["([ч])й([аоу])" repl-step4-02]
		["чйэ" "чье"]
		["ԇэ" "дзе"]
		["([ԇ])й([аоу])" repl-step4-04]
		["ԇйэ" "дзье"]
		["ԇ" "дз"]
	]

        word (reduce (fn [word [k v]]
                       (re-sub
                         (re-pattern k)
                         v
                         word)) word step-4)

	step-5 [
		["й([аоу])" repl-step5-01]
		["йэ" "е"]
	]

        word (reduce (fn [word [k v]]
                       (re-sub
                         (re-pattern k)
                         v
                         word)) word step-5)

	step-6 [
		["([бвгжкмпрфхцш])э" repl-step6-01]
	]

        word (reduce (fn [word [k v]]
                       (re-sub
                         (re-pattern k)
                         v
                         word)) word step-6)

        ]
    word
    )
  )

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

(defn apply-caps-mode [word wcm]
  (let [word (clojure.string/lower-case word)]
    (cond
      (empty? word)
      word

      (= wcm 1)
      (str
        (clojure.string/upper-case (str (first word)))
        (when (>= (count word) 1)
          (subs word 1)))

      (= wcm 2)
      (clojure.string/upper-case word)
      :else
      word)))

;; stuff
(defn convert-word [word f]
  (let [wcm (word-caps-mode word)
        token-to-lower (clojure.string/lower-case word)
        converted (f token-to-lower)]
    (apply-caps-mode converted wcm)))


(defn convert-string [string f]
  (let [pattern (re-pattern (str "[" komi-and-molodtsov-letters "]+"
                                      "|"
                                      "[^" komi-and-molodtsov-letters "]+"))
        tokens (re-seq pattern string)
        tokens-converted (map (fn [token]
                                (if (re-matches pattern token)
                                  (convert-word token f)
                                  token)
                                ) tokens)]
    (clojure.string/join tokens-converted)))

(defn main
  "I don't do a whole lot."
  []
  ;(println "Hello, World!")
  (run-tests 'molodtsov.core)
  (let [s "Тані ті верманныд пӧртны коми гижӧдъяс Молодцов гижанногысь ӧнія гижанногӧ да мӧдарӧ. Дерт жӧ. Дядя Вася Тётя Зина."
        
        c1 (convert-string s o2m)
        c2 (convert-string c1 m2o)]
    (println s)
    (println c1)
    (println c2)
    (println (= s c2))
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

; apply-caps-mode
(deftest test-apply-caps-mode-001
  (is (= (apply-caps-mode "" 0) "")))

(deftest test-apply-caps-mode-002
  (is (= (apply-caps-mode "" 1) "")))

(deftest test-apply-caps-mode-003
  (is (= (apply-caps-mode "" 2) "")))

(deftest test-apply-caps-mode-011
  (is (= (apply-caps-mode "абу" 0) "абу")))

(deftest test-apply-caps-mode-012
  (is (= (apply-caps-mode "абу" 1) "Абу")))

(deftest test-apply-caps-mode-013
  (is (= (apply-caps-mode "абу" 2) "АБУ")))
