(ns advent-of-code.day4
  (:require
   [advent-of-code.utils :refer [in? uuid]]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as s]))

(defn card [input-line]
  (let [[_ card-id winning-nums nums]
        (re-find #"Card\s+(\d+):\s+([\d\s]*)\s+\|\s+([\d\s]*)" input-line)]
    {:card-id (parse-long card-id)
     :winning-nums (s/split winning-nums #"\s+")
     :nums (s/split nums #"\s+")
     :copies 1}))

(card "Card 1: 41 48 83 8686 17 | 83 86  6 31 17  9 48 53")

(defn winning-num-count [card]
  (->> (:nums card)
       (filter #(in? (:winning-nums card) %))
       count))

(winning-num-count (card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))
(winning-num-count (card"Card 193: 53 40  5 39 13 12 27 57 68 45 | 67 10 87 64 22  6 77 17 20 24 78 52 19 18 99 88 66 31 65 47 11 61 90  9 92"))
(winning-num-count (card "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"))



(defn add-copy [card-by-id id-to-add]
  (let [card (get card-by-id id-to-add)
        copy-added-card (assoc card :copies (inc (:copies card)))]
    (assoc card-by-id id-to-add copy-added-card)))

(defn solution [input]
  (let [card-by-id (->> (s/split-lines input)
                        (map card)
                        (map #(assoc % :win-count (winning-num-count %)))
                        (map #(vector (:card-id %) %))
                        (sort-by #(first %) <)
                        (into {}))]
    (->> (reduce (fn [card-accumulator card-id]
                   (let [card (get card-accumulator card-id)
                         win-count (:win-count card)
                         copy-id-start (inc card-id)
                         card-id-to-copy (->> (range copy-id-start (+ copy-id-start win-count))
                                              (repeat (:copies card))
                                              flatten)
                         result (if (empty? card-id-to-copy)
                                  card-accumulator
                                  (->> (reduce add-copy card-accumulator card-id-to-copy)))]
                     result))
                 card-by-id
                 (sort < (keys card-by-id)))
         (map second)
         (map :copies)
         (apply +))))

(let [input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]
  (pprint (solution input)))

(solution (slurp (io/resource "day4.txt")))
