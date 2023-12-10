(ns advent-of-code.day4
  (:require
   [advent-of-code.utils :refer [in? uuid]]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as s]))

(defn card [input-line]
  (let [[_ card-id winning-nums nums]
        (re-find #"Card\s+(\d+):\s+([\d\s]*)\s+\|\s+([\d\s]*)" input-line)
        card {:card-id card-id
              :winning-nums (s/split winning-nums #"\s+")
              :nums (s/split nums #"\s+")}]
    (pprint card)
    card))

(card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

(defn points [card]
  (let [winning-num-count (->> (:nums card)
                               (filter #(in? (:winning-nums card) %))
                               count)]
    (if (<= 1 winning-num-count)
      (->> (repeat (dec winning-num-count) 2)
           (concat [1])
           (apply *))
      0)))

(points (card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))
(points (card "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"))
(points (card "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"))

(defn solution [input]
  (->> (s/split-lines input)
       (map card)
       (map points)
       (apply +)))

(let [input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]
  (solution input))

(solution (slurp (io/resource "day4.txt")))
