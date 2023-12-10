(ns advent-of-code.day3-1
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as s]))

(defn in? 
  "true if coll contains elm"
  [coll elm]  
  (some #(= elm %) coll))

(defn schematic-number-accumulator [accumulator s]
  (let [{:keys [schematic-line scanned-index schematic-numbers]} accumulator
        start-index (-> (subs schematic-line scanned-index)
                        (s/index-of s)
                        (+ scanned-index))
        end-index (+ start-index (dec (count s)))]
    (-> accumulator
        (assoc :schematic-numbers
               (conj schematic-numbers {:number (parse-long s)
                                        :range [start-index end-index]}))
        (assoc :scanned-index (inc end-index)))))

(defn schematic-numbers [schematic-line]
  (->> (re-seq #"\d+" schematic-line)
       (reduce schematic-number-accumulator
               {:schematic-line schematic-line
                :scanned-index 0
                :schematic-numbers []})
       :schematic-numbers))

(reduce schematic-number-accumulator
               {:schematic-line "test"
                :scanned-index 0
                :schematic-numbers []}
               nil)

(comment
  (schematic-number-accumulator
   {:schematic-line  ".......284.....*............*.....$...+.....*...377..................*.......419.............488...*.......*...................*..-....939.."
    :scanned-index 0
    :schematic-numbers []}
   "284" )

  (schematic-number-accumulator
   {:schematic-line
    ".......284.....*............*.....$...+.....*...377..................*.......419.............488...*.......*...................*..-....939..",
    :scanned-index 10,
    :schematic-numbers [{:number 284, :range [7 9]}]}
   "377")

  (schematic-numbers  ".......284.....*............*.....$...+.....*...377..................*.......419.............488...*.......*...................*..-....939.."))

(defn schematic-symbol-accumulator [accumulator s]
  (let [{:keys [schematic-line scanned-index schematic-symbols]} accumulator
        position (-> (subs schematic-line scanned-index)
                     (s/index-of s)
                     (+ scanned-index))]
    (-> accumulator
        (assoc :schematic-symbols
               (conj schematic-symbols {:symbol s
                                        :position position}))
        (assoc :scanned-index (inc position)))))

(defn schematic-symbols [schematic-line]
  (->> (re-seq #"[^\d.]+" schematic-line)
       (reduce schematic-symbol-accumulator
               {:schematic-line schematic-line
                :scanned-index 0
                :schematic-symbols []})
       :schematic-symbols))

(comment
  (pprint 
   (schematic-symbol-accumulator
    {:schematic-line  ".......284.....*............*.....$...+.....*...377..................*.......419.............488...*.......*...................*..-....939.."
     :scanned-index 0
     :schematic-symbols []}
    "*"))

  (schematic-symbol-accumulator
   {:schematic-line
    ".......284.....*............*.....$...+.....*...377..................*.......419.............488...*.......*...................*..-....939..",
    :scanned-index 16,
    :schematic-symbols [{:symbol "*", :position 15}]}
   "*")

  (schematic-symbols  ".......284.....*............*.....$...+.....*...377..................*.......419.............488...*.......*...................*..-....939.."))

(defn parsed-schematic-line [schematic-line]
  {:symbols (schematic-symbols schematic-line)
   :numbers (schematic-numbers schematic-line)})

(pprint
 (parsed-schematic-line  "617*......"))

(let [input ".......284.....*............*.....$...+.....*...377..................*.......419.............488...*.......*...................*..-....939.."]
  (->> (parsed-schematic-line input) 
       ))

(defn parsed-schematic [schematic-s]
  (->> (s/split-lines schematic-s)
       (map parsed-schematic-line)
       (into [])))

(defn possible-symbol-positions [schematic-number parsed-schematic]
  (let [r (:range schematic-number)
        range-start (dec (first r)) 
        range-end (-> (second r)
                      inc
                      inc ;; extra increment due do range end being exclusive
                      )]
    (range range-start range-end)))

(defn symbol-positions [index parsed-schematic]
  (->> [(dec index) index (inc index)]
       (filter #(<= 0 % (dec (count parsed-schematic))))
       (map #(nth parsed-schematic %))
       (map :symbols)
       flatten
       (map :position)
       (into [])))

(defn part-number? [schematic-numbers symbol-positions]
  (->> (:possible-symbol-positions schematic-numbers)
       (some #(in? symbol-positions %))))

(comment) 
(let [a (parsed-schematic "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")]
  #_(-> a
      first
      :numbers
      first
      (possible-symbol-positions a))
  (-> 
      (symbol-positions 2 a))

  )

(defn part-number [parsed-schematic index parsed-schematic-line]
  (let [symbol-positions (symbol-positions index parsed-schematic)]
    (->> (:numbers parsed-schematic-line)
         (map #(assoc % :possible-symbol-positions (possible-symbol-positions % parsed-schematic)))
         (filter #(part-number? % symbol-positions))
         (map :number))))
       

(defn solution [schematic]
  (let [parsed (parsed-schematic schematic)
        part-number-f (partial part-number parsed)]
    (->> parsed
         (map-indexed part-number-f)
         flatten
         (apply +))))

(comment 
  (solution  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
.......
...$.*....
.664.598..")

  (solution (slurp (io/resource "day3.txt"))))
