(ns advent-of-code.day3-2
  (:require
   [advent-of-code.utils :refer [in? uuid]]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as s]))

(defn schematic-numbers [y line]
  (let [number-accumulator
        (fn [{:keys [from-index numbers]} n]
          (let [x (s/index-of line n from-index)
                item {:id (uuid)
                      :type :number
                      :value (parse-long n)
                      :x x
                      :y y
                      :length (count n)}]
            {:from-index (+ x (:length item) 1)
             :numbers (conj numbers item)}))]
    (->> (re-seq #"\d+" line)
         (reduce number-accumulator {:from-index 0})
         :numbers)))

(defn schematic-symbols [y line]
  (let [symbol-accumulator
        (fn [{:keys [from-index symbols]} sym]
          (let [x (s/index-of line sym from-index)
                item {:id (uuid)
                      :type :symbol
                      :value sym
                      :x x
                      :y y
                      :length (count sym)}]
            {:from-index (+ x (:length item) 1)
             :symbols (conj symbols item)}))]
    (->> (re-seq #"[^\d.]" line)
         (reduce symbol-accumulator {:from-index 0})
         :symbols)))

(comment 
  (schematic-numbers "467..114.." 0)
  (schematic-symbols "...$.*...." 0)
  )

(defn schematic-items [y line]
  (concat (schematic-numbers y line)
          (schematic-symbols y line)))

(comment 
  (schematic-items "617*......" 0)
  (schematic-items "467..114.." 0))

(defn part-number? [num sym]
  (let [min-x (dec (:x num))
        max-x (+ (:x num) (:length num))]
    (<= min-x (:x sym) max-x)))

(comment
  (part-number?
   {:id "17240b60-5284-4253-aa3c-7de128d51869",
    :type :number,
    :value 467,
    :x 1,
    :y 0,
    :length 3}
   {:id "17240b60-5284-4253-aa3c-7de128d51869",
    :type :symbol,
    :value "*",
    :x 1,
    :y 0,
    :length 3}))

(defn schematic [input]
  (let [input-lines (s/split-lines input)
        schem (->> input-lines
                   (map-indexed schematic-items)
                   flatten)
        schem-items-by-y (->> schem
                              (reduce #(let [y (:y %2)]
                                         (assoc %1 y (if (contains? %1 y)
                                                       (conj (%1 y) %2)
                                                       [%2])))
                                      {}))]
    (->> schem
         (filter #(= :symbol (:type %)))
         (map (fn [sym]
                (let [min-y (max (dec (:y sym))
                                 0)
                      max-y (min (inc (:y sym))
                                 (dec (count input-lines)))
                      nums (->> schem-items-by-y
                                    (filter #(<= min-y (first %) max-y))
                                    (map second)
                                    (flatten)
                                    (filter #(= :number (:type %))))]
                  (assoc sym :adjacent (->> nums
                                            (filter #(part-number? % sym))))))))))

(defn solution [input]
  (->> (schematic input)
       (map (fn [sym]
              (if (and (= (:value sym) "*")
                       (<= 2 (count (:adjacent sym))))
                ;; gear
                (->> (:adjacent sym)
                     (map :value)
                     (apply *))
                ;; part number
                0
                )))
       (apply +)))

(let [input
      "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."
      ]
  #_(pprint (schematic input))
  (solution input)
  )


(solution (slurp (io/resource "day3.txt")))
