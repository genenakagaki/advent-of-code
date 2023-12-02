(ns advent-of-code.day1
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as s]))

(def letters->digit {"one" "1"
                     "two" "2"
                     "three" "3"
                     "four" "4"
                     "five" "5"
                     "six" "6"
                     "seven" "7"
                     "eight" "8"
                     "nine" "9"})

(defn lowest-index [line search-strings]
  (->> search-strings
       (map #(let [index (s/index-of line %)]
               {:string %
                :index index}))
       (filter #(some? (:index %)))
       (sort-by :index <)
       first))

(defn lowest-index-letters [line]
  (lowest-index line (keys letters->digit)))

(defn lowest-index-digit [line]
  (lowest-index line (vals letters->digit)))

(defn highest-index [line search-strings]
  (->> search-strings
       (map #(let [index (s/last-index-of line %)]
               {:string %
                :index index}))
       (filter #(some? (:index %)))
       (sort-by :index >)
       first))

(defn highest-index-letters [line]
  (highest-index line (keys letters->digit)))

(defn highest-index-digit [line]
  (highest-index line (vals letters->digit)))

(comment 
  (lowest-index-letters "eightwothree")
  (lowest-index-digit "eightwothree")
  (highest-index-letters "pqr3stu8vwx")
  (highest-index-digit "pqr3stu8vwx")
  (highest-index-letters "eightwothree"))

(defn first-letter-replaced [line]
  (let [first-letters (lowest-index-letters line)
        first-digit (lowest-index-digit line)]
    (cond
      (nil? first-letters)
      line

      (and (some? first-digit)
           (< (:index first-digit) (:index first-letters)))
      line

      :else
      (let [replacement (get letters->digit (:string first-letters))]
        (s/replace-first line (:string first-letters) replacement)))))

(defn last-letter-replaced [line]
  (let [last-letters (highest-index-letters line)
        last-digit (highest-index-digit line)]
    (cond
      (nil? last-letters)
      line

      (and (some? last-digit)
           (< (:index last-letters) (:index last-digit)))
      line

      :else
      (let [digit (:string last-letters)
            index (:index last-letters)
            replacement (get letters->digit digit)]
        (str (subs line 0 index)
             (s/replace-first (subs line index) digit replacement))))))

(comment 
  (first-letter-replaced "1eight2wothree")
  (last-letter-replaced "pqr3stu8vtwo2wx")
  (last-letter-replaced "eightwothree"))

(defn first-and-last-digit [calibration-document-line]
  (let [digits (re-seq #"\d" calibration-document-line)]
    (-> (str (first digits) (last digits))
        (parse-long))))

(defn calibration-v1 [calibration-document]
  (->> calibration-document
       s/split-lines
       (map first-and-last-digit)
       (apply +)))

(defn calibration-v2 [calibration-document]
  (->> calibration-document
       s/split-lines
       (map first-letter-replaced)
       (map last-letter-replaced)
       (map first-and-last-digit)
       (apply +)))

(comment 
  (let [calibration-document
        "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"]
    (calibration-v2 calibration-document))

  (let [calibration-document (slurp (io/resource "day1.txt"))]
    (calibration-v2 calibration-document)))


