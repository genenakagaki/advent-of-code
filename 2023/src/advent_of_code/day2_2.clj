(ns advent-of-code.day2-2
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as s]))

(defn cube [s-cube]
  (let [[count color] (->> s-cube
                           (re-find #"(\d+) (.*)")
                           rest)]
    {(keyword color) (parse-long count)}))

(defn cube-subset [s-subset]
  (->> (s/split s-subset #", ")
       (map cube)
       (apply merge)))

(defn game [s-game]
  (let [[game-id s-cube-subsets] (->> (re-find #"Game (\d+): (.*$)" s-game)
                                      rest)
        cube-subsets (->> (s/split s-cube-subsets #"; ")
                          (map cube-subset))]
    {:game-id (parse-long game-id)
     :cube-subsets cube-subsets}))

(defn cube-subset-min-cube-set [cube-set cube-subset]
  (->> cube-subset
       (filter #(let [[color count] %]
                  (< (get cube-set color) count)))
       (reduce #(assoc %1 (first %2) (second %2)) cube-set)))

(defn game-min-cube-set [game]
  (->> (:cube-subsets game)
       (reduce cube-subset-min-cube-set {:red 0
                                         :green 0
                                         :blue 0})))

(defn cube-set-power [cube-set]
  (->> cube-set
       (map second)
       (apply *)))

(comment
  (cube "1 blue")
  (cube-subset "1 blue, 1 red")
  (pprint (cube-subset "1 blue, 1 red"))
  (game "Game 1: 1 blue, 1 red; 10 red; 8 red, 1 blue, 1 green; 1 green, 5 blue")
  (cube-subset-min-cube-set {:red 8 :green 5 :blue 0}
                            (cube-subset "1 blue, 12 red"))
  (->> (game "Game 1: 1 blue, 1 red; 10 red; 8 red, 1 blue, 1 green; 1 green, 5 blue")
       game-min-cube-set
       cube-set-power)
  (game-min-cube-set (game "Game 1: 1 blue, 1 red; 10 red; 8 red, 1 blue, 1 green; 1 green, 5 blue"))
  )

(defn total-cube-set-power [games]
  (->> (s/split-lines games)
       (map game)
       (map game-min-cube-set)
       (map cube-set-power)
       (apply +)))

(comment 

  (let [games "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]
    (->> (total-cube-set-power games)))

  (let [games (slurp (io/resource "day2.txt"))]
    (total-cube-set-power games)
    )
  (->> (game "Game 1: 1 blue, 1 red; 10 red; 8 red, 1 blue, 1 green; 1 green, 5 blue")
       game-min-cube-set))
