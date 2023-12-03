(ns advent-of-code.day2-1
  (:require
   [clojure.java.io :as io]
   [clojure.io]
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
  (let [[game-id s-cube-subsets] (->> (re-find #"Game (\d): (.*$)" s-game)
                                      rest)
        cube-subsets (->> (s/split s-cube-subsets #"; ")
                          (map cube-subset))]
    {:game-id (parse-long game-id)
     :cube-subsets cube-subsets}))

(defn cube-subset-possible? [cube-subset cube-set]
  (->> cube-subset
       (every? #(let [[color count] %]
                  (<= count (get cube-set color))))))

(defn game-possible? [game cube-set]
  (->> (:cube-subsets game)
       (every? #(cube-subset-possible? % cube-set))))

(comment
  (cube "1 blue")
  (cube-subset "1 blue, 1 red")
  (pprint (cube-subset "1 blue, 1 red"))
  (game "Game 1: 1 blue, 1 red; 10 red; 8 red, 1 blue, 1 green; 1 green, 5 blue")
  (cube-subset-possible?
   {:blue 13 :red 1}
   {:blue 14 :red 1 :green 1})
  (game-possible?
   (game "Game 1: 1 blue, 1 red; 10 red; 8 red, 1 blue, 1 green; 1 green, 5 blue")
   {:blue 14 :red 10 :green 1})
  )

(defn possible-games [games cube-set]
  (->> (s/split-lines games)
       (map game)
       (filter #(game-possible? % cube-set))
       (map :game-id)))

(comment 

  (let [games "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
        cube-set {:red 12
                  :green 13
                  :blue 14}]
    (->> (possible-games games cube-set)
         (apply +)))

  (let [games (slurp (io/resource "day2.txt"))
        cube-set {:red 12
                  :green 13
                  :blue 14}]
    (->> (s/split-lines games)
         (map game)
         (filter #(game-possible? % cube-set))
         (map :game-id))
    #_(->> (possible-games games cube-set)
           (apply +))))
