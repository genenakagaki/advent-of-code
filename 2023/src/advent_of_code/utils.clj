(ns advent-of-code.utils)

(defn in? 
  "true if coll contains elm"
  [coll elm]  
  (some #(= elm %) coll))

(defn uuid []
  (str (java.util.UUID/randomUUID)))

