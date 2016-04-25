(ns zombie-apocalypse.core
  (:gen-class)
  (:require [clojure.string :as s]))

(def size 13)

(def filename "US_largest500_airportnetwork_shrunk.csv")

(def headers->keywords {"SourceIATA" :sourceid
                        "DestIATA" :destid "Airlines":routes})

(defn str->int
  [str]
  (Integer. str))

(def conversions {:sourceid str->int :destid str->int
                  :routes str->int})

(defrecord airport [airport-id connections infection-state t])


(defn parse
  [string]
  (map #(s/split % #",")
       (s/split string #"\n")))

(defn numberised [s]
     (into [] (map read-string (reduce into  (parse s)))))

(defn routes [coll]
     (for [s (partition 2 coll)] (into [] s)))

(def edges (routes (numberised (slurp filename))))


(defn empty-graph [n]
  (vec (repeat n #{})))

(defn add-directed-edge [g n1 n2]
  (let [n1-neighbours (g n1)]
    (assoc g n1 (conj n1-neighbours n2))))


(defn add-edge [g [n1 n2]]
  (-> g (add-directed-edge n1 n2)
      (add-directed-edge n2 n1)))
   

(defn graph [n] 
  (reduce add-edge (empty-graph n)
          (into [] edges)))


(def airport-graph (graph size))
      

(defn airport-new
  ([airport-id connections infection-state time] (airport. airport-id (into #{} connections) infection-state time))
  ([airport-id connections infection-state] (airport-new airport-id connections infection-state 0))
  ([airport-id connections] (airport-new airport-id connections :S 0))
  ([airport-id] (airport-new airport-id #{} :S 0)))

(defn airport-node [x]
  (airport-new x (airport-graph x)))

(def airport-network (into #{}
      (map airport-node (range size))))

(defn degree
  [x]
  (count (:connections (airport-node x)))) 

(defn set-field-airport ;;123
  [airport field new-value]
  (assoc-in airport [field] new-value))

(defn set-infection-state-airport
  [])
