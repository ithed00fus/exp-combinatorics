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

;;define airport as a record (class implementation) with fields - id, connections, infection-state and time
(defrecord airport [airport-id connections infection-state t])


(defn parse
  [string]
  (map #(s/split % #",")
       (s/split string #"\n")))

(defn numberised [s]
     (into [] (map read-string (reduce into  (parse s)))))

(defn routes [coll]
     (for [s (partition 2 coll)] (into [] s)))

;;lazy-sequence of the id pair - denoting a connection
(def edges (routes (numberised (slurp filename))))


(defn empty-graph [n]
  (vec (repeat n #{})))

;;directed edge
(defn add-directed-edge [g n1 n2]
  (let [n1-neighbours (g n1)]
    (assoc g n1 (conj n1-neighbours n2))))


;;undirected-edge
(defn add-edge [g [n1 n2]]
  (-> g (add-directed-edge n1 n2)
      (add-directed-edge n2 n1)))
   
;;creates a graph
(defn graph [n] 
  (reduce add-edge (empty-graph n)
          (into [] edges)))

;;airport-graph - persistentVector of sets
(def airport-graph (graph size))
      
;;An overloaded function that creates a node object. Sets default field values when only airport-id is given
(defn airport-new
  ([airport-id connections infection-state time] (airport. airport-id (into #{} connections) infection-state time))
  ([airport-id connections infection-state] (airport-new airport-id connections infection-state 0))
  ([airport-id connections] (airport-new airport-id connections :S 0))
  ([airport-id] (airport-new airport-id #{} :S 0)))

;;create an airport node object using the -arity overloaded function airport-new
(defn airport-node [airport-id]
  ;;takes in an id of the airport and calls airport-new
  (airport-new airport-id (airport-graph airport-id)))

;; build a hash-map with every "airport" (key) filled in with all the data in values
(defn hash-of-airport [airport-id] (into #{} (into [] (hash-map airport-id (airport-node airport-id)))))

;;build a hash-map of the airport-network
(def airport-network (into {} (map hash-of-airport (range size))))

;;degree of the airport
(defn degree
  [x]
  (count (:connections (airport-node x)))) 

(defn set-field-airport ;;123
  [airport-id field new-value]
  (assoc-in (airport-node airport-id) [field] new-value))

(defn set-field-in-the-network
  [airport-network airport-id field new-value]
  (assoc-in airport-network (airport-node airport-id) (set-field-airport airport-node field new-value)))

(defn set-infection-state-airport
  [])
