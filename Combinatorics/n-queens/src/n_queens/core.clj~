(ns n-queens.core
  (:gen-class))

(defn main 
 (all_isos1 one))
 
(defn which-queens-to-add [psol m]
  (for [q (range m)
        :when (not-any? true?
              (for [i (range (count psol)) :let [queen-i (psol i)]]
                (or
                   (= queen-i q)
                   (= (- (count psol) i) (Math/abs (- q queen-i))))))]
         q))

(defn n-queens [n m]
  (cond
    (= n 1) (map vector (range m))
    :else   (for  [psol  (n-queens (dec n)  m)
                   q     (which-queens-to-add psol m)]
                  (conj (vec psol) q))))

(def one (first (n-queens 8 8)))

(defn expand [sol] 
       (map vector (range 8) sol))

;The 8 transformations that can map the chess board to itself can be achieved by - 1.orig (1 soln) 2. Reflecting along horizontal and also vertical (2 solns) 3. Rotating by 90 (1 soln) 4. Reflecting (3) along horizontal and vertical (2 solns) 5. Rotating by 180 (1 soln) 6. Rotating by 270 (1 soln).

;3 transformations by rotating and 4 transformations by reflecting (H and V)

;rotate-90 : [i j] is transformed to [j 7-i]
(defn rotate-90 [sol]
     (map (fn[x] (let [[i j] x] (vector j (- 7 i)))) sol))

;reflect-horizontal : [i j] is transformed to [7-i j]
(defn reflect-horizontal [sol]
     (map (fn[x] (let [[i j] x] (vector (- 7 i) j))) sol))

;reflect-vertical : [i j] is transformed to [i 7-j]
(defn reflect-vertical [sol]
     (map (fn[x] (let [[i j] x] (vector i (- 7 j)))) sol))

;rotate-90 : [i j] is transformed to [7-i 7-j]
(defn rotate-180 [sol]
     (map (fn[x] (let [[i j] x] (vector (- 7 i) (- 7 j)))) sol))

;rotate-90 : [i j] is transformed to [7-j i]
(defn rotate-270 [sol]
     (map (fn[x] (let [[i j] x] (vector (- 7 j) i ))) sol))

(defn collapse [exp-q] (into [] (map (fn[ij] (last ij)) exp-q)))


(defn all_isos1 [orig]
 (let [exp-o (expand orig)]
  (map collapse [(reflect-horizontal exp-o) (reflect-vertical exp-o) (rotate-90 exp-o) (reflect-horizontal (rotate-90 exp-o)) (reflect-vertical (rotate-90 exp-o)) (rotate-180 exp-o) (rotate-270 exp-o)])))

;n-queens.core> (all_isos1 one)
;([0 4 7 5 2 6 1 3] [7 3 0 2 5 1 6 4] [7 6 5 4 3 2 1 0] [7 6 5 4 3 2 1 0] [0 1 2 3 4 5 6 7] [7 3 0 2 5 1 6 4] [0 1 2 3 4 5 6 7])

;(defn any_iso1 [orig]
; (some #(= % orig) (all_isos1 orig)))
;=>true

;(count (filter #(= % true)
 ;(map any_iso (n-queens 8 8))))
;=> 4

;10-queens:

;n-queens.core> (count (n-queens 10 10))
;724

;(count (filter #(= % true)
; (map any_iso (n-queens 10 10))))
;=> 0

