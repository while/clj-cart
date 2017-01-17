;;;; CART Algorithm
(ns cart.core)

;; Is it equal to one?
(defn one?
  "Returns true if num is one, else false"
  [x]
  (== x 1))


;; Gini impurity
(defn gini
  "Gini impurity. "
  ([p] (* 2 p (- 1 p)))
  ([p & ps]
   {:pre [(one? (reduce + p ps))]}
   (reduce (fn [acc p] (+ acc (* p (- 1 p)))) 0 (cons p ps))))


;; Average
(defn avg
  "Compute average of vector values"
  [xs]
  (/ (apply + xs)
     (count xs)))


;; Find split that minimizes gini impurity 
(defn find-split
  "Fins split that minimize gini-impurity. If idx is -1 it means no split."
  [xs cp]
  (let [parent-gini (gini (avg xs))]
  (loop [idx (dec (count xs))                           ; Start splitting on n-1
         min-gini parent-gini                           ; Gini on parent set
         min-idx -1]                                    ; -1 means no split
    #_(println (str "i: " idx                           ; DEBUG
                  ", min-idx: " min-idx                 ; DEBUG
                  ", min-g: " (double min-gini)))       ; DEBUG
    (if (or (zero? min-gini)
            (zero? idx))
      {:gini min-gini, :idx min-idx}                    ; Return perfect split
      (let [[p1 p2] (map avg (split-at idx xs))         ; Get shares in each set
            g (/ (+ (gini p1) (gini p2)) 2)]            ; Gini resulting from split
        #_(println (str "i: " idx ", g: " (double g)))  ; DEBUG
        (if (> (- 1 cp) (/ g parent-gini))              ; update min- vals if improvement > cp
          (recur (dec idx)                              ; reduce idx by 1
                 (min min-gini g)                       ; pass on minimum gini
                 (if (< g min-gini)                     ; pass on split idx
                     idx min-idx))
          (recur (dec idx) min-gini min-idx)))))))


;; Sample without replacement (Algorithm R)
(defn sample
  [x k]
  {:pre [(<= k (count x))]}
  (let [S (vec x)
        n (count x)]
    (loop [i k
           R (vec (take k x))
           j (rand-int (inc k))]
      #_(println (str "i=" i " j=" j " S=" S " x[i]=" (get S i)))
      (if (= i n)
        R
        (if (< j k)
          (recur (inc i) (assoc R j (get S i)) (rand-int (+ i 2)))
          (recur (inc i) R (rand-int (+ i 2))))))))


;; Compute optimal split points for the choosen variables 
(defn compute-splits
  [data vars cp]
  #_(println (str "Computing splits for " vars))          ; DEBUG
  (map #(conj (find-split (map :y (sort-by % data)) cp)
              [:var %])
       vars))


;; Train a decision tree recursively using CART
(defn cart
  "Train a decisiontree using CART on provided dataset."
  ([data] (cart data {}))
  ([data opts]
   (let [vars (disj (set (keys (first data))) :y)      ; get vars which are not target
         mtry (:mtry opts (count vars))                ; Nbr of vars to consider in splits
         minsplit (:minsplit opts 1)                   ; Min size of leafs
         cp (:cp opts 1e-6)                            ; Complexity param
         splits (compute-splits data (sample vars mtry) cp)
         optimal (apply min-key :gini splits)
         split-dat (split-at (:idx optimal) (sort-by (:var optimal) data))]
     #_(println splits)                                ; DEBUG
     (if (or (= (:idx optimal) -1)                         ; If no optimal split exists or
             (<= (count data) minsplit))
       (avg (map :y data))
       {:var (:var optimal)
        :split ((:var optimal) (first (second split-dat)))
        :left (cart (first split-dat) opts)
        :right (cart (second split-dat) opts)}))))


;; Read data from CSV file and parse into vector of maps. This is still a bit
;; predefined for this specific dataset.
(defn load-data []
  (rest (map (fn [line] 
               (into {} 
                     (map #(vector %1 (read-string %2))
                          [:x1 :x2 :y] (clojure.string/split line #",")))) 
             (clojure.string/split (slurp "resources/data-2dnorm-100.csv") #"\n"))))
