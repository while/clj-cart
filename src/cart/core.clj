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
  "Compute average of list values"
  [xs]
  (let [n (count xs)
        x (reduce + xs)]
    (/ x n)))


;; Find split that minimizes gini impurity 
(defn find-split
  "Fins split that minimize gini-impurity. If idx is -1 it means no split."
  [xs]
  (loop [idx (- (count xs) 1)                           ; Start splitting on n-1
         min-gini (gini (avg xs))                       ; Gini on full set
         min-idx -1]                                    ; -1 means no split
    (let [[p1 p2] (map avg (split-at idx xs))           ; Get shares in each set
          g (+ (gini p1) (gini p2))]                    ; Total gini resulting from split
      #_(println (str "i: " idx ", g: " (double g)))      ; debug output
      (if (= idx 1)                                     ; Stop at idx 1 
        {:gini min-gini, :idx min-idx}          ; return min
        (recur (dec idx)                                ; reduce idx by 1
               (min min-gini g)                         ; find minimum gini
               (if (> min-gini g) idx min-idx))))))     ; update min-idx if smaller


;; Sample without replacement (Algorithm R)
(defn sample
  [x k]
  {:pre [(<= k (count x))]}
  (let [S (into [] x)
        n (count x)]
    (loop [i k
           R (into [] (take k x))
           j (rand-int (inc k))]
      #_(println (str "i=" i " j=" j " S=" S " x[i]=" (get S i)))
      (if (= i n)
        R
        (if (< j k)
          (recur (inc i) (assoc R j (get S i)) (rand-int (+ i 2)))
          (recur (inc i) R (rand-int (+ i 2))))))))


;; Compute optimal split points for the choosen variables 
(defn compute-splits
  [data vars]
  (map #(conj (find-split (map :y (sort-by % data))) [:var %]) vars))


;; Train a decision tree recursively using CART
(defn cart
  "Train a decisiontree using CART on provided dataset."
  [data opts]
  (let [max-depth (:max-depth opts 5)
        mtry (:mtry opts (int (Math/sqrt (count data))))
        vars (disj (into #{} (keys (first data))) :y)
        splits (compute-splits data (sample vars mtry))
        optimal (apply min-key :gini splits)
        split-dat (split-at (:idx optimal) (sort-by (:var optimal) data))]
    (if (= (:idx optimal) -1)
      (avg (map :y data))
      {:var (:var optimal)
       :split ((:var optimal) (first (second split-dat)))
       :left (cart (first split-dat) opts)
       :right (cart (second split-dat) opts)})))

