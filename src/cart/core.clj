(ns cart.core)

;; Is it equal to one?
(defn one?
  "Returns true if num is one, else false"
  [x]
  (= x 1))


;; Gini impurity
(defn gini
  "Gini impurity. "
  ([p] (* 2 p (- 1 p)))
  ([p & ps]
   {:pre [(one? (reduce + p ps))]}
   (reduce (fn [acc p] (+ acc (* p (- 1 p)))) 0 ps)))


;; Average
(defn avg
  "Compute average of list values"
  [xs]
  (let [n (count xs)
        x (reduce + xs)]
    (/ x n)))


;; Find split that minimizes gini impurity 
(defn find-split
  "Fins split that minimize gini-impurity. If min-idx is -1 it means no split."
  [xs]
  (loop [idx (- (count xs) 1)                           ; Start splitting on n-1
         min-gini (gini (avg xs))                       ; Gini on full set
         min-idx -1]                                    ; -1 means no split
    (let [[p1 p2] (map avg (split-at idx xs))           ; Get shares in each set
          g (+ (gini p1) (gini p2))]                    ; Total gini resulting from split
      ; (println (str "i: " idx ", g: " (double g)))      ; debug output
      (if (= idx 1)                                     ; Stop at idx 1 
        {:min-gini min-gini, :min-idx min-idx}          ; return min
        (recur (dec idx)                                ; reduce idx by 1
               (min min-gini g)                         ; find minimum gini
               (if (> min-gini g) idx min-idx))))))     ; update min-idx if smaller


