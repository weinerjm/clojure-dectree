(ns clojure-dectree.core
  (:gen-class))


;; debugging let statement
;; taken from https://gist.github.com/jhsu/10920792785a4b7519f4
(defmacro dlet [bindings & body]
  `(let [~@(mapcat (fn [[n v]]
                       (if (or (vector? n) (map? n))
                           [n v]
                         [n v '_ `(println (name '~n) ":" ~v)]))
                   (partition 2 bindings))]
     ~@body))

(defn proportion 
  "Given a group and a class label, return the fraction
  of the group that has that label"
  [group class-value]
  (let [classes (map last group)
        group-size (count group)
        class-counts (frequencies classes)]
    (if (> group-size 0)
      (/ (class-counts class-value) group-size)
      0
      )))

;; (defn gini-index
;;   [groups class-values]
;;   (reduce + 
;;           (for [class-value class-values]
;;             (reduce + (for [group groups
;;                             :let [prop (proportion group class-value)]]
;;             (* prop (- 1 prop)))))))

(defn gini-index
  [group]
  (let [classes (set (map last group))
        proportions (map #(proportion group %) classes)
        squared-proportions (map #(* % %) proportions)]
    (- 1 (reduce + squared-proportions))))
          
(defn apply-split
  "Splits a dataset (vector of observation vectors) into two 
  groups ('left' and 'right') based on a splitting value in 
  a certain column."
  [feature-index value data]
  (let [{left true right false} 
        (group-by (fn [row] (< (get row feature-index) value)) 
                  data)]
    [left right]))

(defn find-splits
  "Evaluate all splits for every feature and return the best one"
  [dataset scorer]
  ;; For each feature and for each observation in that feature
  ;; apply the split and calculate the Gini index
  ;; TODO: replace the for loop with a map?
  (for [feature-index (range (dec (count (first dataset))))
        row (map #(vec (drop-last %)) dataset)
        :let [data (map #(vec (drop-last %)) dataset)
              value (get row feature-index)
              groups (apply-split feature-index value dataset)
              scores (map #(scorer %) groups)]]
    {:index feature-index 
     :score (reduce + scores) 
     :groups groups :value value}))

(defn find-best-split
  "Given a dataset, find the split that minimizes the metric
  (Gini index by default)"
  ([dataset] (first (sort-by :score (find-splits dataset gini-index))))
  ([dataset scorer] (first (sort-by :score (find-splits dataset scorer))))) 


(defn terminal-node
  "Returns a class label by majority vote given a group"
  [group]
  (let [class-labels (map last group)]
    (vector (ffirst (sort-by val > (frequencies class-labels))))))

;; TODO: This is ugly! Need to clean up the logic. 
;; Does that mean changing the data structure?
(defn split
  "Build a decision tree with a specified maximum depth and
  minimum group size"
  [node max-depth min-size depth]
  (let [[left right] (:groups node)]
    (cond 
      (and (empty? left) (empty? right)) (assoc node :children [(terminal-node (conj left right)) (terminal-node (conj left right))])
      (>= depth max-depth) (assoc node :children [(terminal-node left)                                                    (terminal-node right)])
      (<= (count left) min-size) (assoc node :children [(terminal-node left) 
                                                        (split (find-best-split right) max-depth min-size (inc depth))])
      (<= (count right) min-size) (assoc node :children [(split (find-best-split right) max-depth min-size (inc depth)) 
                                                         (terminal-node right)])
      :else (assoc node :children [(split (find-best-split left) max-depth min-size (inc depth)) 
                                   (split (find-best-split right) max-depth min-size (inc depth))]))))

(defn splits-list
  [tree]
  (dlet [[left right] (:children tree)]
       (cond 
         (map? tree) [{:value (:value tree) 
                       :score (:score tree)
                       :index (:index tree)} 
                      [(splits-list left) (splits-list right)]]
         :else tree
)))

(defn predict
  "Given a decision tree and row, predict which class the 
  observation should belong to"
  [tree row]
  (let [[node children] tree
        [left right] children]
    (if (map? node)
      (if (< (row (:index node)) (:value node))
        (predict left row)
        (predict right row))
      node)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
