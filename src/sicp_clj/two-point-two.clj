(ns sicp-clj.two-point-two)

(defn append
  [list1 list2] (if (empty? list1)
                  list2
                  (cons (first list1) 
                        (append (rest list1) list2))))

(defn last-pair
  [xs] (let [next-list (rest xs)]
         (if (empty? next-list)
           (first xs)
           (last-pair next-list))))

(defn my-reverse
  ([xs] (my-reverse xs nil))
  ([xs acc] (if (empty? xs)
              acc
              (my-reverse (rest xs) (cons (first xs) acc)))))


(defn deep-reverse
  ([xs] (deep-reverse xs nil))
  ([xs acc] (if (empty? xs)
              acc
              (let [x (first xs)]
               (deep-reverse (rest xs) 
                             (cons (if (list? x)
                                     (deep-reverse x)
                                     x) 
                                   acc))))))

(defn fringe
  [tree] (if (empty? tree)
           nil
           (let [current-tree (first tree)
                 next-tree (rest tree)]
             (if (list? current-tree)
               (let [subtree (rest current-tree)]
                 (cons (first current-tree)
                       (if (empty? subtree)
                         (fringe next-tree)
                         (fringe (cons subtree next-tree)))))
               (cons current-tree
                     (fringe next-tree))))))

(defn same-parity
  [x & xs] (let [parity (even? x)]
             (filter #(= (even? %) parity) xs)))

(defn square-list1
  [xs] (map (fn [x] (* x x)) xs))

(defn square-list2
  [xs] (if (empty? xs)
         nil
         (cons (* (first xs) (first xs)) (square-list2 (rest xs)))))

(defn count-leaves
  [xs] (cond
         (not (list? xs)) 1
         (empty? xs) 0
         :else (+ (count-leaves (first xs))
                  (count-leaves (rest xs)))))

;; test

(last-pair (list 1 3 5))
(my-reverse (list 1 3 5))
(same-parity 1 2 3 4 5)
(same-parity 2 3 4 5 6)
(square-list1 (list 1 2 3 4))
(square-list2 (list 1 2 3 4))
(def x (list 1 2 3))
(def y (list 4 5 6))
