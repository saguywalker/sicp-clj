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

(defn count-leaves-2
  [xs] (reduce + 
               0 
               (map (fn [x] (if (list? x)
                              (count-leaves-2 x)
                              1)) 
                    xs)))

(defn square-tree
  [tree] (map (fn [x] (if (list? x) (square-tree x) (* x x))) tree))

(defn tree-map
  [f tree] (map (fn [x] (if (list? x)
                          (tree-map f x)
                          (f x))) tree))

(defn square [n] (* n n))

(defn square-tree2
  [tree] (tree-map square tree))

(defn subsets
  [s] (if (empty? s)
        (list ())
        (let [tail (subsets (rest s))
              head (first s)]
          (append tail (map #(cons head %) tail)))))

(defn my-map
  [f coll] (reduce (fn [acc x] (append acc (list (f x)))) nil coll))

(defn my-append
  [seq1 seq2] (reduce (fn [acc x] (append acc (list x))) 
                      seq1
                      seq2))
(defn my-length
  [coll] (reduce (fn [acc, _] (+ acc 1)) 0 coll))

(defn horner-eval
  [x term] (reduce (fn [acc coeff] (+ (* acc x) coeff))
                   0 
                   (my-reverse term)))

(defn reduce-n
  [op init seqs] (if (or (nil? (first seqs)) 
                         (empty? (first seqs)))
                   nil
                    (cons (my-reverse (reduce (fn [acc, x] (cons x acc)) 
                                              init 
                                              (map first seqs)))
                         (reduce-n op init (map rest seqs)))))

(defn flatmap
  [f coll] (reduce append nil (map f coll)))

;; matrix - vector

(defn dot-product
  [v w] (reduce + 0 (map * v w)))

(defn matrix-*-vector
  [m v] (map (fn [mi] (dot-product mi v))
              m))

(defn transpose
  [m] (reduce-n cons nil m))

(defn matrix-*-matrix
  [m n] (let [cols (transpose n)]
          (map #(matrix-*-vector cols %) m)))

(defn unique-pairs
  [n] (flatmap (fn [i] (map (fn [j] (list j i)) 
                        (range 1 i)))
               (range 1 (+ n 1))))

(defn unique-triple-pairs
  [n] (mapcat (fn [i] 
                 (mapcat (fn [j] 
                            (map (fn [k] (list k j i)) 
                                 (range 1 j)))
                          (range 1 i))) 
                 (range 1 (+ n 1))))

(defn valid-sum?
  [triple, n] (= n (reduce + 0 triple)))

(defn valid-unique-triple-pairs
  [n, s] (filter #(valid-sum? % s) (unique-triple-pairs n)))

;; test

(last-pair (list 1 3 5))
(my-reverse (list 1 3 5))
(same-parity 1 2 3 4 5)
(same-parity 2 3 4 5 6)
(square-list1 (list 1 2 3 4))
(square-list2 (list 1 2 3 4))
(def x (list 1 2 3))
(def y (list 4 5 6))
