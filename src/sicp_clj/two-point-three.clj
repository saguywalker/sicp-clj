(ns sicp-clj.two-point-tree)

(defn memq
  [item x] (cond
             (or (nil? x) (empty? x)) false
             (= item (first x)) x
             :else (memq item (rest x))))

(defn equal?
  [xs ys] (cond
            (and (empty? xs) (empty? ys)) true
            (or (empty? xs) (empty? ys)) false
            (not= (first xs) (first ys)) false
            :else (equal? (rest xs) (rest ys))))

;; differentiation

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2] (and (variable? v1) (variable? v2) (= v1 v2)))

(defn =number? [exp n] (and (number? exp) (= exp n)))

(defn make-sum [a1 a2]
  (cond
    (=number? a1 0) a2
    (=number? a2 0) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond
    (or (=number? m1 0) (=number? m2 0)) 0
    (=number? m1 1) m2
    (=number? m2 1) m1
    (and (number? m1) (number? m2)) (* m1 m2)
    :else (list '* m1 m2)))

(defn sum? [x] (and (list? x) (= (first x) '+)))
(defn addend [s] (first (rest s)))
(defn augend [s] (first (rest (rest s))))

(defn product? [x] (and (list? x) (= (first x) '*)))
(defn multiplier [p] (first (rest p)))
(defn multiplicand [p] (first (rest (rest p))))

(defn deriv [exp v]
  (cond
    (number? exp) 0
    (variable? exp) (if (same-variable? exp v) 1 0)
    (sum? exp) (make-sum (deriv (addend exp) v)
                         (deriv (augend exp) v))
    (product? exp) (make-sum (make-product (multiplier exp) 
                                           (deriv (multiplicand exp) v))
                             (make-product (deriv (multiplier exp) v)
                                           (multiplicand exp)))
    :else (println "unknown expression type - DERIV" exp)))
