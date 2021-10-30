(ns sicp-clj.two-point-three)

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
    (list? a2) (apply list '+ a1 a2)
    :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond
    (or (=number? m1 0) (=number? m2 0)) 0
    (=number? m1 1) m2
    (=number? m2 1) m1
    (and (number? m1) (number? m2)) (* m1 m2)
    (list? m2) (apply list '* m1 m2) 
    :else (list '* m1 m2)))

(defn make-exponentiation [b e]
  (cond
    (=number? e 0) 1
    (=number? e 1) b
    :else (list '** b e)))

(defn sum? [x] (and (list? x) (= (first x) '+)))
(defn addend [s] (first (rest s)))
(defn augend [s] (let [rest-terms (rest (rest (rest s)))
                       second-term (first (rest (rest s)))]
                   (if (empty? rest-terms)
                     second-term
                     (make-sum second-term rest-terms))))

(defn product? [x] (and (list? x) (= (first x) '*)))
(defn multiplier [p] (first (rest p)))
(defn multiplicand [p] (let [rest-terms (rest (rest (rest p)))
                       second-term (first (rest (rest p)))]
                   (if (empty? rest-terms)
                     second-term
                     (make-product second-term rest-terms))))

(defn exponentiation? [x] (and (list? x) (= (first x) '**)))
(defn base [e] (first (rest e)))
(defn exponent [e] (first (rest (rest e))))

(defn ^:dynamic deriv [exp v]
  (cond
    (number? exp) 0
    (variable? exp) (if (same-variable? exp v) 1 0)
    (sum? exp) (make-sum (deriv (addend exp) v)
                         (deriv (augend exp) v))
    (product? exp) (make-sum (make-product (multiplier exp) 
                                           (deriv (multiplicand exp) v))
                             (make-product (deriv (multiplier exp) v)
                                           (multiplicand exp)))
    (exponentiation? exp) (let [b (base exp)
                                e (exponent exp)]
                            (make-product 
                              (make-product e (make-exponentiation b (- e 1)))
                              (deriv b v)))
    :else (println "unknown expression type - DERIV" exp)))

;; sets as ordered lists

(defn element-of-set? [x s]
  (cond
    (or (nil? s) (empty? s)) false
    (= x (first s)) true
    (< x (first s)) false
    :else (element-of-set? x (rest s))))

;; for unordered sets
;;(defn adjoin-set [x s]
;;  (if (element-of-set? x s)
;;    s
;;    (cons x s)))

(defn adjoin-set [x s]
  (if (empty? s)
    (list x)
    (let [head (first s)
          tail (rest s)]
      (cond
        (= x head) s
        (< x head) (cons x s)
        (> x head) (cons head (adjoin-set x tail))))))

(defn intersection-set [set1 set2]
  (if (or (empty? set1) (empty? set2))
    '()
    (let [x1 (first set1)
          x2 (first set2)]
      (cond
        (= x1 x2) (cons x1 (intersection-set (rest set1) (rest set2)))
        (< x1 x2) (intersection-set (rest set1) set2)
        (< x2 x1) (intersection-set set1 (rest set2))))))

;; for unordered sets
;;(defn intersection-set [set1 set2]
;;  (cond
;;    (or (or (nil? set1) (empty? set1)) (or (nil? set2) (empty? set2))) '()
;;    (element-of-set? (first set1) set2) (cons (first set1)
;;                                              (intersection-set (rest set1)
;;                                                                set2))
;;    :else (intersection-set (rest set1) set2)))

;; O(n)
(defn union-set [set1 set2]
  (cond
    (empty? set1) set2
    (empty? set2) set1
    (= (first set1) (first set2)) (cons (first set1)
                                        (union-set (rest set1) (rest set2)))
    (< (first set1) (first set2)) (cons (first set1)
                                        (union-set (rest set1) set2))
    :else (cons (first set2) 
                (union-set set1 (rest set2)))))

;; for unirdered sets
;;(defn union-set [set1 set2]
;;  (cond
;;    (or (nil? set1) (empty? set1)) set2
;;    (or (nil? set2) (empty? set2)) set1
;;    (element-of-set? (first set1) set2) (union-set (rest set1) set2)
;;    :else (cons (first set1) (union-set (rest set1) set2))))



