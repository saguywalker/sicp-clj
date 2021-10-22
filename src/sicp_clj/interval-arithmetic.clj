(ns sicp-clj.interval-arithmetic)

(defn make-interval
  [l u] {:lower l :upper u})

(defn lower-bound [interval] (:lower interval))
(defn upper-bound [interval] (:upper interval))

(defn add-interval
  [x y] (make-interval (+ (lower-bound x) (lower-bound y))
                       (+ (upper-bound x) (upper-bound y))))

(defn mul-interval
  [x y] (let [p1 (* (lower-bound x) (lower-bound y))
              p2 (* (lower-bound x) (upper-bound y))
              p3 (* (upper-bound x) (lower-bound y))
              p4 (* (upper-bound x) (upper-bound y))]
          (make-interval (min p1 p2 p3 p4)
                         (max p1 p2 p3 p4))))

(defn div-interval
  [x y] (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y)))))

(defn sub-interval
  [x y] (make-interval (- (lower-bound x) (upper-bound y))
                       (- (upper-bound x) (lower-bound y))))

(defn width
  [interval] (/ (- (upper-bound interval) (lower-bound interval)) 2))

(defn center
  [i] (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn make-center-width
  [c w] (make-interval (- c w) (+ c w)))

(defn make-center-percent
  [c p] (let [ratio (* c (/ p 100.0))]
          (make-interval (- c ratio) (+ c ratio))))
