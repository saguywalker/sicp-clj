(ns sicp-clj.rational-number)

(defn gcd
  [a b] (if (= b 0)
          a
          (recur b (rem a b))))

(defn make-rat
  [n d] (let [g (Math/abs (gcd n d))
              lowest-n (/ n g)
              lowest-d (/ d g)]
          {:numerator (if (or (< lowest-n 0)
                              (< lowest-d 0))
                        (- (Math/abs lowest-n))
                        lowest-n)
           :denominator (Math/abs lowest-d)}))

(def numer :numerator)

(def denom :denominator)

(defn add-rat
  [x y] (make-rat (+ (* (numer x) (denom y)) 
                      (* (numer y) (denom x)))
                   (* (denom x) (denom y))))

(defn sub-rat
  [x y] (make-rat (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))

(defn mul-rat
  [x y] (make-rat (* (numer x) (numer y))
                  (* (denom x) (denom y))))

(defn div-rat
  [x y] (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer y))))

(defn equal-rat?
  [x y] (= (* (numer x) (denom y))
           (* (numer y) (denom x))))

;; test

(def one-half (make-rat 1 2))
(def one-third (make-rat 1 3))
(add-rat one-half one-third)
(mul-rat one-half one-third)
(add-rat one-third one-third)
(make-rat (- 1) 3)
(make-rat 1 (- 3))
(equal-rat? (make-rat 3 6) (make-rat 1 2))

