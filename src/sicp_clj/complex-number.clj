(ns sicp-clj.complex-number)

(defn square [n] (* n n))

(defn attach-tag [type-tag contents] {:type-tag type-tag :contents contents})

(defn type-tag [datum]
  (if (map? datum)
    (:type-tag datum)
    (throw (Throwable. (str "Bad tagged datum -- TYPE-TAG" datum)))))

(defn contents [datum]
  (if (map? datum)
    (:contents datum)
    (throw (Throwable. (str "Bad tagged datum -- CONTENTS" datum)))))

(defn rectangular? [z] (= (type-tag z) 'rectangular))
(defn polar? [z] (= (type-tag z) 'polar))


;; rectangular selecters
(defn real-part-rectangular [z] (:real z))
(defn imag-part-rectangular [z] (:imag z))

(defn magnitude-rectangular [z]
  (Math/sqrt (+ (square (real-part-rectangular z))
                (square (imag-part-rectangular z)))))

(defn angle-rectangular [z] (Math/atan2 (imag-part-rectangular z) 
                                        (real-part-rectangular z)))


;; rectangular constructors
(defn make-from-real-imag-rectangular [x y] 
  (attach-tag 'rectangular {:real x :imag y}))

(defn make-from-mag-ang-rectangular [r a]
  (attach-tag 'rectangular 
              {:real (* r (Math/cos a)) :imag (* r (Math/sin a))}))


;; polar selectors
(defn magnitude-polar [z] (:magnitude z))
(defn angle-polar [z] (:angle z))

(defn real-part-polar [z]
  (* (magnitude-polar z) (Math/cos (angle-polar z))))

(defn imag-part-polar [z]
  (* (magnitude-polar z) (Math/sin (angle-polar z))))


;; polar constructors
(defn make-from-real-imag-polar [x y]
  (attach-tag 'polar
              {:magnitude (Math/sqrt (+ (square x) (square y)))
               :angle (Math/atan2 y x)}))

(defn make-from-mag-ang-polar [r a]
  (attach-tag 'polar
              {:magnitude r :angle a}))


;; generic

(defn real-part [z]
  (cond
    (rectangular? z) (real-part-rectangular (contents z))
    (polar? z) (real-part-polar (contents z))
    :else (throw (Throwable. (str "Unknown type -- REAL-PART" z)))))

(defn imag-part [z]
  (cond
    (rectangular? z) (imag-part-rectangular (contents z))
    (polar? z) (imag-part-polar (contents z))
    :else (throw (Throwable. (str "Unknown type -- IMAG-PART" z)))))

(defn magnitude [z]
  (cond
    (rectangular? z) (magnitude-rectangular (contents z))
    (polar? z) (magnitude-polar (contents z))
    :else (throw (Throwable. (str "Unknown type -- MAGNITUDE" z)))))

(defn angle [z]
  (cond
    (rectangular? z) (angle-rectangular (contents z))
    (polar? z) (angle-polar (contents z))
    :else (throw (Throwable. (str "Unknown type -- ANGLE" z)))))

(defn make-from-real-imag [x y] (make-from-real-imag-rectangular x y))
(defn make-from-mag-ang [r a] (make-from-mag-ang-polar r a))


;; + - * /
(defn add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defn sub-complex [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z2) (imag-part z2))))

(defn mul-complex [z1 z2]
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(defn div-complex [z1 z2]
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


