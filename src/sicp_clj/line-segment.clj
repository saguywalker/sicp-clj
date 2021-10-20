(ns sicp-clj.line-segment)

;; point

(defn make-point
  [x y] {:x x :y y})

(def x-point :x)
(def y-point :y)


;; line segment 

(defn make-segment
  [start end] {:start start :end end})

(def start-segment :start)
(def end-segment :end)

(defn midpoint-segment
  [segment] (make-segment (/ (+ (x-point (start-segment segment))
                                (x-point (end-segment segment))) 
                             2)
                          (/ (+ (y-point (start-segment segment))
                                (y-point (end-segment segment)))
                             2)))

;; rectangle

(defn make-rectangle
  [segment] (let [start (start-segment segment)
                  end (end-segment segment)]
              {:v1 (make-segment start 
                                 (make-point (x-point start) (y-point end)))
               :v2 (make-segment end
                                 (make-point (x-point end) (y-point start)))
               :h1 (make-segment start
                                 (make-point (x-point end) (y-point start)))
               :h2 (make-segment end
                                 (make-point (x-point start) (y-point end)))}))

(defn horizontal-length
  [rectangle] (let [horizontal (:h1 rectangle)]
                (Math/abs (- (x-point (start-segment horizontal))
                             (x-point (end-segment horizontal))))))

(defn vertical-length
  [rectangle] (let [vertical (:v1 rectangle)]
                (Math/abs (- (y-point (start-segment vertical))
                             (y-point (end-segment vertical))))))

(defn perimeter
  [rectangle] (+ (* (horizontal-length rectangle) 2) 
                 (* (vertical-length rectangle) 2)))
  
(defn area
  [rectangle] (* (horizontal-length rectangle)
                 (vertical-length rectangle)))

;; test

(make-point 2 3)
(y-point (make-point (- 1) 3))
(midpoint-segment (make-segment (make-point 3 2) (make-point 8 5)))
(def my-rec (make-rectangle (make-segment (make-point 3 2) (make-point 8 5))))
(horizontal-length my-rec)
(vertical-length my-rec)
(perimeter my-rec)
(area my-rec)
