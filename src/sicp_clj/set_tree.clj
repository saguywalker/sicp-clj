(ns sicp-clj.set-tree)

(defn entry [tree] (first tree))
(defn left-branch [tree] (first (rest tree)))
(defn right-branch [tree] (first (rest (rest tree))))
(defn make-tree [entry left right] (list entry left right))

(defn element-of-set? [x s]
  (cond
    (empty? s) false
    (= x (entry s)) true
    (< x (entry s)) (element-of-set? x (left-branch s))
    (> x (entry s)) (element-of-set? x (right-branch s))))

(defn adjoin-set [x s]
  (cond
    (empty? s) (make-tree x '() '())
    (= x (entry s)) set
    (< x (entry s)) (make-tree (entry s) 
                               (adjoin-set x (left-branch s))
                               (right-branch s))
    (> x (entry s)) (make-tree (entry s)
                               (left-branch s)
                               (adjoin-set x (right-branch s)))))

(defn tree->list 
  ([tree] (tree->list tree '()))
  ([tree result]
    (if (empty? tree)
      result
      (tree->list (left-branch tree)
                    (cons (entry tree)
                          (tree->list (right-branch tree) result))))))

(defn partial-tree [elements n]
  (if (= n 0)
    (cons '() elements)
    (let [left-size (quot (- n 1) 2)
          left-result (partial-tree elements left-size)
          left-tree (first left-result)
          non-left-elements (rest left-result)
          right-size (- n (+ left-size 1))
          this-entry (first non-left-elements)
          right-result (partial-tree (rest non-left-elements)
                                     right-size)
          right-tree (first right-result)
          remaining-elements (rest right-result)]
      (cons (make-tree this-entry left-tree right-tree)
            remaining-elements))))

(defn ^:dynamic list->tree [elements]
  (first (partial-tree elements (count elements)))) ;; O(n) for counting








