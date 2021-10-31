(ns sicp-clj.huffman)

(defn make-leaf [sym weight] (list 'leaf sym weight))
(defn leaf? [object] (= (first object) 'leaf))
(defn symbol-leaf [x] (first (rest x)))
(defn weight-leaf [x] (first (rest (rest x))))

(defn left-branch [tree] (first tree))
(defn right-branch [tree] (first (rest tree)))

(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (first (rest (rest tree)))))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (first (rest (rest (rest tree))))))

(defn make-code-tree [left right]
  (list left
        right
        (concat (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn choose-branch [bit branch]
  (cond
    (= bit 0) (left-branch branch)
    (= bit 1) (right-branch branch)
    :else (throw (Throwable. "choose-branch accepts either 0 or 1 only"))))

(defn decode
  ([bits tree] (decode bits tree tree))
  ([bits current-branch tree] 
   (if (empty? bits)
     '()
     (let [next-branch (choose-branch (first bits) current-branch)]
       (if (leaf? next-branch)
         (cons (symbol-leaf next-branch)
               (decode (rest bits) tree tree))
         (decode (rest bits) next-branch tree))))))
