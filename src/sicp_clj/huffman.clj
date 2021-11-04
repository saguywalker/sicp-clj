(ns sicp-clj.huffman)

(defn make-leaf [sym weight] (list 'leaf sym weight))
(defn leaf? [object] (= (first object) 'leaf))
(defn symbol-leaf [x] (first (rest x)))
(defn weight-leaf [x] (first (rest (rest x))))

(defn left-branch [tree] (first tree))
(defn right-branch [tree] (first (rest tree)))

(defn element-of-set? [x s]
  (cond
    (empty? s) false
    (= x (first s)) true
    :else (element-of-set? x (rest s))))

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

(defn adjoin-set [x s]
  (cond
    (empty? s) (list x)
    (< (weight x) (weight (first s))) (cons x s)
    :else (cons (first s)
                (adjoin-set x (rest s)))))

(defn make-leaf-set [pairs]
  (if (empty? pairs)
    '()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair)
                             (first (rest pair)))
                  (make-leaf-set (rest pairs))))))

(defn encode-symbol [symb tree]
  (if (leaf? tree)
    (if (= symb (symbol-leaf tree))
      '()
      (throw (Throwable. (str "unknown symbol " symb " for tree " tree))))
    (let [left (left-branch tree)]
      (if (element-of-set? symb (symbols left))
        (cons 0 (encode-symbol symb left))
        (cons 1 (encode-symbol symb (right-branch tree)))))))

(defn encode [message tree]
  (if (empty? message)
    '()
    (concat (encode-symbol (first message) tree)
            (encode (rest message) tree))))

(defn successive-merge [trees]
  (let [lower-weight-tree (first trees)
        higher-weight-trees (rest trees)]
    (if (empty? higher-weight-trees)
      lower-weight-tree
      (successive-merge (adjoin-set (make-code-tree lower-weight-tree
                                                    (first higher-weight-trees))
                                    (rest higher-weight-trees))))))

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))

(def sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
