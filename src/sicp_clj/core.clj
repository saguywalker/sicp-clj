(ns sicp-clj.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn fact
  ([n] (fact n 1))
  ([n acc] (if (<= n 1)
             acc
             (fact (- n 1) (* acc n)))))

