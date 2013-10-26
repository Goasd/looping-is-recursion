(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq)) (first a-seq) (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (= seq1 seq2) (recur (rest seq1) (rest seq2))
   :else false
   ))

(defn find-first-index [pred a-seq]
  (loop [n 0
         x a-seq]
    (cond
     (empty? x) nil
     (pred (first x)) n
     :else (recur (inc n) (rest x)))))

(defn avg [a-seq]
  (loop [n 0
         s 0
         x a-seq]
    (if (empty? x) (/ s n) (recur (inc n) (+ s (first x)) (rest x)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [n a-seq
         s #{}]
    (if (empty? n) s (recur (rest n) (toggle s (first n))))))

(defn fast-fibo [n]
  (loop [a 0
         s 1
         x n]
    (if (zero? x) a (recur s (+ a s) (dec x)))))

(defn cut-at-repetition [a-seq]
  (loop [n a-seq
         s []]
    (if (or (empty? n) (contains? (set s) (first n))) (reverse s) (recur (rest n) (cons (first n) s)))))

