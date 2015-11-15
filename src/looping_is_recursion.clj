(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [value a-seq]
                 (if (empty? a-seq)
                   value
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
     (and (empty? seq1) (empty? seq2)) true
     (empty? seq1) false
     (empty? seq2) false
     (not (= (first seq1) (first seq2))) false
     :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         seq a-seq]
    (cond 
     (empty? seq) nil
     (pred (first seq)) i
     :else (recur (inc i) (rest seq)))))

(defn avg [a-seq]
  (loop [sum 0
         elem 0
         seq a-seq]
    (if (empty? seq)
      (/ sum elem)
      (recur (+ sum (first seq)) (inc elem) (rest seq)))))

(defn parity [a-seq]
  (loop [in a-seq
         out #{}]
    (if (empty? in)
      out
      (if (contains? out (first in))
        (recur (rest in) (disj out (first in)))
        (recur (rest in) (conj out (first in)))))))

(defn fast-fibo [n]
  (loop [f-1 1
         f-2 -1
         num n]
    (if (zero? num)
      (+ f-1 f-2)
      (recur (+ f-1 f-2) f-1 (dec num)))))

(defn cut-at-repetition [a-seq]
  (loop [t-set #{}
         out []
         seq a-seq]
    (if (or (empty? seq) (contains? t-set (first seq)))
        out
        (recur (conj t-set (first seq))
               (conj out (first seq))
               (rest seq)))))
