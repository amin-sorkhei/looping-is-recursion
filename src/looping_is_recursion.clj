(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base expr]
                 (if (zero? expr)
                   acc
                   (recur (* acc base) base (dec expr))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [len a-seq]
                 (cond
                  (zero? len)
                   nil
                  (= 1 len)
                  (first a-seq)
                  :else
                   (recur (dec len) (rest a-seq))))]
    (helper (count a-seq) a-seq)))
(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
   true
   (or (empty? a-seq) (empty? b-seq))
   false
  (= (first a-seq) (first b-seq))
    (recur (rest a-seq) (rest b-seq))
  :else
  false
  ))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         n (count a-seq)]
    (cond
     (zero? n)
     nil
     (true? (pred (get a-seq  acc)))
     acc
     :else
     (recur (inc acc) (dec n))
     )
    ))

(defn avg [a-seq]
  (loop [total 0
         i 0
         n (count a-seq)
         ]
    (cond
    (empty? a-seq)
    0
    (zero? n)
    (/ total (count a-seq))
    :else
    (recur (+ total (get a-seq i)) (inc i) (dec n) )
    )))

(defn parity [a-seq]
  (loop [i 0
         n (count a-seq)
         b-set #{}]
    (cond
     (zero? n)
     b-set

     (contains? b-set (get a-seq i))
     (recur (inc i) (dec n) (disj b-set (get a-seq i)))

     :else
     (recur (inc i) (dec n) (conj b-set (get a-seq i)))
     )
    ))

(defn fast-fibo [n]
  (loop [i 2
         second 1
         first 0
         ]
    (cond
    (= n 0)
     0
    (= n 1)
     1
    (= i n)
     (+ first second)
     :else
     (recur (inc i) (+ first second) second)
    )
    ))

(defn cut-at-repetition [a-seq]
  (loop [outp []
         sq a-seq]
    (cond
      (empty? sq)
        outp
      (some #(= (first sq) %) outp)
        outp
      :else
        (recur (conj outp (first sq)) (rest sq)))))

