(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (if (empty? a-seq) a-seq
    (reduce concat a-seq)))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [a b]
            (if (empty? a) (conj a b)
              (conj a x b))) [] a-seq))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a))
          0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq) []
    (let [head (first a-seq)
          tail (rest a-seq)]
      (reduce (fn [[mi ma] b] [(min mi b) (max ma b)]) [head head] tail))))

(defn insert [sorted-seq n]
  (apply (fn
           ([] [n])
           ([sorted-seq] (if (>= (first sorted-seq) n) (cons n sorted-seq)
                           (concat sorted-seq [n])))
           ([lt gt] (concat lt [n] gt))) (partition-by #(>= n %1) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (set (keys (filter #(odd? (second %))
                     (loop [m {}
                            a-seq a-seq]
                       (if (empty? a-seq) m
                         (recur
                           (update-in m [(first a-seq)] (fnil inc 0))
                           (rest a-seq))))))))

(defn minus
  ([x] (* -1 x))
  ([x y] (- x y)))

(defn count-params [& args]
  (count args))

(defn my-* [& more] (reduce * 1 more))

(defn pred-and
  ([] (fn [x] true))
  ([& more] (fn [x]
              (loop [preds more]
                (cond
                  (empty? preds) true
                  (not ((first preds) x)) false
                  :else (recur (rest preds)))))))

(defn my-map
  ([f] '())
  ([f & more] (loop [acc '()
                     seqs more] 
                (if (empty? (first seqs)) (reverse acc)
                  (recur
                    (cons (apply f (map first seqs)) acc)
                    (map rest seqs))))))
